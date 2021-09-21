use llvm_sys::{core::*, prelude::*};
use std::{collections::HashMap, ffi::CString};

use analyzer::{
	ast::{Analyzed, TopLevel, AST},
	ty::{Ctx, Ty},
};

use crate::llvm::block::generate_block;

/// LLVM equivalents to Ares types.
pub struct DataTypes {
	pub int: LLVMTypeRef,
	pub float: LLVMTypeRef,
	pub boolean: LLVMTypeRef,
	pub string: LLVMTypeRef,
	pub void: LLVMTypeRef,
}

/// Global `print` variants for all possible input types.
pub struct PrintFns {
	pub int: LLVMValueRef,
	pub float: LLVMValueRef,
	pub boolean: LLVMValueRef,
	pub string: LLVMValueRef,
}

/// Keeps track of all the necessary state which is
/// required in multiple places to do codegen.
/// (`LLVMContext`, `LLVMBuilder`, `DataTypes`, etc)
/// It is very similar in nature to the `Environment`
/// struct in the analyzer. (Scopes, variables, etc)
pub struct Environment<'a> {
	context: LLVMContextRef,
	builder: LLVMBuilderRef,
	datatypes: DataTypes,
	fns: HashMap<String, LLVMValueRef>,
	print_fns: PrintFns,
	string_len_fn: LLVMValueRef,
	scopes: Vec<HashMap<&'a str, LLVMValueRef>>,
	loops: Vec<LLVMBasicBlockRef>,
	current_fn: Option<LLVMValueRef>,
}

impl<'a> Environment<'a> {
	pub fn context(&self) -> LLVMContextRef {
		self.context
	}

	pub fn builder(&self) -> LLVMBuilderRef {
		self.builder
	}

	pub fn print_fns(&self) -> &PrintFns {
		&self.print_fns
	}

	pub fn string_len_fn(&self) -> LLVMValueRef {
		self.string_len_fn
	}

	pub fn datatypes(&self) -> &DataTypes {
		&self.datatypes
	}

	pub fn get_fn(&self, name: &str) -> Option<LLVMValueRef> {
		self.fns.get(name).cloned()
	}

	pub fn current_fn(&self) -> LLVMValueRef {
		self.current_fn.unwrap()
	}

	pub fn get_loop(&mut self) -> Option<&LLVMBasicBlockRef> {
		self.loops.last()
	}

	pub fn add_loop(&mut self, bb: LLVMBasicBlockRef) {
		self.loops.push(bb);
	}

	pub fn remove_loop(&mut self) {
		self.loops.pop();
	}

	pub fn add_scope(&mut self) {
		self.scopes.push(HashMap::new());
	}

	pub fn remove_scope(&mut self) {
		self.scopes.pop();
	}

	pub fn add_variable(&mut self, name: &'a str, value: LLVMValueRef) {
		let scope = self.scopes.last_mut().unwrap();
		scope.insert(name, value);
	}

	pub fn find_variable(&mut self, name: &'a str) -> Option<LLVMValueRef> {
		for scope in self.scopes.iter().rev() {
			let var = scope.get(name);
			if let Some(value) = var {
				return Some(*value);
			}
		}
		None
	}
}

/// Convert Ares types to their LLVM equivalents.
pub unsafe fn ty_to_ir(ty: &Ty, datatypes: &DataTypes) -> LLVMTypeRef {
	match ty {
		Ty::Int(_) => datatypes.int,
		Ty::Float(_) => datatypes.float,
		Ty::String(_) => datatypes.string,
		Ty::Boolean(_) => datatypes.boolean,
		Ty::Void(_) => datatypes.void,
		Ty::Never(_) => unreachable!(),
		Ty::Unknown(_, _) => unreachable!(),
		Ty::TyRef(_) => unreachable!(),
		Ty::Array(_, _) => todo!(),
		Ty::Possibility(_, _, _) => unreachable!(),
		Ty::Union(_, _) => todo!(),
		Ty::Fn(p, r, _) => LLVMFunctionType(
			ty_to_ir(r, datatypes),
			p.iter()
				.map(|ty| ty_to_ir(ty, datatypes))
				.collect::<Vec<_>>()
				.as_mut_ptr(),
			p.len() as u32,
			0,
		),
	}
}

/// Generate LLVM IR for a given AST.
pub unsafe fn generate_ir(
	context: LLVMContextRef,
	module: LLVMModuleRef,
	ctx: &Ctx,
	ast: &AST<Analyzed>,
) {
	let i32_type = LLVMInt32TypeInContext(context); // i32
	let i8_type = LLVMInt8TypeInContext(context); // i8
	let i8_ptr_type = LLVMPointerType(i8_type, 0); // *i8
	let void_type = LLVMVoidTypeInContext(context); // void

	let builder = LLVMCreateBuilderInContext(context);
	assert!(!builder.is_null());

	let datatypes = DataTypes {
		void: void_type,                         // void
		int: LLVMInt64TypeInContext(context),    // i64
		float: LLVMDoubleTypeInContext(context), // f64
		boolean: LLVMInt1TypeInContext(context), // i1
		/// String { length: i64, chars: *i8 }
		string: {
			let name = CString::new("String").unwrap();
			let struct_type = LLVMStructCreateNamed(context, name.as_ptr());
			let i64_type = LLVMInt64TypeInContext(context);
			LLVMStructSetBody(struct_type, vec![i64_type, i8_ptr_type].as_mut_ptr(), 2, 0);
			struct_type
		},
	};

	let print_fns = {
		// This is the C/C++ `printf` function.
		// This function should get linked and so calling this function
		// should print things to stdout.
		let fn_type = LLVMFunctionType(i32_type, vec![i8_ptr_type].as_mut_ptr(), 1, 1);
		assert!(!fn_type.is_null());
		let name = CString::new("printf").unwrap();
		let printf_fn = LLVMAddFunction(module, name.as_ptr(), fn_type);
		assert!(!printf_fn.is_null());
		PrintFns {
			int: {
				// Declare fn print-int(Int) -> Void
				let fn_type =
					LLVMFunctionType(datatypes.void, vec![datatypes.int].as_mut_ptr(), 1, 0);
				assert!(!fn_type.is_null());
				let name = CString::new("print-int").unwrap();
				let f = LLVMAddFunction(module, name.as_ptr(), fn_type);
				assert!(!f.is_null());

				// Create the function body.
				let name = CString::new("entry").unwrap();
				let bb = LLVMAppendBasicBlockInContext(context, f, name.as_ptr());
				assert!(!bb.is_null());
				LLVMPositionBuilderAtEnd(builder, bb);

				// Create a global "%i" string.
				let format_str = CString::new("%i\n").unwrap();
				let name = CString::new("int-fmt-str").unwrap();
				let format_str =
					LLVMBuildGlobalStringPtr(builder, format_str.as_ptr(), name.as_ptr());
				assert!(!format_str.is_null());

				// Call printf("%i", Int)
				let name = CString::new("res").unwrap();
				LLVMBuildCall(
					builder,
					printf_fn,
					vec![format_str, LLVMGetParam(f, 0)].as_mut_ptr(),
					2,
					name.as_ptr(),
				);

				// Return void.
				LLVMBuildRetVoid(builder);
				f
			},
			float: {
				// Declare fn print-float(Float) -> Void
				let fn_type =
					LLVMFunctionType(datatypes.void, vec![datatypes.float].as_mut_ptr(), 1, 0);
				assert!(!fn_type.is_null());
				let name = CString::new("print-float").unwrap();
				let f = LLVMAddFunction(module, name.as_ptr(), fn_type);
				assert!(!f.is_null());

				// Create the function body.
				let name = CString::new("entry").unwrap();
				let bb = LLVMAppendBasicBlockInContext(context, f, name.as_ptr());
				assert!(!bb.is_null());
				LLVMPositionBuilderAtEnd(builder, bb);

				// Create a global "%f" string.
				let format_str = CString::new("%f\n").unwrap();
				let name = CString::new("float-fmt-str").unwrap();
				let format_str =
					LLVMBuildGlobalStringPtr(builder, format_str.as_ptr(), name.as_ptr());
				assert!(!format_str.is_null());

				// Call printf("%f", Float)
				let name = CString::new("res").unwrap();
				LLVMBuildCall(
					builder,
					printf_fn,
					vec![format_str, LLVMGetParam(f, 0)].as_mut_ptr(),
					2,
					name.as_ptr(),
				);

				// Return void.
				LLVMBuildRetVoid(builder);
				f
			},
			boolean: {
				// Declare fn print-boolean(Boolean) -> Void
				let fn_type =
					LLVMFunctionType(datatypes.void, vec![datatypes.boolean].as_mut_ptr(), 1, 0);
				assert!(!fn_type.is_null());
				let name = CString::new("print-boolean").unwrap();
				let f = LLVMAddFunction(module, name.as_ptr(), fn_type);
				assert!(!f.is_null());

				// Create the function body.
				let name = CString::new("entry").unwrap();
				let bb = LLVMAppendBasicBlockInContext(context, f, name.as_ptr());
				assert!(!bb.is_null());
				LLVMPositionBuilderAtEnd(builder, bb);

				// if Boolean == true { then_bb } else { else_bb }
				let name = CString::new("then").unwrap();
				let then_bb = LLVMAppendBasicBlockInContext(context, f, name.as_ptr());
				let name = CString::new("else").unwrap();
				let else_bb = LLVMAppendBasicBlockInContext(context, f, name.as_ptr());
				LLVMBuildCondBr(builder, LLVMGetParam(f, 0), then_bb, else_bb);
				{
					// Create a global "true" string.
					LLVMPositionBuilderAtEnd(builder, then_bb);
					let format_str = CString::new("true\n").unwrap();
					let name = CString::new("bool-true-fmt-str").unwrap();
					let true_str =
						LLVMBuildGlobalStringPtr(builder, format_str.as_ptr(), name.as_ptr());

					// Call printf("true").
					let name = CString::new("res").unwrap();
					LLVMBuildCall(
						builder,
						printf_fn,
						vec![true_str].as_mut_ptr(),
						1,
						name.as_ptr(),
					);

					// Return void.
					LLVMBuildRetVoid(builder);
				}
				{
					// Create a global "false" string.
					LLVMPositionBuilderAtEnd(builder, else_bb);
					let format_str = CString::new("false\n").unwrap();
					let name = CString::new("bool-false-fmt_str").unwrap();
					let false_str =
						LLVMBuildGlobalStringPtr(builder, format_str.as_ptr(), name.as_ptr());

					// Call printf("false").
					let name = CString::new("res").unwrap();
					LLVMBuildCall(
						builder,
						printf_fn,
						vec![false_str].as_mut_ptr(),
						1,
						name.as_ptr(),
					);

					// Return void.
					LLVMBuildRetVoid(builder);
				}

				f
			},
			string: {
				// Declare fn print-string(String) -> Void
				let fn_type =
					LLVMFunctionType(datatypes.void, vec![datatypes.string].as_mut_ptr(), 1, 0);
				assert!(!fn_type.is_null());
				let name = CString::new("print-string").unwrap();
				let f = LLVMAddFunction(module, name.as_ptr(), fn_type);
				assert!(!f.is_null());

				// Create the function body.
				let name = CString::new("entry").unwrap();
				let bb = LLVMAppendBasicBlockInContext(context, f, name.as_ptr());
				assert!(!bb.is_null());
				LLVMPositionBuilderAtEnd(builder, bb);

				// Get the char pointer in the string struct. (String.chars)
				let string = LLVMGetParam(f, 0);
				let name = CString::new("string-contents-ptr").unwrap();
				let string =
					LLVMBuildStructGEP2(builder, datatypes.string, string, 1, name.as_ptr());
				let name = CString::new("string-contents").unwrap();
				let string = LLVMBuildLoad(builder, string, name.as_ptr());

				// Call printf(String.chars).
				let name = CString::new("res").unwrap();
				LLVMBuildCall(
					builder,
					printf_fn,
					vec![string].as_mut_ptr(),
					1,
					name.as_ptr(),
				);

				// Return void.
				LLVMBuildRetVoid(builder);
				f
			},
		}
	};

	// String#len() method implementation.
	let string_len_fn = {
		// Declare fn get-string-len(self) -> Int
		let fn_type = LLVMFunctionType(datatypes.int, vec![datatypes.string].as_mut_ptr(), 1, 0);
		assert!(!fn_type.is_null());
		let name = CString::new("get-string-len").unwrap();
		let f = LLVMAddFunction(module, name.as_ptr(), fn_type);
		assert!(!f.is_null());

		// Create the function body.
		let name = CString::new("entry").unwrap();
		let bb = LLVMAppendBasicBlockInContext(context, f, name.as_ptr());
		assert!(!bb.is_null());
		LLVMPositionBuilderAtEnd(builder, bb);

		// Get the string length field from the String struct. (String.length)
		let string = LLVMGetParam(f, 0);
		let name = CString::new("string-len-ptr").unwrap();
		let string_len = LLVMBuildStructGEP2(builder, datatypes.string, string, 0, name.as_ptr());
		let name = CString::new("string-len").unwrap();
		let string_len = LLVMBuildLoad(builder, string_len, name.as_ptr());

		// Return the Int.
		LLVMBuildRet(builder, string_len);
		f
	};

	// Generate all the function declarations ahead of time
	// to support self and forward references.
	let mut fns = HashMap::new();
	for top_level in ast {
		match top_level {
			TopLevel::Function(f) => {
				// Generate the function signature.
				let fn_type = ty_to_ir(ctx.resolve_ref(&f.ty()), &datatypes);
				assert!(!fn_type.is_null());
				// Create the LLVM function.
				let name = CString::new(f.name().as_str()).unwrap();
				let v = LLVMAddFunction(module, name.as_ptr(), fn_type);
				assert!(!v.is_null());
				// Add the function to the hashmap.
				fns.insert(f.name().clone(), v);
			}
		}
	}

	let mut env = Environment {
		context,
		builder,
		datatypes,
		fns,
		string_len_fn,
		print_fns,
		scopes: vec![],
		loops: vec![],
		current_fn: None,
	};

	for top_level in ast {
		match top_level {
			TopLevel::Function(f) => {
				// Update the environment.
				assert!(env.scopes.len() == 0);
				assert!(env.loops.len() == 0);
				let fn_ref = *env.fns.get(f.name()).unwrap();
				env.current_fn = Some(fn_ref);

				// Create the function body for the already existing LLVM function.
				let name = CString::new("entry").unwrap();
				let bb = LLVMAppendBasicBlockInContext(context, fn_ref, name.as_ptr());
				assert!(!bb.is_null());
				LLVMPositionBuilderAtEnd(builder, bb);

				// Add all the function parameters to the scope.
				env.add_scope();
				for (idx, (name, dt)) in f.parameters().iter().enumerate() {
					let cname = CString::new(name.as_str()).unwrap();
					let value = LLVMBuildAlloca(
						env.builder(),
						ty_to_ir(ctx.resolve_ref(&dt.ty()), env.datatypes()),
						cname.as_ptr(),
					);
					LLVMBuildStore(env.builder(), LLVMGetParam(fn_ref, idx as u32), value);
					env.add_variable(name.as_str(), value);
				}

				// Generate the IR for the function body.
				let res = generate_block(ctx, f.body(), &mut env);
				env.remove_scope();

				// If the function body always exits then there is no
				// need to return again.
				if !f.body().exit_status().will_exit() {
					// If the function returns a value then return it otherwise
					// return void.
					if let Some(res) = res {
						LLVMBuildRet(builder, res);
					} else {
						LLVMBuildRetVoid(builder);
					}
				}
			}
		}
	}
}
