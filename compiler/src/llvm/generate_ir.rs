use llvm_sys::{core::*, prelude::*, LLVMModule, LLVMType, LLVMValue};
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
	pub array: LLVMTypeRef,
	pub void: LLVMTypeRef,
}

/// Helper functions.
pub struct HelperFns {
	pub floor_float: LLVMValueRef,
	pub ceil_float: LLVMValueRef,
	pub round_float: LLVMValueRef,

	pub prompt_int: LLVMValueRef,
	pub prompt_float: LLVMValueRef,

	pub len_string: LLVMValueRef,
	pub len_array: LLVMValueRef,

	pub index_of_int: LLVMValueRef,
	pub index_of_float: LLVMValueRef,
	pub index_of_boolean: LLVMValueRef,
	pub index_of_string: LLVMValueRef,

	pub print_int: LLVMValueRef,
	pub print_float: LLVMValueRef,
	pub print_boolean: LLVMValueRef,
	pub print_string: LLVMValueRef,

	pub println_int: LLVMValueRef,
	pub println_float: LLVMValueRef,
	pub println_boolean: LLVMValueRef,
	pub println_string: LLVMValueRef,
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
	helper_fns: HelperFns,
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

	pub fn helper_fns(&self) -> &HelperFns {
		&self.helper_fns
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
		Ty::Array(ty, _) => match ty.as_ref() {
			Ty::Int(_) | Ty::Float(_) | Ty::String(_) | Ty::Boolean(_) => datatypes.array,
			_ => todo!(),
		},
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

unsafe fn add_function(
	module: *mut LLVMModule,
	name: &str,
	parameters: &mut [*mut LLVMType],
	return_type: *mut LLVMType,
) -> *mut LLVMValue {
	let fn_type = LLVMFunctionType(
		return_type,
		parameters.as_mut_ptr(),
		parameters.len() as u32,
		0,
	);
	assert!(!fn_type.is_null());
	let name = CString::new(name).unwrap();
	let fn_value = LLVMAddFunction(module, name.as_ptr(), fn_type);
	assert!(!fn_value.is_null());
	fn_value
}

/// Generate LLVM IR for a given AST.
pub unsafe fn generate_ir(
	context: LLVMContextRef,
	module: LLVMModuleRef,
	ctx: &Ctx,
	ast: &AST<Analyzed>,
) {
	let i8_type = LLVMInt8TypeInContext(context); // i8
	let i8_ptr_type = LLVMPointerType(i8_type, 0); // *i8
	let void_type = LLVMVoidTypeInContext(context); // void
	let void_ptr_type = LLVMPointerType(void_type, 0); // *void
	let i32_type = LLVMInt32TypeInContext(context);
	let i64_type = LLVMInt64TypeInContext(context);

	let builder = LLVMCreateBuilderInContext(context);
	assert!(!builder.is_null());

	let datatypes = DataTypes {
		void: void_type,                         // void
		int: LLVMInt64TypeInContext(context),    // i64
		float: LLVMDoubleTypeInContext(context), // f64
		boolean: LLVMInt1TypeInContext(context), // i1
		// String { length: i64, chars: *i8 }
		string: {
			let name = CString::new("String").unwrap();
			let struct_type = LLVMStructCreateNamed(context, name.as_ptr());
			LLVMStructSetBody(struct_type, vec![i64_type, i8_ptr_type].as_mut_ptr(), 2, 0);
			struct_type
		},
		// Array { length: i64, element_size: i64, data: *void }
		array: {
			let name = CString::new("Array").unwrap();
			let struct_type = LLVMStructCreateNamed(context, name.as_ptr());
			LLVMStructSetBody(
				struct_type,
				vec![i64_type, i64_type, void_ptr_type].as_mut_ptr(),
				3,
				0,
			);
			struct_type
		},
	};

	let helper_fns = HelperFns {
		floor_float: add_function(
			module,
			"floor_float",
			&mut [datatypes.float],
			datatypes.float,
		),
		ceil_float: add_function(
			module,
			"ceil_float",
			&mut [datatypes.float],
			datatypes.float,
		),
		round_float: add_function(
			module,
			"round_float",
			&mut [datatypes.float],
			datatypes.float,
		),

		prompt_int: add_function(module, "prompt_int", &mut [datatypes.string], datatypes.int),
		prompt_float: add_function(
			module,
			"prompt_float",
			&mut [datatypes.string],
			datatypes.float,
		),

		len_string: add_function(module, "len_string", &mut [datatypes.string], datatypes.int),
		len_array: add_function(module, "len_array", &mut [datatypes.array], datatypes.int),

		index_of_int: add_function(
			module,
			"index_of_int",
			&mut [datatypes.array, datatypes.int],
			datatypes.int,
		),
		index_of_float: add_function(
			module,
			"index_of_float",
			&mut [datatypes.array, datatypes.int],
			datatypes.float,
		),
		index_of_boolean: add_function(
			module,
			"index_of_boolean",
			&mut [datatypes.array, datatypes.int],
			datatypes.boolean,
		),
		index_of_string: add_function(
			module,
			"index_of_string",
			&mut [datatypes.array, datatypes.int],
			datatypes.string,
		),

		print_int: add_function(module, "print_int", &mut [datatypes.int], void_type),
		print_float: add_function(module, "print_float", &mut [datatypes.float], void_type),
		print_boolean: add_function(module, "print_boolean", &mut [datatypes.boolean], void_type),
		print_string: add_function(module, "print_string", &mut [datatypes.string], void_type),

		println_int: add_function(module, "println_int", &mut [datatypes.int], void_type),
		println_float: add_function(module, "println_float", &mut [datatypes.float], void_type),
		println_boolean: add_function(
			module,
			"println_boolean",
			&mut [datatypes.boolean],
			void_type,
		),
		println_string: add_function(module, "println_string", &mut [datatypes.string], void_type),
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
		helper_fns,
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
					if f.name() == "main" {
						LLVMBuildRet(builder, LLVMConstInt(i32_type, 0, 0));
					} else if let Some(res) = res {
						LLVMBuildRet(builder, res);
					} else {
						LLVMBuildRetVoid(builder);
					}
				}
			}
		}
	}
}
