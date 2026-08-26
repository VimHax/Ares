use llvm_sys::{core::*, prelude::*, LLVMIntPredicate, LLVMRealPredicate};
use std::ffi::CString;

use analyzer::{
	ast::{Analyzed, BinaryOperator, Expression, LiteralValue, UnaryOperator},
	ty::{Ctx, Ty},
};

use crate::llvm::generate_ir::ty_to_ir;

use super::{generate_ir::Environment, stmt::generate_stmt};

/// Generate LLVM IR for an AST `Expression<Analyzed>` node.
pub unsafe fn generate_expr<'a>(
	ctx: &Ctx,
	expr: &'a Expression<Analyzed>,
	env: &mut Environment<'a>,
) -> Option<LLVMValueRef> {
	let name = CString::new("expr").unwrap();
	match expr {
		Expression::Parenthesis(e) => generate_expr(ctx, e.expression(), env),
		Expression::Literal(e) => Some(match e.value() {
			LiteralValue::Int(v) => {
				let neg = (*v < 0) as i32;
				LLVMConstInt(env.datatypes().int, (*v).abs() as u64, neg)
			}
			LiteralValue::Float(v) => LLVMConstReal(env.datatypes().float, *v),
			LiteralValue::Boolean(v) => LLVMConstInt(env.datatypes().boolean, *v as u64, 0),
			LiteralValue::String(v) => {
				// Create a global string with the contents of this literal.
				let str_contents = {
					let contents = CString::new(v.as_str()).unwrap();
					let name = CString::new("static-str").unwrap();
					LLVMBuildGlobalString(env.builder(), contents.as_ptr(), name.as_ptr())
				};
				LLVMConstNamedStruct(
					env.datatypes().string,
					[
						LLVMConstInt(
							LLVMStructGetTypeAtIndex(env.datatypes().string, 0),
							v.len() as u64,
							0,
						),
						str_contents,
					]
					.as_mut_ptr(),
					2,
				)
			}
		}),
		Expression::Array(e) => {
			let mut elements = e
				.elements()
				.iter()
				.map(|expr| generate_expr(ctx, expr, env).expect("can't be void"))
				.collect::<Vec<_>>();

			let Ty::Array(element_ty, _) = ctx.resolve_ref(&e.ty()) else {
				panic!("must be an array");
			};
			let element_ty = ty_to_ir(element_ty, env.datatypes());

			// https://stackoverflow.com/a/30830445/10685858
			let element_pointer_ty = LLVMPointerType(element_ty, 0);
			let size = {
				let ptr = LLVMBuildGEP(
					env.builder(),
					LLVMConstNull(element_pointer_ty),
					[LLVMConstInt(LLVMInt32TypeInContext(env.context()), 1, 0)].as_mut_ptr(),
					1,
					name.as_ptr(),
				);
				LLVMConstPtrToInt(ptr, env.datatypes().int)
			};

			let arr = LLVMBuildAlloca(
				env.builder(),
				LLVMArrayType(element_ty, elements.len() as u32),
				name.as_ptr(),
			);
			let arr_value =
				LLVMConstArray(element_ty, elements.as_mut_ptr(), elements.len() as u32);
			LLVMBuildStore(env.builder(), arr_value, arr);

			Some(LLVMConstNamedStruct(
				env.datatypes().array,
				[
					LLVMConstInt(
						LLVMStructGetTypeAtIndex(env.datatypes().array, 0),
						elements.len() as u64,
						0,
					),
					size,
					arr,
				]
				.as_mut_ptr(),
				3,
			))
		}
		Expression::Variable(e) => {
			if let Some(var) = env.find_variable(e.name()) {
				if let Ty::Array(_, _) = ctx.resolve_ref(&e.ty()) {
					Some(var)
				} else {
					let name = CString::new("var").unwrap();
					// Load the value at the stack pointer of this variable.
					Some(LLVMBuildLoad(env.builder(), var, name.as_ptr()))
				}
			} else if e.name() == "print" {
				// Based on the argument return the appropriate
				// LLVM print function.
				if let Ty::Fn(args, _, _) = ctx.resolve_ref(&e.ty()) {
					Some(match args[0] {
						Ty::Int(_) => env.print_fns().int,
						Ty::Float(_) => env.print_fns().float,
						Ty::Boolean(_) => env.print_fns().boolean,
						Ty::String(_) => env.print_fns().string,
						_ => todo!(),
					})
				} else {
					todo!()
				}
			} else if let Some(f) = env.get_fn(e.name()) {
				// Return a function pointer.
				Some(f)
			} else {
				todo!()
			}
		}
		Expression::IndexOf(e) => {
			let name = CString::new("res").unwrap();
			// Generate the IR for the expression.
			let arr = generate_expr(ctx, e.expression(), env).unwrap();
			// Generate the IR for the index.
			let idx = generate_expr(ctx, e.index(), env).unwrap();

			Some(LLVMBuildCall(
				env.builder(),
				match ctx.resolve_ref(&e.ty()) {
					Ty::Int(_) => env.array_element_fns().int,
					Ty::Float(_) => env.array_element_fns().float,
					Ty::String(_) => env.array_element_fns().string,
					Ty::Boolean(_) => env.array_element_fns().boolean,
					_ => todo!(),
				},
				[arr, idx].as_mut_ptr(),
				2,
				name.as_ptr(),
			))
		}
		Expression::PropertyOf(_e) => todo!(),
		Expression::Call(e) => {
			let name = CString::new("res").unwrap();
			// Generate the IR for the expression.
			let f = generate_expr(ctx, e.expression(), env).unwrap();
			// Generate the IR to evaluate every argument.
			let mut args = e
				.arguments()
				.iter()
				.map(|x| generate_expr(ctx, x, env).unwrap())
				.collect::<Vec<_>>();
			// Call the function with the provided arguments.
			let res = LLVMBuildCall(
				env.builder(),
				f,
				args.as_mut_ptr(),
				args.len() as u32,
				name.as_ptr(),
			);
			// If the function does not return void then return
			// the value.
			if let Ty::Void(_) = ctx.resolve_ref(&e.ty()) {
				None
			} else {
				Some(res)
			}
		}
		Expression::NamedCall(e) => {
			// The String#len method.
			if e.name() == "len" {
				let name = CString::new("res").unwrap();
				// Generate the IR for the expression.
				let expr = generate_expr(ctx, e.expression(), env).unwrap();
				// The only argument for this method will be
				// the String struct in question. (self)
				let mut args = vec![expr];
				// Call the method.
				let res = LLVMBuildCall(
					env.builder(),
					env.string_len_fn(),
					args.as_mut_ptr(),
					args.len() as u32,
					name.as_ptr(),
				);
				// This method must return an Int so
				// no need to check.
				Some(res)
			} else {
				todo!();
			}
		}
		Expression::Unary(e) => match e.operator() {
			UnaryOperator::Minus => {
				let op = generate_expr(ctx, e.operand(), env).expect("can't be void");
				match ctx.resolve_ref(&e.operand().ty()) {
					Ty::Int(_) => Some(LLVMBuildNeg(env.builder(), op, name.as_ptr())),
					Ty::Float(_) => Some(LLVMBuildFNeg(env.builder(), op, name.as_ptr())),
					_ => unreachable!(),
				}
			}
			UnaryOperator::Not => Some(LLVMBuildXor(
				env.builder(),
				generate_expr(ctx, e.operand(), env).expect("can't be void"),
				LLVMConstInt(env.datatypes().boolean, 1, 0),
				name.as_ptr(),
			)),
		},
		Expression::Binary(e) => {
			// Generate the IR for both operands.
			let (op1, op2) = (
				generate_expr(ctx, &e.operands().0, env).unwrap(),
				generate_expr(ctx, &e.operands().1, env).unwrap(),
			);
			match ctx.resolve_ref(&e.operands().0.ty()) {
				Ty::Int(_) => Some(match e.operator() {
					BinaryOperator::Add => LLVMBuildAdd(env.builder(), op1, op2, name.as_ptr()),
					BinaryOperator::Subtract => {
						LLVMBuildSub(env.builder(), op1, op2, name.as_ptr())
					}
					BinaryOperator::Multiply => {
						LLVMBuildMul(env.builder(), op1, op2, name.as_ptr())
					}
					BinaryOperator::Divide => LLVMBuildSDiv(env.builder(), op1, op2, name.as_ptr()),
					BinaryOperator::Exponentiate => todo!(),
					BinaryOperator::Mod => LLVMBuildSRem(env.builder(), op1, op2, name.as_ptr()),
					BinaryOperator::And => todo!(),
					BinaryOperator::Or => todo!(),
					BinaryOperator::Equal
					| BinaryOperator::NotEqual
					| BinaryOperator::LessThan
					| BinaryOperator::LessThanOrEqual
					| BinaryOperator::GreaterThan
					| &BinaryOperator::GreaterThanOrEqual => {
						let name = CString::new("int-cmp").unwrap();
						LLVMBuildICmp(
							env.builder(),
							match e.operator() {
								BinaryOperator::Equal => LLVMIntPredicate::LLVMIntEQ,
								BinaryOperator::NotEqual => LLVMIntPredicate::LLVMIntNE,
								BinaryOperator::GreaterThan => LLVMIntPredicate::LLVMIntSGT,
								BinaryOperator::GreaterThanOrEqual => LLVMIntPredicate::LLVMIntSGE,
								BinaryOperator::LessThan => LLVMIntPredicate::LLVMIntSLT,
								BinaryOperator::LessThanOrEqual => LLVMIntPredicate::LLVMIntSLE,
								_ => unreachable!(),
							},
							op1,
							op2,
							name.as_ptr(),
						)
					}
				}),
				Ty::Float(_) => Some(match e.operator() {
					BinaryOperator::Add => LLVMBuildFAdd(env.builder(), op1, op2, name.as_ptr()),
					BinaryOperator::Subtract => {
						LLVMBuildFSub(env.builder(), op1, op2, name.as_ptr())
					}
					BinaryOperator::Multiply => {
						LLVMBuildFMul(env.builder(), op1, op2, name.as_ptr())
					}
					BinaryOperator::Divide => LLVMBuildFDiv(env.builder(), op1, op2, name.as_ptr()),
					BinaryOperator::Exponentiate => todo!(),
					BinaryOperator::Mod => LLVMBuildFRem(env.builder(), op1, op2, name.as_ptr()),
					BinaryOperator::And => todo!(),
					BinaryOperator::Or => todo!(),
					BinaryOperator::Equal
					| BinaryOperator::NotEqual
					| BinaryOperator::LessThan
					| BinaryOperator::LessThanOrEqual
					| BinaryOperator::GreaterThan
					| &BinaryOperator::GreaterThanOrEqual => {
						let name = CString::new("float-cmp").unwrap();
						LLVMBuildFCmp(
							env.builder(),
							match e.operator() {
								BinaryOperator::Equal => LLVMRealPredicate::LLVMRealOEQ,
								BinaryOperator::NotEqual => LLVMRealPredicate::LLVMRealONE,
								BinaryOperator::GreaterThan => LLVMRealPredicate::LLVMRealOGT,
								BinaryOperator::GreaterThanOrEqual => {
									LLVMRealPredicate::LLVMRealOGE
								}
								BinaryOperator::LessThan => LLVMRealPredicate::LLVMRealOLT,
								BinaryOperator::LessThanOrEqual => LLVMRealPredicate::LLVMRealOLE,
								_ => unreachable!(),
							},
							op1,
							op2,
							name.as_ptr(),
						)
					}
				}),
				Ty::String(_) => todo!(),
				Ty::Boolean(_) => todo!(),
				Ty::Void(_) => todo!(),
				Ty::Never(_) => todo!(),
				Ty::Array(_, _) => todo!(),
				Ty::Fn(_, _, _) => todo!(),
				Ty::Unknown(_, _) => todo!(),
				Ty::Possibility(_, _, _) => todo!(),
				Ty::TyRef(_) => todo!(),
				Ty::Union(_, _) => todo!(),
			}
		}
		Expression::Statement(e) => generate_stmt(ctx, e, env),
	}
}
