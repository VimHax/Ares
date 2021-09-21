use llvm_sys::{core::*, prelude::LLVMValueRef};
use std::ffi::CString;

use analyzer::{
	ast::{Analyzed, LValue, Statement},
	ty::{Ctx, Ty},
};

use super::{
	block::generate_block,
	expr::generate_expr,
	generate_ir::{ty_to_ir, Environment},
};

/// Generate LLVM IR for an AST `Statement<Analyzed>` node.
pub unsafe fn generate_stmt<'a>(
	ctx: &Ctx,
	stmt: &'a Statement<Analyzed>,
	env: &mut Environment<'a>,
) -> Option<LLVMValueRef> {
	match stmt {
		Statement::Block(s) => generate_block(ctx, s, env),
		Statement::Expression(s) => generate_expr(ctx, s, env),
		Statement::Return(s) => {
			// If there's an expression return the expression
			// otherwise return void.
			if let Some(e) = s.expression() {
				LLVMBuildRet(env.builder(), generate_expr(ctx, e, env).unwrap());
			} else {
				LLVMBuildRetVoid(env.builder());
			}
			None
		}
		Statement::Break(_s) => {
			assert!(_s.expression().is_none());
			LLVMBuildBr(env.builder(), *env.get_loop().unwrap());
			None
		}
		Statement::Let(s) => {
			let name = CString::new(s.name().as_str()).unwrap();
			// Allocate memory in the stack to store the expression.
			let value = LLVMBuildAlloca(
				env.builder(),
				ty_to_ir(ctx.resolve_ref(&s.value().ty()), env.datatypes()),
				name.as_ptr(),
			);
			// Store the value of the expression in the allocation.
			LLVMBuildStore(
				env.builder(),
				generate_expr(ctx, s.value(), env).unwrap(),
				value,
			);
			env.add_variable(s.name(), value);
			None
		}
		Statement::Assign(s) => {
			// Get the stack pointer of the variable.
			let name = if let LValue::Variable(v) = s.lvalue() {
				env.find_variable(v.name()).expect("var found")
			} else {
				todo!()
			};
			// Store the value of the expression to the stack.
			let value = generate_expr(ctx, s.value(), env).expect("not void");
			LLVMBuildStore(env.builder(), value, name);
			None
		}
		Statement::If(s) => {
			// Check whether the if statement evaluates to a value
			// and if so get the LLVM type of that value.
			let eval_type = match ctx.resolve_ref(&s.ty()) {
				Ty::Void(_) => None,
				Ty::Never(_) => None,
				ty => Some(ty_to_ir(ty, env.datatypes())),
			};

			// Allocate memory in the stack for the value being evaluated
			// to be stored at.
			let if_eval = if let Some(eval_type) = eval_type {
				let name = CString::new("if-eval").unwrap();
				Some(LLVMBuildAlloca(env.builder(), eval_type, name.as_ptr()))
			} else {
				None
			};

			// Create the then block.
			let name = CString::new("then").unwrap();
			let then_bb =
				LLVMAppendBasicBlockInContext(env.context(), env.current_fn(), name.as_ptr());

			// Create all the else-if conditions and else-if body blocks.
			let else_if_bb = s
				.else_if_branch()
				.iter()
				.map(|_x| {
					let name = CString::new("else-if-cond").unwrap();
					let else_if_cond = LLVMAppendBasicBlockInContext(
						env.context(),
						env.current_fn(),
						name.as_ptr(),
					);
					let name = CString::new("else-if").unwrap();
					let else_if = LLVMAppendBasicBlockInContext(
						env.context(),
						env.current_fn(),
						name.as_ptr(),
					);
					(else_if_cond, else_if)
				})
				.collect::<Vec<_>>();

			// Create the else block.
			let name = CString::new("else").unwrap();
			let else_bb = if let Some(_) = s.else_branch() {
				Some(LLVMAppendBasicBlockInContext(
					env.context(),
					env.current_fn(),
					name.as_ptr(),
				))
			} else {
				None
			};

			// Create the final block (which is after all the other blocks)
			// which the other blocks will jump to after they are done executing.
			// If all the branches in the if statement exit then the final block
			// is unnecessary as code execution will never reach it.
			let finally_bb = if s.exit_status().will_exit() {
				None
			} else {
				let name = CString::new("if-finally").unwrap();
				Some(LLVMAppendBasicBlockInContext(
					env.context(),
					env.current_fn(),
					name.as_ptr(),
				))
			};

			// Generate the IR for the main if expression.
			let val = generate_expr(ctx, &s.if_branch().0, env).unwrap();

			// Build a conditional which will jump to the then block
			// if the main expression is true and otherwise jump to
			// an else-if or else or to the final block.
			LLVMBuildCondBr(
				env.builder(),
				val,
				then_bb,
				if else_if_bb.len() != 0 {
					else_if_bb.first().unwrap().0
				} else if let Some(bb) = else_bb {
					bb
				} else {
					finally_bb.unwrap()
				},
			);

			// Generate the IR for the then block.
			LLVMPositionBuilderAtEnd(env.builder(), then_bb);
			let eval = generate_block(ctx, &s.if_branch().1, env);
			// If the then block evaluates then store the value in the
			// previously allocated stack memory.
			if let Some(eval) = eval {
				LLVMBuildStore(env.builder(), eval, if_eval.unwrap());
			}
			// If the then block does not exit jump to the final block.
			if !s.if_branch().1.exit_status().will_exit() {
				LLVMBuildBr(env.builder(), finally_bb.unwrap());
			}

			// Generate the IR for all the else-if branches.
			for (idx, bb) in else_if_bb.iter().enumerate() {
				LLVMPositionBuilderAtEnd(env.builder(), bb.0);
				let branch = &s.else_if_branch()[idx];
				// Generate the IR for the else-if condition.
				let val = generate_expr(ctx, &branch.0, env).unwrap();
				// If the condition is true then jump to this else-if's block
				// otherwise jump to any other remaining else-if or to else
				// or to the final block.
				LLVMBuildCondBr(
					env.builder(),
					val,
					bb.1,
					if else_if_bb.len() > idx + 1 {
						else_if_bb[idx + 1].0
					} else if let Some(bb) = else_bb {
						bb
					} else {
						finally_bb.unwrap()
					},
				);

				// Generate the IR for this else-if's block.
				LLVMPositionBuilderAtEnd(env.builder(), bb.1);
				let eval = generate_block(ctx, &branch.1, env);
				// If the else-if's block evaluates then store the value in the
				// previously allocated stack memory.
				if let Some(if_eval) = if_eval {
					LLVMBuildStore(env.builder(), eval.unwrap(), if_eval);
				}
				// If the else-if's block does not exit jump to the final block.
				if !branch.1.exit_status().will_exit() {
					LLVMBuildBr(env.builder(), finally_bb.unwrap());
				}
			}

			// Generate the IR for the else branch.
			if let Some(bb) = else_bb {
				LLVMPositionBuilderAtEnd(env.builder(), bb);
				let block = &s.else_branch().as_ref().unwrap().0;
				// Generate the IR for the else's block.
				let eval = generate_block(ctx, block, env);
				// If the else's block evaluates then store the value in the
				// previously allocated stack memory.
				if let Some(eval) = eval {
					LLVMBuildStore(env.builder(), eval, if_eval.unwrap());
				}
				// If the else's block does not exit jump to the final block.
				if !block.exit_status().will_exit() {
					LLVMBuildBr(env.builder(), finally_bb.unwrap());
				}
			}

			// If the final block exists and the if statement evaluates
			// then load the value stored and return it, otherwise return
			// None.
			if let Some(bb) = finally_bb {
				LLVMPositionBuilderAtEnd(env.builder(), bb);
				if let Some(if_eval) = if_eval {
					let name = CString::new("if-eval").unwrap();
					Some(LLVMBuildLoad(env.builder(), if_eval, name.as_ptr()))
				} else {
					None
				}
			} else {
				None
			}
		}
		Statement::Loop(s) => {
			// Create the loop body block.
			let name = CString::new("loop").unwrap();
			let loop_bb =
				LLVMAppendBasicBlockInContext(env.context(), env.current_fn(), name.as_ptr());
			// Create the loop's final block.
			// This is the block the loop will jump to if the loop
			// breaks, if the loop never breaks this final block
			// is unnecessary.
			let finally_bb = if !s.exit_status().will_exit() {
				let name = CString::new("loop-finally").unwrap();
				let finally_bb =
					LLVMAppendBasicBlockInContext(env.context(), env.current_fn(), name.as_ptr());
				env.add_loop(finally_bb);
				Some(finally_bb)
			} else {
				None
			};

			// Jump to the loop body.
			LLVMBuildBr(env.builder(), loop_bb);
			LLVMPositionBuilderAtEnd(env.builder(), loop_bb);
			// Generate the IR for the loop body.
			generate_block(ctx, s.block(), env);
			// Jump to the loop body again.
			// If the block always returns then
			// there is no point in jumping back into
			// the body.
			if !s.block().exit_status().will_exit() {
				LLVMBuildBr(env.builder(), loop_bb);
			}

			if let Some(bb) = finally_bb {
				env.remove_loop();
				LLVMPositionBuilderAtEnd(env.builder(), bb);
			}
			None
		}
	}
}
