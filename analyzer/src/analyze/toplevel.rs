use super::{
	analyze::TopLevelInfo, ast::*, block::analyze_block, env::Environment, ty::Ctx, ty::Ty,
	TypeError,
};

/// Analyze a top level node, `TopLevel<NotAnalyzed>`.
pub fn analyze_toplevel(
	top_level: TopLevel<NotAnalyzed>,
	info: &TopLevelInfo,
	ctx: &mut Ctx,
) -> Result<TopLevel<Analyzed>, TypeError> {
	match top_level {
		// fn name(arg1: dt1, arg2: dt2, ...) -> ret_dt { ... }
		TopLevel::Function(f) => {
			// Get the pre-analyzed function type and signature.
			let fn_type = info.fn_tys.get(f.name()).unwrap();
			let fn_sig = info.fn_sigs.get(f.name()).cloned().unwrap();

			let ret_ty = if let Ty::Fn(_, r, _) = fn_type {
				*r.clone()
			} else {
				unreachable!();
			};

			Ok(TopLevel::Function(f.analyze(|_, _, block| {
				let mut env = Environment::new(ret_ty.clone());
				env.add_scope();

				// Add every function into the local scope.
				// (This is required because otherwise code inside
				// this function body cannot call itself or other
				// functions.)
				for (name, ty) in info.fn_tys.iter() {
					let ty = ctx.new_ty(ty.clone());
					env.add_variable(name.clone(), ty);
				}

				// Add every parameter into the local scope.
				fn_sig.0.iter().enumerate().for_each(|(idx, (name, _))| {
					if let Ty::Fn(p, _, _) = &fn_type {
						let ty = p[idx].clone();
						let ty = ctx.new_ty(ty);
						env.add_variable(name.clone(), ty);
					} else {
						unreachable!();
					}
				});

				let block = analyze_block(block, &mut env, ctx)?;
				env.remove_scope();
				assert!(env.scopes() == 0);

				// Check whether the function return type is
				// Void and, if not, make sure the function
				// body definitely returns a value.
				let ret_ty = ctx.substitute_ty(ret_ty.clone());
				match ret_ty {
					Ty::Void(_) => (),
					_ => {
						let es = block.exit_status();
						if !block.does_eval() && es.return_status() != Status::Will {
							return Err(TypeError::ReturnRequired(
								ret_ty.span().clone(),
								block.span().clone(),
							));
						}
						if es.infinite_loop_status() != Status::Never {
							return Err(TypeError::InfiniteLoop(
								ret_ty.span().clone(),
								block.span().clone(),
							));
						}
						if es.break_status() != Status::Never {
							unreachable!();
						}
					}
				};

				if block.does_eval() && !block.exit_status().will_exit() {
					// ret_ty == block
					ctx.add_constraint(
						(ret_ty.clone(), Ty::TyRef(block.ty())),
						block.span().clone(),
					);
				}

				Ok((fn_sig.0, fn_sig.1, block, ctx.new_ty(fn_type.clone())))
			})?))
		}
	}
}
