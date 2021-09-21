use super::{
	ast::*,
	block::analyze_block,
	dt::analyze_dt,
	env::Environment,
	expr::analyze_expr,
	ty::{Ctx, Ty},
	TypeError,
};

/// Analyze an lvalue node, `LValue<NotAnalyzed>`.
fn analyze_lvalue(
	lvalue: LValue<NotAnalyzed>,
	env: &mut Environment,
	ctx: &mut Ctx,
) -> Result<LValue<Analyzed>, TypeError> {
	let full_span = lvalue.span().clone();
	Ok(match lvalue {
		// l //
		LValue::Variable(l) => LValue::Variable({
			if let Some(ty) = env.find_variable(l.name()) {
				l.analyze(ty, ExitStatus::new())
			} else {
				return Err(TypeError::UnresolvedVariable(full_span.clone()));
			}
		}),
		// l[index] //
		LValue::IndexOf(l) => {
			LValue::IndexOf(l.analyze(|l, index| {
				let l = analyze_lvalue(l, env, ctx)?;
				let mut es = l.exit_status().clone();
				let index = analyze_expr(index, env, ctx)?;
				if index.exit_status().will_exit() {
					return Err(TypeError::DeadCode(full_span));
				}
				es.and(index.exit_status());

				// inner :: ?
				let inner = ctx.new_unknown(full_span.clone());

				// Array<inner> == l
				ctx.add_constraint(
					(
						Ty::Array(Box::new(inner.clone()), full_span.clone()),
						Ty::TyRef(l.ty()),
					),
					full_span.clone(),
				);

				// Int == index
				ctx.add_constraint(
					(Ty::Int(index.span().clone()), Ty::TyRef(index.ty())),
					index.span().clone(),
				);

				Ok((l, index, ctx.new_ty(inner), es))
			})?)
		}
		LValue::PropertyOf(_) => todo!(),
	})
}

/// Analyze a statement node, `Statement<NotAnalyzed>`.
pub fn analyze_stmt(
	stmt: Statement<NotAnalyzed>,
	env: &mut Environment,
	ctx: &mut Ctx,
) -> Result<Statement<Analyzed>, TypeError> {
	let full_span = stmt.span().clone();
	Ok(match stmt {
		// { ... } //
		Statement::Block(s) => Statement::Block(analyze_block(s, env, ctx)?),
		// 1 + 1, 3, 2 * 5,... //
		Statement::Expression(s) => Statement::Expression(analyze_expr(s, env, ctx)?),
		// return e //
		Statement::Return(s) => Statement::Return(s.analyze(|e| {
			let mut es = ExitStatus::new();
			let ret_ty = env.ret_ty().clone();

			let e = if let Some(expr) = e {
				let e = analyze_expr(expr, env, ctx)?;
				// If the expression always exits then the
				// statement will never get to return thus
				// it's redundant/dead code.
				if e.exit_status().will_exit() {
					return Err(TypeError::DeadCode(full_span));
				}
				es.and(e.exit_status());
				// ret_ty == e
				ctx.add_constraint((ret_ty, Ty::TyRef(e.ty())), e.span().clone());
				Some(e)
			} else {
				// ret_ty == Void
				ctx.add_constraint((ret_ty, Ty::Void(full_span.clone())), full_span.clone());
				None
			};

			es.set_return(Status::Will);
			Ok((e, ctx.new_ty(Ty::Never(full_span)), es))
		})?),
		// break e //
		Statement::Break(s) => Statement::Break(s.analyze(|e| {
			let mut es = ExitStatus::new();

			let loop_ty = if let Some(ty) = env.loop_ty().cloned() {
				ty
			} else {
				return Err(TypeError::BreakOutsideLoop(full_span));
			};

			let e = if let Some(expr) = e {
				let e = analyze_expr(expr, env, ctx)?;
				// If the expression always exits then the
				// statement will never get to break thus
				// it's redundant/dead code.
				if e.exit_status().will_exit() {
					return Err(TypeError::DeadCode(full_span));
				}
				es.and(e.exit_status());
				// loop_ty == e
				ctx.add_constraint((loop_ty, Ty::TyRef(e.ty())), e.span().clone());
				Some(e)
			} else {
				// loop_ty == Void
				ctx.add_constraint((loop_ty, Ty::Void(full_span.clone())), full_span.clone());
				None
			};

			es.set_break(Status::Will);
			Ok((e, ctx.new_ty(Ty::Never(full_span)), es))
		})?),
		// let name: dt = e //
		Statement::Let(s) => Statement::Let({
			let name = s.name().clone();
			s.analyze(|dt, e| {
				let e = analyze_expr(e, env, ctx)?;
				let es = e.exit_status().clone();
				if es.will_exit() {
					return Err(TypeError::DeadCode(full_span));
				}

				// Analyze the data type.
				let dt = match dt {
					Some(dt) => {
						let dt = analyze_dt(dt, ctx)?;
						// dt == e
						ctx.add_constraint(
							(Ty::TyRef(dt.ty()), Ty::TyRef(e.ty())),
							e.span().clone(),
						);
						Some(dt)
					}
					None => None,
				};

				env.add_variable(name.clone(), e.ty());
				Ok((dt, e, ctx.new_ty(Ty::Void(full_span)), es))
			})
		}?),
		// l = e //
		Statement::Assign(s) => Statement::Assign({
			s.analyze(|l, e| {
				let l = analyze_lvalue(l, env, ctx)?;
				let mut es = l.exit_status().clone();
				let e = analyze_expr(e, env, ctx)?;
				if e.exit_status().will_exit() {
					return Err(TypeError::DeadCode(full_span));
				}
				es.and(e.exit_status());
				// l == e
				ctx.add_constraint((Ty::TyRef(l.ty()), Ty::TyRef(e.ty())), e.span().clone());
				Ok((l, e, ctx.new_ty(Ty::Void(full_span)), es))
			})
		}?),
		// if e { ... } else if e { ... } else { ... } //
		Statement::If(s) => {
			Statement::If(s.analyze(|if_branch, else_if_branches, else_branch| {
				// ty :: ?
				let ty = ctx.new_unknown(full_span.clone());
				let mut es;
				let mut branch_es;
				let mut will_exit = true;

				// Analyze the main `if` branch.
				let if_branch = {
					let e = analyze_expr(if_branch.0, env, ctx)?;
					if e.exit_status().will_exit() {
						return Err(TypeError::DeadCode(full_span));
					}
					es = e.exit_status().clone();

					// Boolean == e
					ctx.add_constraint(
						(Ty::Boolean(e.span().clone()), Ty::TyRef(e.ty())),
						e.span().clone(),
					);

					let b = analyze_block(if_branch.1, env, ctx)?;
					branch_es = b.exit_status().clone();
					if !b.exit_status().will_exit() {
						will_exit = false;
						// ty == b
						ctx.add_constraint((ty.clone(), Ty::TyRef(b.ty())), b.span().clone());
					}

					(e, b)
				};

				// Analyze all the `else if` branches.
				let else_if_branch = {
					let mut v = Vec::with_capacity(else_if_branches.len());
					for branch in else_if_branches {
						let e = analyze_expr(branch.0, env, ctx)?;
						if e.exit_status().will_exit() {
							return Err(TypeError::DeadCode(full_span));
						}
						let mut es = e.exit_status().clone();

						// Boolean == e
						ctx.add_constraint(
							(Ty::Boolean(e.span().clone()), Ty::TyRef(e.ty())),
							e.span().clone(),
						);

						let b = analyze_block(branch.1, env, ctx)?;
						es.and(b.exit_status());
						branch_es.or(&es);
						if !b.exit_status().will_exit() {
							println!("test");
							will_exit = false;
							// ty == b
							ctx.add_constraint((ty.clone(), Ty::TyRef(b.ty())), b.span().clone());
						}

						v.push((e, b));
					}
					v
				};

				// Analyze the else branch.
				let else_branch = if let Some(else_branch) = else_branch {
					let b = analyze_block(else_branch, env, ctx)?;
					branch_es.or(b.exit_status());
					if !b.exit_status().will_exit() {
						will_exit = false;
						// ty == b
						ctx.add_constraint((ty.clone(), Ty::TyRef(b.ty())), b.span().clone());
					}
					Some(b)
				} else {
					branch_es.or(&ExitStatus::new());
					will_exit = false;
					// ty == Void
					ctx.add_constraint(
						(ty.clone(), Ty::Void(full_span.clone())),
						full_span.clone(),
					);
					None
				};

				branch_es.set_will_exit(will_exit);
				es.and(&branch_es);
				let ty_ref = ctx.new_ty(ty.clone());
				if es.will_exit() {
					println!("test");
					ctx.resolve_unknown(
						if let Ty::Unknown(id, _) = ty {
							id
						} else {
							unreachable!()
						},
						Ty::Never(full_span),
					);
				}
				Ok((if_branch, else_if_branch, else_branch, ty_ref, es))
			})?)
		}
		// loop { ... } //
		Statement::Loop(s) => Statement::Loop({
			s.analyze(|b| {
				// ty :: ?
				let ty = ctx.new_unknown(b.span().clone());
				let id = if let Ty::Unknown(id, _) = ty {
					id
				} else {
					unreachable!()
				};
				let ty_ref = ctx.new_ty(ty);

				env.add_loop(Ty::TyRef(ty_ref.clone()));
				let b = analyze_block(b, env, ctx)?;
				env.remove_loop();
				let mut es = b.exit_status().clone();

				if es.break_status() == Status::Never {
					ctx.resolve_unknown(
						id,
						// If the loop body will never break but
						// the loop body might return then it is
						// assumed that the loop must exit by
						// returning.
						if es.return_status() != Status::Never {
							es.set_return(Status::Will);
							// ty :: Never
							Ty::Never(full_span.clone())
						}
						// If the loop body never breaks or
						// returns then the loop must run
						// indefinitely.
						else {
							es.set_infinite_loop(Status::Will);
							// ty :: Void
							Ty::Never(full_span.clone())
						},
					);
				} else {
					// Breaks within the loop body
					// won't affect outside code.
					es.set_break(Status::Never);
					es.or(&ExitStatus::new());
				}

				Ok((b, ty_ref, es))
			})?
		}),
	})
}
