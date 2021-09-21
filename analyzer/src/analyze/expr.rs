use super::{
	ast::*,
	env::Environment,
	stmt::analyze_stmt,
	ty::{Ctx, Ty},
	TypeError,
};

/// Analyze an expression node, `Expression<NotAnalyzed>`.
pub fn analyze_expr(
	expr: Expression<NotAnalyzed>,
	env: &mut Environment,
	ctx: &mut Ctx,
) -> Result<Expression<Analyzed>, TypeError> {
	let full_span = expr.span().clone();
	Ok(match expr {
		// (e) //
		Expression::Parenthesis(e) => Expression::Parenthesis(e.analyze(|e| {
			let e = analyze_expr(e, env, ctx)?;
			let ty = e.ty();
			let es = e.exit_status().clone();
			Ok((e, ty, es))
		})?),
		// "hello", 1, false,... //
		Expression::Literal(e) => Expression::Literal({
			let ty = match e.value() {
				LiteralValue::Int(_) => Ty::Int(full_span), // ty :: Int
				LiteralValue::Float(_) => Ty::Float(full_span), // ty :: Float
				LiteralValue::Boolean(_) => Ty::Boolean(full_span), // ty :: Boolean
				LiteralValue::String(_) => Ty::String(full_span), // ty :: String
			};
			e.analyze(ctx.new_ty(ty), ExitStatus::new())
		}),
		// [...elems] //
		Expression::Array(e) => Expression::Array(e.analyze(|elems| {
			let mut analyzed = vec![];
			let mut es = ExitStatus::new();

			// Analyze all the elements of the array.
			for elem in elems {
				let elem = analyze_expr(elem, env, ctx)?;
				// All the elements will have to be evaluated
				// to evaluate the array thus all of their exit
				// statuses must be combined.
				es.and(elem.exit_status());
				analyzed.push(elem);
			}

			// elem_ty :: ?
			let elem_ty = ctx.new_unknown(full_span.clone());
			// elem_ty == elems[0] == elems[1] == ..
			// (Equate the types of all the elements)
			for elem in &analyzed {
				ctx.add_constraint((elem_ty.clone(), Ty::TyRef(elem.ty())), elem.span().clone());
			}

			Ok((
				analyzed,
				// Array<elem_ty>
				ctx.new_ty(Ty::Array(Box::new(elem_ty), full_span)),
				es,
			))
		})?),
		// e //
		Expression::Variable(e) => {
			if let Some(ty) = env.find_variable(e.name()) {
				Expression::Variable(e.analyze(ty, ExitStatus::new()))
			}
			// If the name of the variable is "print" then
			// return the type of the global print function.
			else if e.name() == "print" {
				// p :: Int || Float || Boolean || String
				let p = ctx.new_possibility(
					vec![
						Ty::Int(full_span.clone()),
						Ty::Float(full_span.clone()),
						Ty::Boolean(full_span.clone()),
						Ty::String(full_span.clone()),
					],
					full_span.clone(),
				);

				// fn_ty :: Fn(p) -> Void
				let fn_ty = ctx.new_ty(Ty::Fn(
					vec![p],
					Box::new(Ty::Void(full_span.clone())),
					full_span,
				));

				Expression::Variable(e.analyze(fn_ty, ExitStatus::new()))
			} else {
				return Err(TypeError::UnresolvedVariable(full_span));
			}
		}
		// e[index] //
		Expression::IndexOf(e) => Expression::IndexOf(e.analyze(|e, index| {
			let (e, index) = (analyze_expr(e, env, ctx)?, analyze_expr(index, env, ctx)?);
			let mut es = e.exit_status().clone();
			es.and(index.exit_status());

			// inner :: ?
			let inner = ctx.new_unknown(full_span.clone());

			// Array<inner> == e
			// (Make sure the expression being indexed
			// is an array and also simultaneously infer
			// the element type.)
			ctx.add_constraint(
				(
					Ty::Array(Box::new(inner.clone()), e.span().clone()),
					Ty::TyRef(e.ty()),
				),
				full_span,
			);

			// Int == index
			ctx.add_constraint(
				(Ty::Int(index.span().clone()), Ty::TyRef(index.ty())),
				index.span().clone(),
			);

			Ok((e, index, ctx.new_ty(inner), es.clone()))
		})?),
		// e.name //
		Expression::PropertyOf(_e) => {
			todo!();
		}
		// e(...args) //
		Expression::Call(e) => Expression::Call(e.analyze(|e, args| {
			let e = analyze_expr(e, env, ctx)?;
			let mut es = e.exit_status().clone();
			let mut analyzed = vec![];

			// Analyze all the arguments.
			for arg in args {
				let arg = analyze_expr(arg, env, ctx)?;
				es.and(arg.exit_status());
				analyzed.push(arg);
			}

			// ret_ty :: ?
			let ret_ty = ctx.new_unknown(full_span.clone());
			// e == Fn(...args) -> ret_ty
			// (Make sure that the expression has a function
			// signature which satisfies the arguments provided
			// and also simultaneously infer the return type.)
			ctx.add_constraint(
				(
					Ty::TyRef(e.ty()),
					Ty::Fn(
						analyzed.iter().map(|x| Ty::TyRef(x.ty())).collect(),
						Box::new(ret_ty.clone()),
						e.span().clone(),
					),
				),
				full_span,
			);

			Ok((e, analyzed, ctx.new_ty(ret_ty), es.clone()))
		})?),
		// e.name(...args) //
		Expression::NamedCall(e) => {
			let name = e.name().clone();
			Expression::NamedCall(e.analyze(|e, args| {
				let e = analyze_expr(e, env, ctx)?;
				let mut es = e.exit_status().clone();
				let mut analyzed = vec![];

				// Analyze all the arguments.
				for arg in args {
					let arg = analyze_expr(arg, env, ctx)?;
					es.and(arg.exit_status());
					analyzed.push(arg);
				}

				// ret_ty :: ?
				let ret_ty = ctx.new_unknown(full_span.clone());
				// fn_ty :: Fn(...args) -> ret_ty
				let fn_ty = Ty::Fn(
					analyzed.iter().map(|x| Ty::TyRef(x.ty())).collect(),
					Box::new(ret_ty.clone()),
					e.span().clone(),
				);

				// cmp_ty :: if name == "len" then Fn() -> Int
				let cmp_ty = if name == "len" {
					// String == e
					ctx.add_constraint(
						(Ty::String(e.span().clone()), Ty::TyRef(e.ty())),
						e.span().clone(),
					);
					Ty::Fn(
						vec![],
						Box::new(Ty::Int(full_span.clone())),
						full_span.clone(),
					)
				} else {
					todo!();
				};

				// cmp_ty == fn_ty
				ctx.add_constraint((cmp_ty, fn_ty), full_span);

				Ok((e, analyzed, ctx.new_ty(ret_ty), ExitStatus::new()))
			})?)
		}
		// (-/not)op //
		Expression::Unary(e) => {
			let operator = e.operator().clone();
			Expression::Unary(e.analyze(|op| {
				let op = analyze_expr(op, env, ctx)?;
				let es = op.exit_status().clone();

				Ok(match operator {
					// - //
					UnaryOperator::Minus => {
						let ty = op.ty();
						// p :: Int || Float
						let p = ctx.new_possibility(
							vec![Ty::Int(op.span().clone()), Ty::Float(op.span().clone())],
							op.span().clone(),
						);
						// p == op
						ctx.add_constraint((p, Ty::TyRef(ty.clone())), op.span().clone());
						(op, ty, es)
					}
					// not //
					UnaryOperator::Not => {
						let ty = op.ty();
						// Boolean == op
						ctx.add_constraint(
							(Ty::Boolean(op.span().clone()), Ty::TyRef(ty.clone())),
							op.span().clone(),
						);
						(op, ty, es)
					}
				})
			})?)
		}
		// op1 (+,-,*,..) op2 //
		Expression::Binary(e) => {
			let operator = e.operator().clone();
			Expression::Binary(e.analyze(|op1, op2| {
				let (op1, op2) = (analyze_expr(op1, env, ctx)?, analyze_expr(op2, env, ctx)?);

				let mut es = op1.exit_status().clone();
				// Because of short-circuiting both operands may not be evaluated.
				if let BinaryOperator::And | BinaryOperator::Or = operator {
					es.or(op2.exit_status());
				} else {
					es.and(op2.exit_status());
				}

				Ok(match operator {
					// +, -, /, mod, ^ //
					BinaryOperator::Add
					| BinaryOperator::Subtract
					| BinaryOperator::Divide
					| BinaryOperator::Mod
					| BinaryOperator::Exponentiate => {
						// ty :: Int || Float
						let ty = ctx.new_possibility(
							vec![Ty::Int(full_span.clone()), Ty::Float(full_span.clone())],
							full_span,
						);

						// ty == op1 == op2
						ctx.add_constraint((ty.clone(), Ty::TyRef(op1.ty())), op1.span().clone());
						ctx.add_constraint((ty.clone(), Ty::TyRef(op2.ty())), op2.span().clone());

						(op1, op2, ctx.new_ty(ty), es)
					}
					// * //
					BinaryOperator::Multiply => {
						// ty :: ?
						let ty = ctx.new_unknown(full_span.clone());
						// fn_type :: Fn(op1, op2) -> ty
						let fn_ty = Ty::Fn(
							vec![Ty::TyRef(op1.ty()), Ty::TyRef(op2.ty())],
							Box::new(ty.clone()),
							full_span.clone(),
						);

						let mul_fn1 = {
							// p == Int || Float
							let p = ctx.new_possibility(
								vec![Ty::Int(full_span.clone()), Ty::Float(full_span.clone())],
								full_span.clone(),
							);
							// mul_fn1 :: Fn(p, p) -> p
							Ty::Fn(
								vec![p.clone(), p.clone()],
								Box::new(p.clone()),
								full_span.clone(),
							)
						};

						// mul_fn2 :: Fn(String, Int) -> Int
						let mul_fn2 = Ty::Fn(
							vec![Ty::String(op1.span().clone()), Ty::Int(op2.span().clone())],
							Box::new(Ty::String(full_span.clone())),
							full_span.clone(),
						);

						// mul_fn3 :: Fn(Int, String) -> Int
						let mul_fn3 = Ty::Fn(
							vec![Ty::Int(op1.span().clone()), Ty::String(op2.span().clone())],
							Box::new(Ty::String(full_span.clone())),
							full_span.clone(),
						);

						// p :: mul_fn1 || mul_fn2 || mul_fn3
						let p =
							ctx.new_possibility(vec![mul_fn1, mul_fn2, mul_fn3], full_span.clone());
						// p == fn_type
						ctx.add_constraint((p, fn_ty), full_span);

						(op1, op2, ctx.new_ty(ty), es)
					}
					// >, >=, <, <= //
					BinaryOperator::GreaterThan
					| BinaryOperator::GreaterThanOrEqual
					| BinaryOperator::LessThan
					| BinaryOperator::LessThanOrEqual => {
						// ty :: Int || Float
						let ty = ctx.new_possibility(
							vec![Ty::Int(full_span.clone()), Ty::Float(full_span.clone())],
							full_span.clone(),
						);

						// ty == op1 == op2
						ctx.add_constraint((ty.clone(), Ty::TyRef(op1.ty())), op1.span().clone());
						ctx.add_constraint((ty.clone(), Ty::TyRef(op2.ty())), op2.span().clone());

						(op1, op2, ctx.new_ty(Ty::Boolean(full_span)), es)
					}
					// and, or //
					BinaryOperator::And | BinaryOperator::Or => {
						// Boolean == op1 == op2
						ctx.add_constraint(
							(Ty::Boolean(op1.span().clone()), Ty::TyRef(op1.ty())),
							op1.span().clone(),
						);
						ctx.add_constraint(
							(Ty::Boolean(op2.span().clone()), Ty::TyRef(op2.ty())),
							op2.span().clone(),
						);
						(op1, op2, ctx.new_ty(Ty::Boolean(full_span)), es)
					}
					// ==, != //
					BinaryOperator::Equal | BinaryOperator::NotEqual => {
						// ty :: Int || Float || String || Boolean
						let ty = ctx.new_possibility(
							vec![
								Ty::Int(full_span.clone()),
								Ty::Float(full_span.clone()),
								Ty::String(full_span.clone()),
								Ty::Boolean(full_span.clone()),
							],
							full_span.clone(),
						);

						// ty == op1 == op2
						ctx.add_constraint((ty.clone(), Ty::TyRef(op1.ty())), op1.span().clone());
						ctx.add_constraint((ty, Ty::TyRef(op2.ty())), op2.span().clone());

						(op1, op2, ctx.new_ty(Ty::Boolean(full_span)), es)
					}
				})
			})?)
		}
		// if a { b } else { c }, loop { break a }, { a }...
		Expression::Statement(e) => Expression::Statement(Box::new(analyze_stmt(*e, env, ctx)?)),
	})
}
