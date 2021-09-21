use ares_core::Span;

use super::{ast::*, env::Environment, stmt::analyze_stmt, ty::Ctx, ty::Ty, TypeError};

/// Analyze a block node, `Block<NotAnalyzed>`.
pub fn analyze_block(
	block: Block<NotAnalyzed>,
	env: &mut Environment,
	ctx: &mut Ctx,
) -> Result<Block<Analyzed>, TypeError> {
	let full_span = block.span().clone();
	let does_eval = block.does_eval();

	Ok(block.analyze(|stmts| {
		let mut es = ExitStatus::new();
		let mut analyzed = vec![];
		let mut ty = None;
		let original_len = stmts.len();
		let mut dead_code = vec![];

		// Analyze all the statements in the block.
		env.add_scope();
		for (idx, stmt) in stmts.into_iter().enumerate() {
			// If `will_exit()` returns `true` that means one
			// of the previous statements are guaranteed to
			// cause the execution to stop, thus all the statements
			// which come after are bound to be unreachable.
			if es.will_exit() {
				// Add all the unreachable statements to `dead_code`.
				dead_code.push(stmt);
				continue;
			}
			let stmt = analyze_stmt(stmt, env, ctx)?;
			// Combine the statements' exit status.
			// eg: If the statement has a chance of
			// looping indefinitely then the entire block
			// must also have a chance of doing so.
			es.and(stmt.exit_status());
			let is_last = idx == original_len - 1;
			// If the block evaluates and this is
			// the last statement then the entire
			// block must evaluate to the value
			// of this statement.
			if does_eval && is_last && !stmt.exit_status().will_exit() {
				ty.replace(stmt.ty());
			}
			analyzed.push(stmt);
		}
		env.remove_scope();

		// If there is some dead code return a type error.
		if dead_code.len() != 0 {
			let span = Span::combine_span(
				dead_code.first().unwrap().span(),
				dead_code.last().unwrap().span(),
			);
			return Err(TypeError::DeadCode(span));
		}

		let ty = if let Some(ty) = ty {
			ty
		} else {
			ctx.new_ty(
				// If the block is guaranteed to exit
				// then it will ultimately never get the
				// chance to evaluate to a value, thus it is
				// assigned the Never type.
				if es.will_exit() {
					Ty::Never(full_span.clone())
				}
				// Otherwise if the block does not evaluate
				// to a value then the Void type is assigned
				// by default.
				else {
					Ty::Void(full_span.clone())
				},
			)
		};

		Ok((analyzed, ty, es))
	})?)
}
