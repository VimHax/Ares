use crate::parser::{dt::parse_dt, expr::parse_expr};

use analyzer::ast::*;
use ares_core::{Keyword, Literal, Operator, Span, TokenKind};

use super::{token_iter::TokenIter, ParseError};

/// Parse a statement.
/// eg: `let a = 4;`, `return 5;`, `print("Hello World!")` etc.
pub fn parse_stmt<'a>(
	iter: &mut TokenIter<'a>,
	check_expr: bool,
) -> Result<Statement<NotAnalyzed>, ParseError> {
	// Get the first token.
	let t1 = match iter.current_token() {
		Some(value) => value,
		None => return Err(ParseError::ExpectedStatementButFound(None)),
	};
	Ok(match t1.kind() {
		TokenKind::Keyword(kw) => match kw {
			// return or return <expr>
			Keyword::Return => {
				iter.next();
				iter.wind();
				// Check whether an expression is present.
				let expr = match parse_expr(iter, true) {
					Ok(value) => {
						// If a valid expression does exist
						// then unwind in place as the iterator's
						// current position will be the start of
						// other code.
						iter.unwind_inplace();
						Some(value)
					}
					Err(ParseError::ExpectedExpressionButFound(_)) => {
						// This error is thrown by `parse_expr` in the expectation
						// that the caller is strictly expecting an expression
						// but that's not the case here as the expression is
						// optional, thus we can ignore this error and rewind.
						iter.unwind();
						None
					}
					Err(err) => {
						// Any other errors thrown must be because the
						// expression which was found was invalid but definitely
						// intended to be an expression.
						return Err(err);
					}
				};
				// Make sure to take in to account the span of the expression
				// parsed if it was present.
				let span = if let Some(expr) = &expr {
					Span::combine_span(t1.span(), expr.span())
				} else {
					t1.span().clone()
				};
				Statement::Return(Return::new(expr, span))
			}
			// break or break <expr>
			Keyword::Break => {
				iter.next();
				iter.wind();
				// Check whether an expression is present.
				// (Read the above comments if you want an explanation of the code below.)
				let expr = match parse_expr(iter, true) {
					Ok(value) => {
						iter.unwind_inplace();
						Some(value)
					}
					Err(ParseError::ExpectedExpressionButFound(_)) => {
						iter.unwind();
						None
					}
					Err(err) => {
						return Err(err);
					}
				};
				// Make sure to take in to account the span of the expression
				// parsed if it was present.
				let span = if let Some(expr) = &expr {
					Span::combine_span(t1.span(), expr.span())
				} else {
					t1.span().clone()
				};
				Statement::Break(Break::new(expr, span))
			}
			// let a = 5 or let mut b: Float = 5.0
			Keyword::Let => {
				iter.next();
				// Check whether a `mut` keyword is present.
				let mutable =
					if let Some(TokenKind::Keyword(Keyword::Mut)) = iter.current_token_kind() {
						iter.next();
						true
					} else {
						false
					};
				// Get the variable name.
				let name = match iter
					.consume_token(vec![TokenKind::Literal(Literal::Identifier(
						"".to_string(),
					))])?
					.kind()
				{
					TokenKind::Literal(Literal::Identifier(name)) => name.clone(),
					_ => unreachable!(),
				};
				// Get the data type if it's present.
				let dt =
					if let Some(TokenKind::Operator(Operator::Colon)) = iter.current_token_kind() {
						iter.next();
						Some(parse_dt(iter)?)
					} else {
						None
					};
				// Make sure the next immediate token is a `=`.
				iter.consume_token(vec![TokenKind::Operator(Operator::Eq)])?;
				// Extract the expression.
				let value = parse_expr(iter, true)?;
				let span = Span::combine_span(t1.span(), value.span());
				Statement::Let(Let::new(mutable, name, dt, value, span))
			}
			// if expr { ... } else if expr { ... } else { ... }
			Keyword::If => {
				iter.next();
				let if_branch = {
					// Extract the initial condition.
					let condition = parse_expr(iter, true)?;
					// Extract the first block.
					let block = parse_block(iter)?;
					let span = Span::combine_span(t1.span(), block.span());
					(condition, block, span)
				};
				let mut else_if_branch: Vec<(Expression<NotAnalyzed>, Block<NotAnalyzed>, Span)> =
					vec![];
				let mut else_branch: Option<(Block<NotAnalyzed>, Span)> = None;
				// Parse all the possible following `else if`s and `else`s.
				while let Some(TokenKind::Keyword(Keyword::Else)) = iter.current_token_kind() {
					let span = iter.current_token_span().unwrap();
					iter.next();
					// Check if a `if` proceeds the `else`.
					if let Some(TokenKind::Keyword(Keyword::If)) = iter.current_token_kind() {
						iter.next();
						// Extract the condition.
						let condition = parse_expr(iter, true)?;
						// Extract the following block.
						let block = parse_block(iter)?;
						let span = Span::combine_span(span, block.span());
						else_if_branch.push((condition, block, span));
					} else {
						// Extract the following block.
						let block = parse_block(iter)?;
						let span = Span::combine_span(span, block.span());
						else_branch.replace((block, span));
						break;
					}
				}
				// Get the full span of the `if` statement.
				let full_span = Span::combine_span(t1.span(), {
					if let Some(b) = &else_branch {
						&b.1
					} else if else_if_branch.len() > 0 {
						&else_if_branch.last().unwrap().2
					} else {
						&if_branch.2
					}
				});
				Statement::If(If::new(if_branch, else_if_branch, else_branch, full_span))
			}
			// loop { ... }
			Keyword::Loop => {
				iter.next();
				// Extract the following block.
				let block = parse_block(iter)?;
				let span = Span::combine_span(t1.span(), block.span());
				Statement::Loop(Loop::new(block, span))
			}
			_ => {
				return Err(ParseError::ExpectedStatementButFound(Some(
					t1.span().clone(),
				)))
			}
		},
		// a = expr
		TokenKind::Literal(Literal::Identifier(name)) => {
			iter.wind();
			// Check whether the content can be interpreted
			// as an assignment.
			iter.next();
			let mut lvalue = LValue::Variable(LVariable::new(name.clone(), t1.span().clone()));
			let lvalue_span = lvalue.span().clone();
			let err = loop {
				// Get the first token of a possible accessor
				// or a `=`.
				let t2 = match iter.current_token() {
					Some(value) => value,
					None => {
						break ParseError::ExpectedButFound(
							vec![
								TokenKind::Operator(Operator::LBracket),
								TokenKind::Operator(Operator::Dot),
								TokenKind::Operator(Operator::Eq),
							],
							None,
						)
					}
				};
				// lvalue[index]
				if let Some(Operator::LBracket) = t2.operator() {
					iter.next();
					// Get the index expression.
					let expr = match parse_expr(iter, true) {
						Ok(value) => value,
						Err(e) => break e,
					};
					// Check if the expression is immediately followed by a closing `]`.
					match iter.consume_token(vec![TokenKind::Operator(Operator::RBracket)]) {
						Ok(t3) => {
							lvalue = LValue::IndexOf(LIndexOf::new(
								lvalue,
								expr,
								Span::combine_span(&lvalue_span, t3.span()),
							));
							continue;
						}
						Err(e) => break e,
					};
				}
				// lvalue.property
				if let Some(Operator::Dot) = t2.operator() {
					iter.next();
					// Check if the next token is an identifier.
					match iter.consume_token(vec![TokenKind::Literal(Literal::Identifier(
						"".to_string(),
					))]) {
						Ok(t3) => {
							let name = match t3.kind() {
								TokenKind::Literal(Literal::Identifier(name)) => name.clone(),
								_ => unreachable!(),
							};
							lvalue = LValue::PropertyOf(LPropertyOf::new(
								lvalue,
								name,
								Span::combine_span(&lvalue_span, t3.span()),
							));
							continue;
						}
						Err(e) => break e,
					};
				}
				// lvalue = expr (the assignment statement)
				if let Some(Operator::Eq) = t2.operator() {
					iter.next();
					// At this point it is clear that the content was intended to be
					// interpreted as an assignment.
					iter.unwind_inplace();
					// Extract the expression.
					let value = parse_expr(iter, true)?;
					let span = Span::combine_span(t1.span(), value.span());
					return Ok(Statement::Assign(Assign::new(lvalue, value, span)));
				}
				break ParseError::ExpectedButFound(
					vec![
						TokenKind::Operator(Operator::LBracket),
						TokenKind::Operator(Operator::Dot),
						TokenKind::Operator(Operator::Eq),
					],
					Some(t2.span().clone()),
				);
			};
			if check_expr {
				iter.unwind();
				// Try to reinterpret the content as an expression.
				match parse_expr(iter, false) {
					Ok(expr) => Statement::Expression(expr),
					Err(ParseError::ExpectedExpressionButFound(_)) => return Err(err),
					Err(e) => return Err(e),
				}
			} else {
				return Err(err);
			}
		}
		// { ... }
		TokenKind::Operator(Operator::LBrace) => Statement::Block(parse_block(iter)?),
		_ => {
			if check_expr {
				// Try to interpret the content as an expression.
				match parse_expr(iter, false) {
					Ok(expr) => Statement::Expression(expr),
					Err(ParseError::ExpectedExpressionButFound(_)) => {
						return Err(ParseError::ExpectedStatementButFound(Some(
							t1.span().clone(),
						)))
					}
					Err(e) => return Err(e),
				}
			} else {
				return Err(ParseError::ExpectedStatementButFound(Some(
					t1.span().clone(),
				)));
			}
		}
	})
}

/// Parse a block of code i.e. `{ ... }`.
pub fn parse_block<'a>(iter: &mut TokenIter<'a>) -> Result<Block<NotAnalyzed>, ParseError> {
	// Get the first token.
	let t1 = match iter.current_token() {
		Some(value) => value,
		None => return Err(ParseError::ExpectedBlockButFound(None)),
	};
	// Make sure the first token is an opening `{`.
	if let TokenKind::Operator(Operator::LBrace) = t1.kind() {
		iter.next();
	} else {
		return Err(ParseError::ExpectedBlockButFound(Some(t1.span().clone())));
	}
	let mut stmts = vec![];
	let mut first = true;
	loop {
		// Check if there's a closing `}`.
		if let Some(TokenKind::Operator(Operator::RBrace)) = iter.current_token_kind() {
			let span = Span::combine_span(t1.span(), iter.current_token_span().unwrap());
			iter.next();
			// The final statement, if one exists, must not have been followed
			// by a `;` to reach this point. Thus, if at least
			// one statement was parsed, then this statement's return value
			// will be the value the entire block will get evaluated to.
			let does_eval = stmts.len() != 0;
			break Ok(Block::new(stmts, does_eval, span));
		}
		if first {
			first = false;
		} else {
			// Check whether the previously parsed statement
			// was one of the exceptions for needing to have a
			// ending semicolon.
			let exception = match stmts.last().unwrap() {
				Statement::Block(_) => true,
				Statement::If(_) => true,
				Statement::Loop(_) => true,
				_ => false,
			};
			// If not...
			if !exception {
				// If it's not the first iteration and the previously parsed statement
				// was not immediately followed by a closing `}` then it must be followed
				// by a `;`.
				iter.consume_token(vec![TokenKind::Operator(Operator::SemiColon)])?;
				// There is a chance that a statement termination is followed by
				// a closing `}` so take care of that possibility.
				if let Some(TokenKind::Operator(Operator::RBrace)) = iter.current_token_kind() {
					let span = Span::combine_span(t1.span(), iter.current_token_span().unwrap());
					iter.next();
					// Since the final statement was followed by a closing `;` then the
					// block will not be evaluated to any value.
					break Ok(Block::new(stmts, false, span));
				}
			} else {
				// There is still a chance that one of the exceptional statements
				// is followed by a closing `}`.
				if let Some(TokenKind::Operator(Operator::RBrace)) = iter.current_token_kind() {
					let span = Span::combine_span(t1.span(), iter.current_token_span().unwrap());
					iter.next();
					// In this case it will evaluate to the value of the last
					// statement.
					break Ok(Block::new(stmts, true, span));
				}
			}
		}
		// Extract a statement.
		stmts.push(parse_stmt(iter, true)?);
	}
}
