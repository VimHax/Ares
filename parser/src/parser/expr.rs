use analyzer::ast::*;
use ares_core::{Literal as RawLiteral, Operator, Span, Token, TokenKind};

use crate::parser::{block::parse_stmt, token_iter::TokenIter, ParseError};

/// Parse indexing, property access, method and function calls.
fn parse_accessors<'a>(
	expr: Expression<NotAnalyzed>,
	iter: &mut TokenIter<'a>,
) -> Result<Expression<NotAnalyzed>, ParseError> {
	let expr_span = expr.span().clone();
	let mut rvalue = expr;
	loop {
		// Get the first token.
		let t1 = match iter.current_token() {
			Some(value) => value,
			None => return Ok(rvalue),
		};
		match t1.operator() {
			// expr[index]
			Some(Operator::LBracket) => {
				iter.next();
				// Extract the index expression.
				let index = parse_expr(iter, true)?;
				// Make sure the index is immediately followed by a closing `]`.
				let t2 = iter.consume_token(vec![TokenKind::Operator(Operator::RBracket)])?;
				rvalue = Expression::IndexOf(IndexOf::new(
					rvalue,
					index,
					Span::combine_span(&expr_span, t2.span()),
				));
			}
			// expr.name or expr.name(arg1, arg2, ...)
			Some(Operator::Dot) => {
				iter.next();
				// Get the property/method name.
				let t2 = iter.consume_token(vec![TokenKind::Literal(RawLiteral::Identifier(
					"".to_string(),
				))])?;
				let name = match t2.kind() {
					TokenKind::Literal(RawLiteral::Identifier(name)) => name.clone(),
					_ => unreachable!(),
				};
				// If the name is followed by an opening `(` then this is a method call.
				if let Some(TokenKind::Operator(Operator::LParen)) = iter.current_token_kind() {
					iter.next();
					let mut args = vec![];
					let mut first = true;
					loop {
						// Check if we have reached the end of the arguments, indicated by `)`.
						if let Some(TokenKind::Operator(Operator::RParen)) =
							iter.current_token_kind()
						{
							let span = Span::combine_span(
								&expr_span,
								iter.current_token().unwrap().span(),
							);
							iter.next();
							rvalue =
								Expression::NamedCall(NamedCall::new(rvalue, name, args, span));
							break;
						}
						if first {
							first = false;
						} else {
							// If this is not the first argument a comma must precede the next argument.
							iter.consume_token(vec![TokenKind::Operator(Operator::Comma)])?;
						}
						// Extract the argument.
						args.push(parse_expr(iter, true)?);
					}
					// At this point the named call has been built.
					continue;
				}
				// Build a property access.
				rvalue = Expression::PropertyOf(PropertyOf::new(
					rvalue,
					name,
					Span::combine_span(&expr_span, t2.span()),
				));
			}
			// expr(arg1, arg2, ...)
			Some(Operator::LParen) => {
				iter.next();
				let mut args = vec![];
				let mut first = true;
				loop {
					// Check if we have reached the end of the arguments, indicated by `)`.
					if let Some(TokenKind::Operator(Operator::RParen)) = iter.current_token_kind() {
						let span =
							Span::combine_span(&expr_span, iter.current_token().unwrap().span());
						iter.next();
						rvalue = Expression::Call(Call::new(rvalue, args, span));
						break;
					}
					if first {
						first = false;
					} else {
						// If this is not the first argument a comma must precede the next argument.
						iter.consume_token(vec![TokenKind::Operator(Operator::Comma)])?;
					}
					// Extract the argument.
					args.push(parse_expr(iter, true)?);
				}
				// At this point the call has been built.
				continue;
			}
			_ => break Ok(rvalue),
		}
		return Ok(rvalue);
	}
}

/// Parse `(...)`, `[elem1, elem2, ...]` and literals.
fn primary<'a>(
	iter: &mut TokenIter<'a>,
	check_stmt: bool,
) -> Result<Expression<NotAnalyzed>, ParseError> {
	// Get the first token.
	let t1 = match iter.current_token() {
		Some(value) => value,
		None => return Err(ParseError::ExpectedExpressionButFound(None)),
	};

	match t1.kind() {
		TokenKind::Operator(op) => match op {
			// (...)
			Operator::LParen => {
				iter.next();
				// Extract the inner expression.
				let expr = parse_expr(iter, true)?;
				// Make sure the expression is followed by a closing `)`.
				let t2 = iter.consume_token(vec![TokenKind::Operator(Operator::RParen)])?;
				let parens =
					Expression::Parenthesis(Parenthesis::new(expr, Token::combine_span(t1, t2)));
				parse_accessors(parens, iter)
			}
			// [elem1, elem2, ...]
			Operator::LBracket => {
				iter.next();
				let mut exprs = vec![];
				let mut first = true;
				loop {
					// Check if we have reached the end of the elements, indicated by `]`.
					if let Some(TokenKind::Operator(Operator::RBracket)) = iter.current_token_kind()
					{
						let span = Token::combine_span(t1, iter.current_token().unwrap());
						iter.next();
						let arr = Expression::Array(Array::new(exprs, span));
						break parse_accessors(arr, iter);
					}
					if first {
						first = false;
					} else {
						// If this is not the first element a comma must precede the next element.
						iter.consume_token(vec![TokenKind::Operator(Operator::Comma)])?;
					}
					// Extract the element.
					exprs.push(parse_expr(iter, true)?);
				}
			}
			_ => {
				// If check_stmt is true then try to parse a statement.
				if check_stmt {
					match parse_stmt(iter, false) {
						Ok(value) => Ok(Expression::Statement(Box::new(value))),
						Err(ParseError::ExpectedStatementButFound(_)) => Err(
							ParseError::ExpectedExpressionButFound(Some(t1.span().clone())),
						),
						Err(err) => Err(err),
					}
				} else {
					Err(ParseError::ExpectedExpressionButFound(Some(
						t1.span().clone(),
					)))
				}
			}
		},
		// Literals
		TokenKind::Literal(literal) => {
			// Start winding.
			iter.wind();
			iter.next();
			// Convert the literal to an expression.
			let literal = match literal {
				// Identifiers.
				RawLiteral::Identifier(ident) => {
					Expression::Variable(Variable::new(ident.clone(), t1.span().clone()))
				}
				// 1, 3, 553 etc.
				RawLiteral::Int(int) => {
					Expression::Literal(Literal::new(LiteralValue::Int(*int), t1.span().clone()))
				}
				// 1.2, 4.2, 5.0 etc.
				RawLiteral::Float(float) => Expression::Literal(Literal::new(
					LiteralValue::Float(*float),
					t1.span().clone(),
				)),
				// true or false
				RawLiteral::Boolean(boolean) => Expression::Literal(Literal::new(
					LiteralValue::Boolean(*boolean),
					t1.span().clone(),
				)),
				// "", "Hello World!", etc.
				RawLiteral::String(string) => Expression::Literal(Literal::new(
					LiteralValue::String(string.clone()),
					t1.span().clone(),
				)),
			};
			if check_stmt {
				// This literal may be the beginning of a statement.
				// Specifically the beginning of an assignment statement.
				match parse_accessors(literal, iter) {
					Ok(e) => Ok(
						// If the accessors are followed by a `=` chances are
						// this is an assignment statement attempted to be parsed.
						if let Some(TokenKind::Operator(Operator::Eq)) = iter.current_token_kind() {
							// Unwind to the original winding position
							// so that the prior tokens can be reinterpreted into
							// a statement.
							iter.unwind();
							Expression::Statement(Box::new(parse_stmt(iter, false)?))
						} else {
							// Unwind in place as all the prior tokens
							// were already interpreted as an expression.
							iter.unwind_inplace();
							e
						},
					),
					Err(err) => {
						iter.unwind();
						match parse_stmt(iter, false) {
							Ok(value) => Ok(Expression::Statement(Box::new(value))),
							Err(ParseError::ExpectedStatementButFound(_)) => Err(err),
							Err(err) => Err(err),
						}
					}
				}
			} else {
				parse_accessors(literal, iter)
			}
		}
		TokenKind::Keyword(_) => {
			// If check_stmt is true then try to parse a statement.
			if check_stmt {
				match parse_stmt(iter, false) {
					Ok(value) => Ok(Expression::Statement(Box::new(value))),
					Err(ParseError::ExpectedStatementButFound(_)) => Err(
						ParseError::ExpectedExpressionButFound(Some(t1.span().clone())),
					),
					Err(err) => Err(err),
				}
			} else {
				Err(ParseError::ExpectedExpressionButFound(Some(
					t1.span().clone(),
				)))
			}
		}
	}
}

/// Parse `-` (unary) and `not`.
fn unary<'a>(
	iter: &mut TokenIter<'a>,
	check_stmt: bool,
) -> Result<Expression<NotAnalyzed>, ParseError> {
	let t = match iter.current_token() {
		Some(value) => value,
		None => return Err(ParseError::ExpectedExpressionButFound(None)),
	};
	// -<op>
	if let Some(Operator::Minus) = t.operator() {
		iter.next();
		// Get the operand.
		let expr = primary(iter, check_stmt)?;
		let span = expr.span().clone();
		return Ok(Expression::Unary(Unary::new(
			UnaryOperator::Minus,
			expr,
			Span::combine_span(t.span(), &span),
		)));
	}
	// not <op>
	if let Some(Operator::Not) = t.operator() {
		iter.next();
		// Get the operand.
		let expr = primary(iter, check_stmt)?;
		let span = expr.span().clone();
		return Ok(Expression::Unary(Unary::new(
			UnaryOperator::Not,
			expr,
			Span::combine_span(t.span(), &span),
		)));
	}
	primary(iter, check_stmt)
}

/// Parse `^`.
fn exponentiation<'a>(
	iter: &mut TokenIter<'a>,
	check_stmt: bool,
) -> Result<Expression<NotAnalyzed>, ParseError> {
	// Get the first operand.
	let op1 = unary(iter, check_stmt)?;
	let t = match iter.current_token() {
		Some(value) => value,
		None => return Ok(op1),
	};
	// <op1> ^ <op2>
	if let Some(Operator::Caret) = t.operator() {
		iter.next();
		// Get the second operand.
		let op2 = exponentiation(iter, check_stmt).map_err(|err| match err {
			ParseError::ExpectedExpressionButFound(s) => ParseError::ExpectedOperandButFound(s),
			e => e,
		})?;
		let span = Span::combine_span(op1.span(), op2.span());
		return Ok(Expression::Binary(Binary::new(
			BinaryOperator::Exponentiate,
			(op1, op2),
			span,
		)));
	}
	Ok(op1)
}

/// Parse `/`, `*` and `mod`.
fn multiplication<'a>(
	iter: &mut TokenIter<'a>,
	check_stmt: bool,
) -> Result<Expression<NotAnalyzed>, ParseError> {
	// Get the first operand.
	let op1 = exponentiation(iter, check_stmt)?;
	let t = match iter.current_token() {
		Some(value) => value,
		None => return Ok(op1),
	};
	// <op1> / <op2>
	if let Some(Operator::Slash) = t.operator() {
		iter.next();
		// Get the second operand.
		let op2 = multiplication(iter, check_stmt).map_err(|err| match err {
			ParseError::ExpectedExpressionButFound(s) => ParseError::ExpectedOperandButFound(s),
			e => e,
		})?;
		let span = Span::combine_span(op1.span(), op2.span());
		return Ok(Expression::Binary(Binary::new(
			BinaryOperator::Divide,
			(op1, op2),
			span,
		)));
	}
	// <op1> * <op2>
	if let Some(Operator::Star) = t.operator() {
		iter.next();
		// Get the second operand.
		let op2 = multiplication(iter, check_stmt).map_err(|err| match err {
			ParseError::ExpectedExpressionButFound(s) => ParseError::ExpectedOperandButFound(s),
			e => e,
		})?;
		let span = Span::combine_span(op1.span(), op2.span());
		return Ok(Expression::Binary(Binary::new(
			BinaryOperator::Multiply,
			(op1, op2),
			span,
		)));
	}
	// <op1> mod <op2>
	if let Some(Operator::Mod) = t.operator() {
		iter.next();
		// Get the second operand.
		let op2 = multiplication(iter, check_stmt).map_err(|err| match err {
			ParseError::ExpectedExpressionButFound(s) => ParseError::ExpectedOperandButFound(s),
			e => e,
		})?;
		let span = Span::combine_span(op1.span(), op2.span());
		return Ok(Expression::Binary(Binary::new(
			BinaryOperator::Mod,
			(op1, op2),
			span,
		)));
	}
	Ok(op1)
}

/// Parse `+` and `-`.
fn addition<'a>(
	iter: &mut TokenIter<'a>,
	check_stmt: bool,
) -> Result<Expression<NotAnalyzed>, ParseError> {
	// Get the first operand.
	let op1 = multiplication(iter, check_stmt)?;
	let t = match iter.current_token() {
		Some(value) => value,
		None => return Ok(op1),
	};
	// <op1> + <op2>
	if let Some(Operator::Plus) = t.operator() {
		iter.next();
		// Get the second operand.
		let op2 = addition(iter, check_stmt).map_err(|err| match err {
			ParseError::ExpectedExpressionButFound(s) => ParseError::ExpectedOperandButFound(s),
			e => e,
		})?;
		let span = Span::combine_span(op1.span(), op2.span());
		return Ok(Expression::Binary(Binary::new(
			BinaryOperator::Add,
			(op1, op2),
			span,
		)));
	}
	// <op1> - <op2>
	if let Some(Operator::Minus) = t.operator() {
		iter.next();
		// Get the second operand.
		let op2 = addition(iter, check_stmt).map_err(|err| match err {
			ParseError::ExpectedExpressionButFound(s) => ParseError::ExpectedOperandButFound(s),
			e => e,
		})?;
		let span = Span::combine_span(op1.span(), op2.span());
		return Ok(Expression::Binary(Binary::new(
			BinaryOperator::Subtract,
			(op1, op2),
			span,
		)));
	}
	Ok(op1)
}

/// Parse `<`, `<=`, `>` and `>=`.
fn comparison<'a>(
	iter: &mut TokenIter<'a>,
	check_stmt: bool,
) -> Result<Expression<NotAnalyzed>, ParseError> {
	// Get the first operand.
	let op1 = addition(iter, check_stmt)?;
	let t = match iter.current_token() {
		Some(value) => value,
		None => return Ok(op1),
	};
	// <op1> < <op2>
	if let Some(Operator::Lt) = t.operator() {
		iter.next();
		// Get the second operand.
		let op2 = addition(iter, check_stmt).map_err(|err| match err {
			ParseError::ExpectedExpressionButFound(s) => ParseError::ExpectedOperandButFound(s),
			e => e,
		})?;
		let span = Span::combine_span(op1.span(), op2.span());
		return Ok(Expression::Binary(Binary::new(
			BinaryOperator::LessThan,
			(op1, op2),
			span,
		)));
	}
	// <op1> <= <op2>
	if let Some(Operator::LtEq) = t.operator() {
		iter.next();
		// Get the second operand.
		let op2 = addition(iter, check_stmt).map_err(|err| match err {
			ParseError::ExpectedExpressionButFound(s) => ParseError::ExpectedOperandButFound(s),
			e => e,
		})?;
		let span = Span::combine_span(op1.span(), op2.span());
		return Ok(Expression::Binary(Binary::new(
			BinaryOperator::LessThanOrEqual,
			(op1, op2),
			span,
		)));
	}
	// <op1> > <op2>
	if let Some(Operator::Gt) = t.operator() {
		iter.next();
		// Get the second operand.
		let op2 = addition(iter, check_stmt).map_err(|err| match err {
			ParseError::ExpectedExpressionButFound(s) => ParseError::ExpectedOperandButFound(s),
			e => e,
		})?;
		let span = Span::combine_span(op1.span(), op2.span());
		return Ok(Expression::Binary(Binary::new(
			BinaryOperator::GreaterThan,
			(op1, op2),
			span,
		)));
	}
	// <op1> >= <op2>
	if let Some(Operator::GtEq) = t.operator() {
		iter.next();
		// Get the second operand.
		let op2 = addition(iter, check_stmt).map_err(|err| match err {
			ParseError::ExpectedExpressionButFound(s) => ParseError::ExpectedOperandButFound(s),
			e => e,
		})?;
		let span = Span::combine_span(op1.span(), op2.span());
		return Ok(Expression::Binary(Binary::new(
			BinaryOperator::GreaterThanOrEqual,
			(op1, op2),
			span,
		)));
	}
	Ok(op1)
}

/// Parse `==` and `!=`.
fn equality<'a>(
	iter: &mut TokenIter<'a>,
	check_stmt: bool,
) -> Result<Expression<NotAnalyzed>, ParseError> {
	// Get the first operand.
	let op1 = comparison(iter, check_stmt)?;
	let t = match iter.current_token() {
		Some(value) => value,
		None => return Ok(op1),
	};
	// <op1> == <op2>
	if let Some(Operator::EqEq) = t.operator() {
		iter.next();
		// Get the second operand.
		let op2 = comparison(iter, check_stmt).map_err(|err| match err {
			ParseError::ExpectedExpressionButFound(s) => ParseError::ExpectedOperandButFound(s),
			e => e,
		})?;
		let span = Span::combine_span(op1.span(), op2.span());
		return Ok(Expression::Binary(Binary::new(
			BinaryOperator::Equal,
			(op1, op2),
			span,
		)));
	}
	// <op1> != <op2>
	if let Some(Operator::BangEq) = t.operator() {
		iter.next();
		// Get the second operand.
		let op2 = comparison(iter, check_stmt).map_err(|err| match err {
			ParseError::ExpectedExpressionButFound(s) => ParseError::ExpectedOperandButFound(s),
			e => e,
		})?;
		let span = Span::combine_span(op1.span(), op2.span());
		return Ok(Expression::Binary(Binary::new(
			BinaryOperator::NotEqual,
			(op1, op2),
			span,
		)));
	}
	Ok(op1)
}

/// Parse `and` and `or`.
fn logical<'a>(
	iter: &mut TokenIter<'a>,
	check_stmt: bool,
) -> Result<Expression<NotAnalyzed>, ParseError> {
	// Get the first operand.
	let op1 = equality(iter, check_stmt)?;
	// Check whether an operator exists.
	let t = match iter.current_token() {
		Some(value) => value,
		None => return Ok(op1),
	};
	// <op1> and <op2>
	if let Some(Operator::And) = t.operator() {
		iter.next();
		// Get the second operand.
		let op2 = logical(iter, check_stmt).map_err(|err| match err {
			ParseError::ExpectedExpressionButFound(s) => ParseError::ExpectedOperandButFound(s),
			e => e,
		})?;
		let span = Span::combine_span(op1.span(), op2.span());
		return Ok(Expression::Binary(Binary::new(
			BinaryOperator::And,
			(op1, op2),
			span,
		)));
	}
	// <op1> or <op2>
	if let Some(Operator::Or) = t.operator() {
		iter.next();
		// Get the second operand.
		let op2 = logical(iter, check_stmt).map_err(|err| match err {
			ParseError::ExpectedExpressionButFound(s) => ParseError::ExpectedOperandButFound(s),
			e => e,
		})?;
		let span = Span::combine_span(op1.span(), op2.span());
		return Ok(Expression::Binary(Binary::new(
			BinaryOperator::Or,
			(op1, op2),
			span,
		)));
	}
	Ok(op1)
}

/// Parse an expression.
/// eg: `1`, `2 * 3`, `1 + 2 * 3 > 5` etc.
pub fn parse_expr<'a>(
	iter: &mut TokenIter<'a>,
	check_stmt: bool,
) -> Result<Expression<NotAnalyzed>, ParseError> {
	logical(iter, check_stmt)
}
