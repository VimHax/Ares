use analyzer::ast::{Function, Node, NotAnalyzed, TopLevel};
use ares_core::{Keyword, Literal, Operator, Span, TokenKind};

use super::{block::parse_block, dt::parse_dt, token_iter::TokenIter, ParseError};

/// Parse top level components.
/// eg: Functions, Structs, Imports etc.
pub fn parse_toplevel<'a>(iter: &mut TokenIter<'a>) -> Result<TopLevel<NotAnalyzed>, ParseError> {
	let t1 = match iter.current_token() {
		Some(value) => value,
		None => return Err(ParseError::ExpectedTopLevelButFound(None)),
	};
	Ok(match t1.kind() {
		// fn name(parameter: dt, ...) -> return_dt { ... }
		TokenKind::Keyword(Keyword::Fn) => {
			iter.next();
			// Get the function name.
			let name = match iter
				.consume_token(vec![TokenKind::Literal(Literal::Identifier(
					"".to_string(),
				))])?
				.kind()
			{
				TokenKind::Literal(Literal::Identifier(name)) => name.clone(),
				_ => unreachable!(),
			};
			// Make sure the name is followed by a `(`.
			iter.consume_token(vec![TokenKind::Operator(Operator::LParen)])?;
			let mut parameters = vec![];
			let mut first = true;
			let mut signature_end;
			loop {
				// Check if we have reached the end of the parameters, indicated by `)`.
				if let Some(TokenKind::Operator(Operator::RParen)) = iter.current_token_kind() {
					signature_end = iter.current_token_span().cloned().unwrap();
					iter.next();
					break;
				}
				if first {
					first = false;
				} else {
					// If this is not the first parameter a comma must precede the next parameter.
					iter.consume_token(vec![TokenKind::Operator(Operator::Comma)])?;
				}
				// Get the name of the parameter.
				let name = match iter
					.consume_token(vec![TokenKind::Literal(Literal::Identifier(
						"".to_string(),
					))])?
					.kind()
				{
					TokenKind::Literal(Literal::Identifier(name)) => name.clone(),
					_ => unreachable!(),
				};
				// Make sure the name is followed by a `:`.
				iter.consume_token(vec![TokenKind::Operator(Operator::Colon)])?;
				// Extract the parameter data type.
				let dt = parse_dt(iter)?;
				parameters.push((name, dt));
			}
			// Extract the return type if it is provided.
			let return_dt =
				if let Some(TokenKind::Operator(Operator::MinusGt)) = iter.current_token_kind() {
					iter.next();
					let dt = parse_dt(iter)?;
					signature_end = dt.span().clone();
					Some(dt)
				} else {
					None
				};
			// Extract the function body.
			let body = parse_block(iter)?;
			let span = Span::combine_span(t1.span(), body.span());
			let signature_span = Span::combine_span(t1.span(), &signature_end);
			TopLevel::Function(Function::new(
				name,
				parameters,
				return_dt,
				body,
				span,
				signature_span,
			))
		}
		_ => {
			return Err(ParseError::ExpectedTopLevelButFound(Some(
				t1.span().clone(),
			)))
		}
	})
}
