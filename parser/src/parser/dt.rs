use analyzer::ast::*;
use ares_core::{Literal, Operator, Token, TokenKind};

use super::{token_iter::TokenIter, ParseError};

/// Parse a data type.
/// eg: `Int`, `Boolean`, `Array<Int>` etc.
pub fn parse_dt<'a>(iter: &mut TokenIter<'a>) -> Result<DT<NotAnalyzed>, ParseError> {
	let t1 = match iter.current_token() {
		Some(value) => value,
		None => return Err(ParseError::ExpectedDataTypeButFound(None)),
	};
	Ok(DT::new(match t1.kind() {
		TokenKind::Literal(Literal::Identifier(name)) => {
			iter.next();
			match name.as_str() {
				"Void" => DTKind::Void(t1.span().clone()),       // Void
				"Int" => DTKind::Int(t1.span().clone()),         // Int
				"Float" => DTKind::Float(t1.span().clone()),     // Float
				"String" => DTKind::String(t1.span().clone()),   // String
				"Boolean" => DTKind::Boolean(t1.span().clone()), // Boolean
				// Array<...>
				"Array" => {
					// Make sure the `Array` is followed by a `<`.
					iter.consume_token(vec![TokenKind::Operator(Operator::Lt)])?;
					// Parse the inner data type.
					let inner = parse_dt(iter)?;
					// Make sure the data type is followed by an ending `>`.
					let t2 = iter.consume_token(vec![TokenKind::Operator(Operator::Gt)])?;
					let span = Token::combine_span(t1, t2);
					DTKind::Array(Box::new(inner), span)
				}
				_ => {
					return Err(ParseError::ExpectedDataTypeButFound(Some(
						t1.span().clone(),
					)))
				}
			}
		}
		_ => {
			return Err(ParseError::ExpectedDataTypeButFound(Some(
				t1.span().clone(),
			)))
		}
	}))
}
