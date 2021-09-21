use ares_core::{PartialSpan, Span};

use crate::lexer::{char_iter::CharIter, ScanError};

/// The extracted number.
/// Either an `i64` integer or `f64` float.
pub enum Number {
	Int(i64),
	Float(f64),
}

/// Consumes a number at the current position.
/// An error is returned if one is encountered.
/// **This function assumes, and relies upon,
/// the fact that the starting character is a number.**
pub fn consume<'a>(iter: &mut CharIter<'a>) -> Result<(Number, Span), ScanError> {
	let mut is_float = false;
	let mut span = PartialSpan::new(iter.current_idx());

	// Go through all the characters.
	while let Some(c) = iter.current_char() {
		// If a dot is found that could mean either this
		// number is a float or it's a method/property access.
		// eg: 123.5, 5.a_method()
		//         ^    ^
		// (If `is_float` is `true` then this dot
		// definitely cannot be a part of the number being
		// consumed)
		if c == '.' && !is_float {
			// Check the very next character
			// eg: 123.5, 5.a_method()
			//          ^    ^
			if let Some(c) = iter.peek_char() {
				// If the character after the dot is not a number
				// then it can't be a float, thus the dot
				// is not going to be consumed and the number
				// will get treated as an integer.
				if c < '0' || c > '9' {
					break;
				}
				// Otherwise it must mean that the number
				// currently being consumed is a float.
				is_float = true;
			}
			// If there is no next character then it can't
			// be a float either.
			else {
				break;
			}
		}
		// Check whether the current character is a number
		// eg: 123.5, 5.a_method()
		//          ^  ^
		else if !iter.is_numeric().unwrap() {
			break;
		}
		iter.next();
		span.grow();
	}

	// Parse the extracted string into a number.
	// Extracted string should be in the format
	// /\d+(\.\d+)?/ (regex) which should
	// be able to be parsed into an `i64` or `f64`
	// unless there is a out of range issue.
	let span = span.build();
	let content = iter.source().span_content(&span);
	if is_float {
		match content.parse::<f64>() {
			Ok(value) => Ok((Number::Float(value), span)),
			Err(_) => Err(ScanError::NumberOutOfRange(span)),
		}
	} else {
		match content.parse::<i64>() {
			Ok(value) => Ok((Number::Int(value), span)),
			Err(_) => Err(ScanError::NumberOutOfRange(span)),
		}
	}
}
