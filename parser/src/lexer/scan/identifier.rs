use ares_core::{PartialSpan, Span};

use crate::lexer::{char_iter::CharIter, ScanError};

/// Consumes an identifier at the current position.
/// An error is returned if one is encountered.
/// **This function assumes, and relies upon,
/// the fact that the starting character is an alphabetic
/// character.**
pub fn consume<'a>(iter: &mut CharIter<'a>) -> Result<(String, Span), ScanError> {
	let mut span = PartialSpan::new(iter.current_idx());

	// Go through all the characters.
	while let Some(_) = iter.current_char() {
		// Check if the current character is a white-space.
		if iter.is_whitespace().unwrap() {
			break;
		}
		// Check if the current character is alpha-numeric.
		if !iter.is_alpha_numeric().unwrap() {
			break;
		}
		span.grow();
		iter.next();
	}

	let span = span.build();
	Ok((iter.source().span_content(&span).to_string(), span))
}
