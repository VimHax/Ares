use ares_core::{PartialSpan, Span};

use crate::lexer::char_iter::CharIter;

/// Consumes an identifier at the current position.
/// An error is returned if one is encountered.
/// **This function assumes, and relies upon,
/// the fact that the starting character is an alphabetic
/// character.**
pub fn consume<'a>(iter: &mut CharIter<'a>) -> (String, Span) {
	let mut span = PartialSpan::new(iter.current_idx());

	// Go through all the characters.
	while let Some(_) = iter.current_char() {
		// Check if the current character is a white-space.
		// If so then this is the end of the current identifier.
		// eg: var1 + var2, loop {
		//         ^            ^
		if iter.is_whitespace().unwrap() {
			break;
		}
		// Check if the current character is alpha-numeric.
		// If not then this is the end of the current identifier.
		// eg: var1.a_method(), return;
		//         ^                  ^
		if !iter.is_alpha_numeric().unwrap() {
			break;
		}
		span.grow();
		iter.next();
	}

	let span = span.build();
	(iter.source().span_content(&span).to_string(), span)
}

#[test]
fn test_identifier() {
	use ares_core::Source;

	let contents = "xyz æBc_123 abc".to_string();
	let source = Source::from_string(0, "".to_string(), contents);
	let mut iter = CharIter::new(&source);
	(0..4).for_each(|_| {
		iter.next();
	});

	let (str, span) = consume(&mut iter);
	assert_eq!(str, "æBc_123");
	assert_eq!(source.span_content(&span), "æBc_123");
}
