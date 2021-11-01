use ares_core::{PartialSpan, Span};

use crate::lexer::{char_iter::CharIter, ScanError};

/// Consumes a string at the current position.
/// An error is returned if one is encountered.
/// **This function assumes, and relies upon,
/// the fact that the starting character is a double quote.**
pub fn consume<'a>(iter: &mut CharIter<'a>) -> Result<(String, Span), ScanError> {
	let mut span = PartialSpan::new(iter.current_idx());
	let (mut backslash, mut end, mut s) = (false, false, String::new());

	// Skip the initial quotes.
	iter.next();
	span.grow();

	// Go through all the characters.
	while let Some(c) = iter.current_char() {
		iter.next();
		span.grow();

		// If backslash is `true` then the preceding
		// character was a `\`.
		// eg: "escaping \" quotes"
		//                ^
		if backslash {
			backslash = false;
		}
		// If a `"` is found then this must be the
		// end of the string.
		// eg: "escaping \" quotes"
		//                        ^
		// (It can't be the first quotes as we skip those
		// right away)
		else if c == '"' {
			end = true;
			break;
		}
		// If a `\` is found set `backslash` to `true`.
		// Setting `backslash` to `true` will skip the
		// string termination check for the very next
		// character.
		// eg: "escaping \" quotes"
		//               ^
		else if c == '\\' {
			backslash = true;
			continue;
		}

		s += c.to_string().as_str();
	}

	if !end {
		return Err(ScanError::NoMatchingQuotes(span.build()));
	}

	Ok((s, span.build()))
}

#[test]
fn test_string() {
	use ares_core::Source;

	let contents = "xyz \"æBc_ \\\"'123 \" abc".to_string();
	let source = Source::from_string(0, "".to_string(), contents);
	let mut iter = CharIter::new(&source);
	(0..4).for_each(|_| {
		iter.next();
	});

	match consume(&mut iter) {
		Ok((str, span)) => {
			assert_eq!(str, "æBc_ \"'123 ");
			assert_eq!(source.span_content(&span), "\"æBc_ \\\"'123 \"");
		}
		Err(_) => unreachable!(),
	}
}
