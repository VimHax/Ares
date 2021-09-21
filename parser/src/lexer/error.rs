use ares_core::Span;

use super::char_iter::Cursor;

/// Errors which can occur during lexing.
#[derive(Debug)]
pub enum ScanError {
	/// An integer literal cannot be represented by an i64
	/// or if a float literal cannot be represented by an f64.
	NumberOutOfRange(Span),
	/// A string literal was not terminated by a double quote.
	NoMatchingQuotes(Span),
	/// A character which cannot be tokenized.
	UnrecognizedCharacter(Cursor),
}
