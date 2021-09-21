use std::ops::Range;

use super::{AresError, ErrorCode};
use codespan_reporting::diagnostic::{Label, LabelStyle};
use parser::ScanError;

impl AresError for ScanError {
	fn message(&self) -> String {
		match self {
			ScanError::NumberOutOfRange(_) => "Number literal is too large or too small.",
			ScanError::NoMatchingQuotes(_) => "Found an unterminated string literal.",
			ScanError::UnrecognizedCharacter(_) => "Found a meaningless character.",
		}
		.to_string()
	}

	fn code(&self) -> ErrorCode {
		match self {
			ScanError::NumberOutOfRange(_) => ErrorCode::NUMBER_OUT_OF_RANGE,
			ScanError::NoMatchingQuotes(_) => ErrorCode::INVALID_STRING,
			ScanError::UnrecognizedCharacter(_) => ErrorCode::INVALID_SYNTAX,
		}
	}

	fn labels(&self, _: Range<usize>) -> Vec<Label<()>> {
		vec![Label::new(
			LabelStyle::Primary,
			(),
			match self {
				ScanError::NumberOutOfRange(s) => s.range(),
				ScanError::NoMatchingQuotes(s) => s.range(),
				ScanError::UnrecognizedCharacter(c) => c.range(),
			},
		)]
	}

	fn notes(&self) -> Option<Vec<String>> {
		None
	}
}
