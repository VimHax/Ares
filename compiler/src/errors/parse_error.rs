use std::ops::Range;

use super::{AresError, ErrorCode};
use codespan_reporting::diagnostic::{Label, LabelStyle};
use parser::ParseError;

impl AresError for ParseError {
	fn message(&self) -> String {
		match self {
			ParseError::ExpectedButFound(v, _) => format!("Expected {}.", {
				assert_ne!(v.len(), 0);
				let mut s = v.first().unwrap().to_string();
				for (idx, k) in v.iter().skip(1).enumerate() {
					s += if idx == v.len() - 2 { " and " } else { ", " };
					s += k.to_string().as_str();
				}
				s
			}),
			ParseError::ExpectedDataTypeButFound(_) => "Expected a data type.".to_string(),
			ParseError::ExpectedBlockButFound(_) => "Expected a code block.".to_string(),
			ParseError::ExpectedExpressionButFound(_) => "Expected an expression.".to_string(),
			ParseError::ExpectedOperandButFound(_) => "Expected a operand.".to_string(),
			ParseError::ExpectedStatementButFound(_) => "Expected a statement.".to_string(),
			ParseError::ExpectedTopLevelButFound(_) => {
				"Expected a top-level component.".to_string()
			}
		}
	}

	fn code(&self) -> ErrorCode {
		match self {
			ParseError::ExpectedButFound(_, _) => ErrorCode::UNEXPECTED_TOKEN,
			ParseError::ExpectedDataTypeButFound(_) => ErrorCode::INVALID_DATATYPE,
			ParseError::ExpectedBlockButFound(_) => ErrorCode::INVALID_BLOCK,
			ParseError::ExpectedExpressionButFound(_) => ErrorCode::INVALID_EXPRESSION,
			ParseError::ExpectedOperandButFound(_) => ErrorCode::INVALID_OPERAND,
			ParseError::ExpectedStatementButFound(_) => ErrorCode::INVALID_STATEMENT,
			ParseError::ExpectedTopLevelButFound(_) => ErrorCode::INVALID_TOPLEVEL,
		}
	}

	fn labels(&self, file_end: Range<usize>) -> Vec<Label<()>> {
		vec![Label::new(
			LabelStyle::Primary,
			(),
			match self {
				ParseError::ExpectedButFound(_, Some(s)) => s.range(),
				ParseError::ExpectedDataTypeButFound(Some(s)) => s.range(),
				ParseError::ExpectedBlockButFound(Some(s)) => s.range(),
				ParseError::ExpectedExpressionButFound(Some(s)) => s.range(),
				ParseError::ExpectedOperandButFound(Some(s)) => s.range(),
				ParseError::ExpectedStatementButFound(Some(s)) => s.range(),
				ParseError::ExpectedTopLevelButFound(Some(s)) => s.range(),
				_ => file_end,
			},
		)
		.with_message("the erroneous code")]
	}

	fn notes(&self) -> Option<Vec<String>> {
		None
	}
}
