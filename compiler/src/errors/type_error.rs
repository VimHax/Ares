use std::ops::Range;

use super::{AresError, ErrorCode};
use analyzer::TypeError;
use codespan_reporting::diagnostic::{Label, LabelStyle};

impl AresError for TypeError {
	fn message(&self) -> String {
		match self {
			TypeError::Contradiction(_) => "Data type mismatch.",
			TypeError::Unsolvable(_) => {
				"Unable to find a data type which fits all the requirements."
			}
			TypeError::Ambiguous(_) => {
				"Unable to infer a data type due to insufficient information."
			}
			TypeError::UnresolvedVariable(_) => "An undeclared variable was referenced.",
			TypeError::DeadCode(_) => "Found code which is unreachable at runtime.",
			TypeError::BreakOutsideLoop(_) => "A break statement was called outside a loop.",
			TypeError::ReturnRequired(_, _) => {
				"A function required to return a value does not always return a value."
			}
			TypeError::InfiniteLoop(_, _) => {
				"A function required to return a value can get caught in an infinite loop."
			}
			TypeError::AlreadyExists(_, _) => "A function with this name already exists.",
			TypeError::NoMain => "No main function was found.",
		}
		.to_string()
	}

	fn code(&self) -> ErrorCode {
		match self {
			TypeError::Contradiction(_) => ErrorCode::CONTRADICTION,
			TypeError::Unsolvable(_) => ErrorCode::UNSOLVABLE,
			TypeError::Ambiguous(_) => ErrorCode::AMBIGUOUS,
			TypeError::UnresolvedVariable(_) => ErrorCode::UNRESOLVED_VARIABLE,
			TypeError::DeadCode(_) => ErrorCode::DEAD_CODE,
			TypeError::BreakOutsideLoop(_) => ErrorCode::BREAK_OUTSIDE_LOOP,
			TypeError::ReturnRequired(_, _) => ErrorCode::RETURN_REQUIRED,
			TypeError::InfiniteLoop(_, _) => ErrorCode::INFINITE_LOOP,
			TypeError::AlreadyExists(_, _) => ErrorCode::ALREADY_EXISTS,
			TypeError::NoMain => ErrorCode::NO_MAIN,
		}
	}

	fn labels(&self, _: Range<usize>) -> Vec<Label<()>> {
		vec![Label::new(
			LabelStyle::Primary,
			(),
			match self {
				TypeError::Contradiction(v) => {
					return v
						.iter()
						.map(|ty| {
							Label::new(LabelStyle::Primary, (), ty.span().range())
								.with_message(ty.to_string())
						})
						.collect()
				}
				TypeError::Unsolvable(s) => s.range(),
				TypeError::Ambiguous(ty) => {
					return vec![Label::new(LabelStyle::Primary, (), ty.span().range())
						.with_message(ty.to_string())]
				}
				TypeError::UnresolvedVariable(s) => s.range(),
				TypeError::DeadCode(s) => s.range(),
				TypeError::BreakOutsideLoop(s) => {
					return vec![Label::new(LabelStyle::Primary, (), s.range())
						.with_message("not within a loop")]
				}
				TypeError::ReturnRequired(ret, s) => {
					return vec![
						Label::new(LabelStyle::Primary, (), s.range())
							.with_message("does not always return"),
						Label::new(LabelStyle::Secondary, (), ret.range())
							.with_message("return type is not Void"),
					]
				}
				TypeError::InfiniteLoop(ret, s) => {
					return vec![
						Label::new(LabelStyle::Primary, (), s.range())
							.with_message("can loop indefinitely"),
						Label::new(LabelStyle::Secondary, (), ret.range())
							.with_message("return type is not Void"),
					]
				}
				TypeError::AlreadyExists(s, p) => {
					return vec![
						Label::new(LabelStyle::Primary, (), s.range())
							.with_message("erroneous declaration"),
						Label::new(LabelStyle::Secondary, (), p.range())
							.with_message("previous declaration"),
					]
				}
				TypeError::NoMain => return vec![],
			},
		)]
	}

	fn notes(&self) -> Option<Vec<String>> {
		None
	}
}
