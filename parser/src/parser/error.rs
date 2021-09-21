use ares_core::{Span, TokenKind};

/// Errors that can occur during parsing.
#[derive(Debug)]
pub enum ParseError {
	/// Expected a certain type of token but failed to find it.
	ExpectedButFound(Vec<TokenKind>, Option<Span>),
	/// Expected a data type but failed to find it.
	ExpectedDataTypeButFound(Option<Span>),
	/// Expected a block but failed to find it.
	ExpectedBlockButFound(Option<Span>),
	/// Expected an expression but failed to find it.
	ExpectedExpressionButFound(Option<Span>),
	/// Expected an operand but failed to find it.
	ExpectedOperandButFound(Option<Span>),
	/// Expected a statement but failed to find it.
	ExpectedStatementButFound(Option<Span>),
	/// Expected a top-level component but failed to find it.
	ExpectedTopLevelButFound(Option<Span>),
}
