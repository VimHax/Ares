use ares_core::Span;

use super::ty::Ty;

/// Errors that can occur during static type analysis.
#[derive(Debug)]
pub enum TypeError {
	/// A data type mismatch.
	Contradiction(Vec<Ty>),
	/// A data type which satisfies all the constraints could not be found.
	Unsolvable(Span),
	/// A data type could not be found due to insufficient information.
	Ambiguous(Ty),
	/// An undeclared variable was mentioned.
	UnresolvedVariable(Span),
	/// Found unreachable code.
	DeadCode(Span),
	/// Found a break statement outside of a loop.
	BreakOutsideLoop(Span),
	/// Found a non-void function which does not always return a value.
	ReturnRequired(Span, Span),
	/// Found a non-void function which can get caught in an infinite loop.
	InfiniteLoop(Span, Span),
	/// A function with that name already exists.
	AlreadyExists(Span, Span),
	/// No main function found.
	NoMain,
}
