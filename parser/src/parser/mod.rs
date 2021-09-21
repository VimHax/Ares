use analyzer::ast::{NotAnalyzed, AST};
use ares_core::Token;

mod block;
mod dt;
mod error;
mod expr;
mod token_iter;
mod toplevel;

pub use error::ParseError;
use token_iter::TokenIter;
use toplevel::parse_toplevel;

/// Generates an AST from the provided tokens and source.
pub fn parse<'a>(tokens: &'a Vec<Token>) -> Result<AST<NotAnalyzed>, ParseError> {
	let mut iter = TokenIter::new(tokens);
	let mut ast = vec![];
	while let Some(_) = iter.current_token() {
		ast.push(parse_toplevel(&mut iter)?);
	}
	Ok(ast)
}
