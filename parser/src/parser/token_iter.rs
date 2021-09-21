use ares_core::{Span, Token, TokenKind};

use crate::ParseError;

/// Iterates over all the tokens produced by the lexer.
/// Capable of winding/unwinding to do unlimited
/// forward peeking.
pub struct TokenIter<'a> {
	idx: usize,
	winding: Vec<usize>,
	curr: Option<&'a Token>,
	tokens: &'a Vec<Token>,
}

impl<'a> TokenIter<'a> {
	pub fn new(tokens: &'a Vec<Token>) -> Self {
		Self {
			idx: 0,
			winding: vec![],
			curr: tokens.first(),
			tokens,
		}
	}

	/// Start winding at the current position.
	/// Can be unwound to move back to this position.
	/// Capable of being wound more than once at a time.
	pub fn wind(&mut self) {
		self.winding.push(self.idx);
	}

	/// Unwind to the previous wound position.
	pub fn unwind(&mut self) {
		assert!(self.winding.len() > 0);
		self.idx = self.winding.pop().unwrap();
		self.curr = Some(&self.tokens[self.idx]);
	}

	/// Unwind without retreating to the previous
	/// location.
	pub fn unwind_inplace(&mut self) {
		assert!(self.winding.len() > 0);
		self.winding.pop().unwrap();
	}

	/// The token about to be consumed.
	/// If the token is missing `Option::None` is returned.
	pub fn current_token(&self) -> Option<&'a Token> {
		self.curr
	}

	/// The token about to be consumed.
	/// If the token is missing an error is returned.
	/// If the kind of the token doesn't match any of the provided kinds
	/// an error is also returned.
	pub fn consume_token(&mut self, kinds: Vec<TokenKind>) -> Result<&'a Token, ParseError> {
		if let Some(t) = self.curr {
			if kinds.contains(t.kind()) {
				self.next();
				return Ok(t);
			}
			return Err(ParseError::ExpectedButFound(kinds, Some(t.span().clone())));
		}
		Err(ParseError::ExpectedButFound(kinds, None))
	}

	/// The kind of token about to be consumed.
	/// If the token is missing `Option::None` is returned.
	pub fn current_token_kind(&self) -> Option<&'a TokenKind> {
		if let Some(t) = self.curr {
			Some(t.kind())
		} else {
			None
		}
	}

	/// The span of the token about to be consumed.
	pub fn current_token_span(&self) -> Option<&'a Span> {
		if let Some(t) = self.curr {
			Some(t.span())
		} else {
			None
		}
	}
}

impl<'a> Iterator for TokenIter<'a> {
	type Item = &'a Token;

	fn next(&mut self) -> Option<Self::Item> {
		self.curr = self.tokens.get(self.idx + 1);
		if self.curr.is_none() {
			return None;
		}
		self.idx += 1;
		Some(self.curr.unwrap())
	}
}
