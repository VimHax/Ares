use std::{iter::Peekable, ops::Range, str::Chars};

use ares_core::Source;

/// A single point in the source.
#[derive(Debug)]
pub struct Cursor(usize);

impl Cursor {
	fn new(idx: usize) -> Self {
		Self(idx)
	}

	pub fn idx(&self) -> usize {
		self.0
	}

	pub fn range(&self) -> Range<usize> {
		self.0..self.0 + 1
	}
}

/// Iterates over all the characters in a source.
/// For the most part a simple wrapper over `Chars<'a>`.
pub struct CharIter<'a> {
	idx: usize,
	curr: Option<char>,
	iter: Peekable<Chars<'a>>,
	source: &'a Source,
}

impl<'a> CharIter<'a> {
	pub fn new(source: &'a Source) -> Self {
		let code = source.contents();
		let mut iter = code.chars().peekable();

		Self {
			idx: 0,
			curr: iter.next(),
			iter,
			source,
		}
	}

	pub fn current_idx(&self) -> usize {
		self.idx
	}

	pub fn current_char(&self) -> Option<char> {
		self.curr
	}

	pub fn source(&self) -> &'a Source {
		self.source
	}

	/// Gets the value of the very next character without consuming the
	/// current character.
	pub fn peek_char(&mut self) -> Option<char> {
		self.iter.peek().cloned()
	}

	/// Constructs a cursor at the current point in the iterator.
	pub fn cursor(&self) -> Cursor {
		Cursor::new(self.current_idx())
	}

	/// Checks whether the current character is a white-space.
	/// (White-space according to the `core::char::methods::is_whitespace` method)
	pub fn is_whitespace(&self) -> Option<bool> {
		Some(self.current_char()?.is_whitespace())
	}

	/// Checks whether the current character is `0` to `9`.
	pub fn is_numeric(&self) -> Option<bool> {
		let c = self.current_char()?;
		Some(c >= '0' && c <= '9')
	}

	/// Checks whether the current character is
	/// unicode uppercase, lowercase or a `_`.
	pub fn is_alpha(&self) -> Option<bool> {
		let c = self.current_char()?;
		Some(c.is_uppercase() || c.is_lowercase() || c == '_')
	}

	/// Checks whether the current character is
	/// unicode uppercase, lowercase, numeric or a `_`.
	pub fn is_alpha_numeric(&self) -> Option<bool> {
		let c = self.current_char()?;
		Some(c.is_uppercase() || c.is_lowercase() || c.is_numeric() || c == '_')
	}
}

impl<'a> Iterator for CharIter<'a> {
	type Item = char;

	fn next(&mut self) -> Option<Self::Item> {
		self.curr = self.iter.next();
		if self.curr.is_none() {
			return None;
		}
		self.idx += 1;
		Some(self.curr.unwrap())
	}
}
