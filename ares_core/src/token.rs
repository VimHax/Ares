use std::{fmt::Debug, ops::Range};

/// A code snippet within a source.
/// Only way a `Span` can be built is with a `PartialSpan`.
#[derive(Clone, PartialEq, Eq)]
pub struct Span(usize, usize);

impl Span {
	/// Create a new span.
	/// **Panics if the ending position is not bigger
	/// than the starting position.**
	fn new(start: usize, end: usize) -> Self {
		assert!(end > start);
		Self(start, end)
	}

	pub fn start(&self) -> usize {
		self.0
	}

	pub fn end(&self) -> usize {
		self.1
	}

	pub fn range(&self) -> Range<usize> {
		self.0..self.1
	}

	/// Takes the starting position of the first span
	/// and the ending position of the second span and constructs
	/// a new span using the 2 previously mentioned positions.
	/// **Panics if the second span's ending position is not
	/// bigger than the first span's starting position.**
	pub fn combine_span(first: &Self, second: &Self) -> Self {
		assert!(second.end() > first.start());
		Self::new(first.start(), second.end())
	}
}

impl Debug for Span {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_tuple("Span").field(&self.0).field(&self.1).finish()
	}
}

/// An incomplete span which is still being built.
pub struct PartialSpan(usize, usize);

impl PartialSpan {
	pub fn new(start: usize) -> Self {
		Self(start, start)
	}

	/// The current length of the span.
	/// (end - start)
	pub fn len(&self) -> usize {
		self.1 - self.0
	}

	/// Extend the end position by one.
	pub fn grow(&mut self) {
		self.1 += 1;
	}

	/// Build a full `Span`.
	pub fn build(self) -> Span {
		Span::new(self.0, self.1)
	}
}

/// Capable of being converted to a `Span`.
pub trait SpanLike {
	fn build(self) -> Span;
}

impl SpanLike for Range<usize> {
	fn build(self) -> Span {
		Span::new(self.start, self.end)
	}
}

impl SpanLike for Span {
	fn build(self) -> Self {
		self
	}
}

impl SpanLike for PartialSpan {
	fn build(self) -> Span {
		self.build()
	}
}

/// Keywords in Ares.
/// (Identifiers which have special meaning in
/// Ares code)
#[derive(Debug, PartialEq, Eq)]
pub enum Keyword {
	Fn,
	Let,
	Mut,
	If,
	Else,
	Loop,
	Return,
	Break,
}

impl Keyword {
	pub fn from_str(code: &str) -> Option<Keyword> {
		match code {
			"fn" => Some(Keyword::Fn),
			"let" => Some(Keyword::Let),
			"mut" => Some(Keyword::Mut),
			"if" => Some(Keyword::If),
			"else" => Some(Keyword::Else),
			"loop" => Some(Keyword::Loop),
			"return" => Some(Keyword::Return),
			"break" => Some(Keyword::Break),
			_ => None,
		}
	}

	pub fn to_string(&self) -> String {
		format!(
			"`{}`",
			match self {
				Keyword::Fn => "fn",
				Keyword::Let => "let",
				Keyword::Mut => "mut",
				Keyword::If => "if",
				Keyword::Else => "else",
				Keyword::Loop => "loop",
				Keyword::Return => "return",
				Keyword::Break => "break",
			}
		)
	}
}

/// Literals in Ares.
/// (Identifiers are also considered as literals)
#[derive(Debug)]
pub enum Literal {
	Int(i64),
	Float(f64),
	Boolean(bool),
	String(String),
	Identifier(String),
}

impl Literal {
	pub fn to_string(&self) -> String {
		String::from(match self {
			Literal::Int(_) => "an Int",
			Literal::Float(_) => "a Float",
			Literal::Boolean(_) => "a Boolean",
			Literal::String(_) => "a String",
			Literal::Identifier(_) => "an Identifier",
		})
	}
}

impl PartialEq for Literal {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Self::Int(_), Self::Int(_))
			| (Self::Float(_), Self::Float(_))
			| (Self::Boolean(_), Self::Boolean(_))
			| (Self::String(_), Self::String(_))
			| (Self::Identifier(_), Self::Identifier(_)) => true,
			_ => false,
		}
	}
}

impl<'a> Eq for Literal {}

/// Operators in Ares.
#[derive(Debug, PartialEq, Eq)]
pub enum Operator {
	/// `+`
	Plus,
	/// `++`
	PlusPlus,
	/// `-`
	Minus,
	/// `--`
	MinusMinus,
	/// `->`
	MinusGt,
	/// `/`
	Slash,
	/// `*`
	Star,
	/// `^`
	Caret,
	/// `mod`
	Mod,
	/// `and`
	And,
	/// `not`
	Not,
	/// `or`
	Or,
	/// `=`
	Eq,
	/// `==`
	EqEq,
	/// `!=`
	BangEq,
	/// `>`
	Gt,
	/// `<`
	Lt,
	/// `>=`
	GtEq,
	/// `<=`
	LtEq,
	/// `(`
	LParen,
	/// `)`
	RParen,
	/// `[`
	LBracket,
	/// `]`
	RBracket,
	/// `{`
	LBrace,
	/// `}`
	RBrace,
	/// `:`
	Colon,
	/// `;`
	SemiColon,
	/// `,`
	Comma,
	/// `.`
	Dot,
}

impl Operator {
	pub fn to_string(&self) -> String {
		format!(
			"`{}`",
			match self {
				Operator::Plus => "+",
				Operator::PlusPlus => "++",
				Operator::Minus => "-",
				Operator::MinusMinus => "--",
				Operator::MinusGt => "->",
				Operator::Slash => "/",
				Operator::Star => "*",
				Operator::Caret => "^",
				Operator::Mod => "mod",
				Operator::And => "and",
				Operator::Not => "not",
				Operator::Or => "or",
				Operator::Eq => "=",
				Operator::EqEq => "==",
				Operator::BangEq => "!=",
				Operator::Gt => ">",
				Operator::Lt => "<",
				Operator::GtEq => ">=",
				Operator::LtEq => "<=",
				Operator::LParen => "(",
				Operator::RParen => ")",
				Operator::LBracket => "[",
				Operator::RBracket => "]",
				Operator::LBrace => "{",
				Operator::RBrace => "}",
				Operator::Colon => ":",
				Operator::SemiColon => ";",
				Operator::Comma => ",",
				Operator::Dot => ".",
			}
		)
	}
}

/// The type of `Token`.
#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind {
	Keyword(Keyword),
	Operator(Operator),
	Literal(Literal),
}

impl TokenKind {
	pub fn to_string(&self) -> String {
		match self {
			TokenKind::Keyword(k) => k.to_string(),
			TokenKind::Operator(k) => k.to_string(),
			TokenKind::Literal(k) => k.to_string(),
		}
	}
}

/// Capable of being converted to a `Token`.
pub trait TokenKindResolvable {
	fn build(self) -> TokenKind;
}

impl<'a> TokenKindResolvable for TokenKind {
	fn build(self) -> TokenKind {
		self
	}
}

impl<'a> TokenKindResolvable for i64 {
	fn build(self) -> TokenKind {
		TokenKind::Literal(Literal::Int(self))
	}
}

impl<'a> TokenKindResolvable for f64 {
	fn build(self) -> TokenKind {
		TokenKind::Literal(Literal::Float(self))
	}
}

impl<'a> TokenKindResolvable for bool {
	fn build(self) -> TokenKind {
		TokenKind::Literal(Literal::Boolean(self))
	}
}

impl<'a> TokenKindResolvable for Keyword {
	fn build(self) -> TokenKind {
		TokenKind::Keyword(self)
	}
}

impl<'a> TokenKindResolvable for Operator {
	fn build(self) -> TokenKind {
		TokenKind::Operator(self)
	}
}

impl<'a> TokenKindResolvable for Literal {
	fn build(self) -> TokenKind {
		TokenKind::Literal(self)
	}
}

/// An atomic element of Ares source code.
/// It stores the location and the type (kind) of element.
#[derive(Debug, PartialEq, Eq)]
pub struct Token {
	kind: TokenKind,
	span: Span,
}

impl Token {
	pub fn new<T: TokenKindResolvable, U: SpanLike>(kind: T, span: U) -> Self {
		Self {
			kind: kind.build(),
			span: span.build(),
		}
	}

	pub fn kind(&self) -> &TokenKind {
		&self.kind
	}

	pub fn keyword(&self) -> Option<&Keyword> {
		if let TokenKind::Keyword(keyword) = &self.kind {
			return Some(keyword);
		}
		None
	}

	pub fn operator(&self) -> Option<&Operator> {
		if let TokenKind::Operator(operator) = &self.kind {
			return Some(operator);
		}
		None
	}

	pub fn literal(&self) -> Option<&Literal> {
		if let TokenKind::Literal(literal) = &self.kind {
			return Some(literal);
		}
		None
	}

	pub fn span(&self) -> &Span {
		&self.span
	}

	/// Same as `Span::combine_span` but on the token level instead.
	pub fn combine_span(first: &Token, second: &Token) -> Span {
		Span::combine_span(first.span(), second.span())
	}
}
