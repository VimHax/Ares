use ares_core::{Keyword, Literal, Operator, PartialSpan, Source, Token};

use super::{char_iter::CharIter, scan::*, ScanError};

/// Extracts all the tokens in the provided source.
/// If an error was encountered during extraction it is returned.
pub fn tokenize<'a>(source: &'a Source) -> Result<Vec<Token>, ScanError> {
	let mut iter = CharIter::new(source);
	let mut tokens: Vec<Token> = vec![];

	// Go through all the characters in the source.
	while let Some(c) = iter.current_char() {
		// Ignore white-spaces.
		if c.is_whitespace() {
			iter.next();
			continue;
		}

		// If a character which is not a white-space is found it
		// must be the beginning of a new token. This partial span
		// keeps track of the starting position and, eventually, the ending
		// position which is enough to construct a full blown span which
		// each token must have.
		let mut span = PartialSpan::new(iter.current_idx());

		/// A simple macro for consuming one character long tokens.
		/// The provided parameter will be the kind of token constructed.
		macro_rules! consume {
			($op:expr) => {{
				iter.next();
				span.grow();
				tokens.push(Token::new($op, span));
			}};
		}

		/// A macro which takes in to consideration the very next character
		/// to decide which of the 2 provided tokens should be built.
		/// eg: `consume_2!('+', Operator::Plus, Operator::PlusPlus)`
		/// will construct an `Operator::PlusPlus` token if the next
		/// character is a `+' and otherwise construct an `Operator::Plus`
		/// operator.
		macro_rules! consume_2 {
			($c:expr, $op1:expr, $op2:expr) => {{
				iter.next();
				span.grow();
				tokens.push(if let Some($c) = iter.current_char() {
					iter.next();
					span.grow();
					Token::new($op2, span)
				} else {
					Token::new($op1, span)
				});
			}};
		}

		match c {
			'+' => consume_2!('+', Operator::Plus, Operator::PlusPlus), // +, ++
			'/' => consume!(Operator::Slash),                           // /
			'*' => consume!(Operator::Star),                            // *
			'^' => consume!(Operator::Caret),                           // ^
			'=' => consume_2!('=', Operator::Eq, Operator::EqEq),       // =, ==
			'>' => consume_2!('=', Operator::Gt, Operator::GtEq),       // >, >=
			'<' => consume_2!('=', Operator::Lt, Operator::LtEq),       // <, <=
			'(' => consume!(Operator::LParen),                          // (
			')' => consume!(Operator::RParen),                          // )
			'[' => consume!(Operator::LBracket),                        // [
			']' => consume!(Operator::RBracket),                        // ]
			'{' => consume!(Operator::LBrace),                          // {
			'}' => consume!(Operator::RBrace),                          // }
			':' => consume!(Operator::Colon),                           // :
			';' => consume!(Operator::SemiColon),                       // ;
			',' => consume!(Operator::Comma),                           // ,
			'.' => consume!(Operator::Dot),                             // .
			// -, --, -> (cannot use `consume_2!` here because there are 3 possible tokens)
			'-' => {
				iter.next();
				span.grow();
				tokens.push(if let Some('-') = iter.current_char() {
					iter.next();
					span.grow();
					Token::new(Operator::MinusMinus, span)
				} else if let Some('>') = iter.current_char() {
					iter.next();
					span.grow();
					Token::new(Operator::MinusGt, span)
				} else {
					Token::new(Operator::Minus, span)
				});
			}
			// != (cannot use `consume_2!` here because `!` isn't a valid token on it's own)
			'!' => {
				iter.next();
				span.grow();
				tokens.push(if let Some('=') = iter.current_char() {
					iter.next();
					span.grow();
					Token::new(Operator::BangEq, span)
				} else {
					return Err(ScanError::UnrecognizedCharacter(iter.cursor()));
				});
			}
			// "a string"
			'"' => {
				let (s, span) = consume_string(&mut iter)?;
				tokens.push(Token::new(Literal::String(s), span));
			}
			_ => {
				// Literal integers and floats.
				if iter.is_numeric().unwrap() {
					match consume_number(&mut iter)? {
						(Number::Int(value), span) => tokens.push(Token::new(value, span)),
						(Number::Float(value), span) => tokens.push(Token::new(value, span)),
					}
				}
				// Keywords, boolean literals and identifiers.
				else if iter.is_alpha().unwrap() {
					let (s, span) = consume_identifier(&mut iter)?;
					/// Push a token and then continue.
					macro_rules! push_token {
						($op:expr) => {{
							tokens.push(Token::new($op, span));
							continue;
						}};
					}
					// Check whether the identifier is a keyword.
					if let Some(keyword) = Keyword::from_str(s.as_str()) {
						push_token!(keyword);
					}
					match s.as_str() {
						"true" => push_token!(true),         // true
						"false" => push_token!(false),       // false
						"mod" => push_token!(Operator::Mod), // mod
						"and" => push_token!(Operator::And), // and
						"or" => push_token!(Operator::Or),   // or
						"not" => push_token!(Operator::Not), // not
						_ => (),
					}
					tokens.push(Token::new(Literal::Identifier(s), span));
				}
				// If all else fails this character cannot be tokenized.
				else {
					return Err(ScanError::UnrecognizedCharacter(iter.cursor()));
				}
			}
		};
	}

	Ok(tokens)
}
