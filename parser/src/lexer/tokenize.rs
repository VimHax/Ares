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
		/// character is a `+` and otherwise construct an `Operator::Plus`
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
				// Keywords, boolean literals, some operators and identifiers.
				else if iter.is_alpha().unwrap() {
					let (s, span) = consume_identifier(&mut iter);
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
					// Check whether the identifier is a boolean literal or an operator.
					match s.as_str() {
						"true" => push_token!(true),         // true
						"false" => push_token!(false),       // false
						"mod" => push_token!(Operator::Mod), // mod
						"and" => push_token!(Operator::And), // and
						"or" => push_token!(Operator::Or),   // or
						"not" => push_token!(Operator::Not), // not
						_ => (),
					}
					// Else this is just a regular identifier.
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

#[test]
fn test_tokenizer_keywords() {
	use ares_core::Source;

	let contents = "fn let mut if else loop return break".to_string();
	let source = Source::from_string(0, "".to_string(), contents);

	match tokenize(&source) {
		Ok(v) => assert_eq!(
			v,
			vec![
				Token::new(Keyword::Fn, 0..2),
				Token::new(Keyword::Let, 3..6),
				Token::new(Keyword::Mut, 7..10),
				Token::new(Keyword::If, 11..13),
				Token::new(Keyword::Else, 14..18),
				Token::new(Keyword::Loop, 19..23),
				Token::new(Keyword::Return, 24..30),
				Token::new(Keyword::Break, 31..36),
			]
		),
		Err(_) => unreachable!(),
	}
}

#[test]
fn test_tokenizer_operators() {
	use ares_core::Source;

	let contents = "+++--- ->/*^mod and not or===!=><>=<=()[]{}:;,.".to_string();
	let source = Source::from_string(0, "".to_string(), contents);

	match tokenize(&source) {
		Ok(v) => assert_eq!(
			v,
			vec![
				Token::new(Operator::PlusPlus, 0..2),
				Token::new(Operator::Plus, 2..3),
				Token::new(Operator::MinusMinus, 3..5),
				Token::new(Operator::Minus, 5..6),
				Token::new(Operator::MinusGt, 7..9),
				Token::new(Operator::Slash, 9..10),
				Token::new(Operator::Star, 10..11),
				Token::new(Operator::Caret, 11..12),
				Token::new(Operator::Mod, 12..15),
				Token::new(Operator::And, 16..19),
				Token::new(Operator::Not, 20..23),
				Token::new(Operator::Or, 24..26),
				Token::new(Operator::EqEq, 26..28),
				Token::new(Operator::Eq, 28..29),
				Token::new(Operator::BangEq, 29..31),
				Token::new(Operator::Gt, 31..32),
				Token::new(Operator::Lt, 32..33),
				Token::new(Operator::GtEq, 33..35),
				Token::new(Operator::LtEq, 35..37),
				Token::new(Operator::LParen, 37..38),
				Token::new(Operator::RParen, 38..39),
				Token::new(Operator::LBracket, 39..40),
				Token::new(Operator::RBracket, 40..41),
				Token::new(Operator::LBrace, 41..42),
				Token::new(Operator::RBrace, 42..43),
				Token::new(Operator::Colon, 43..44),
				Token::new(Operator::SemiColon, 44..45),
				Token::new(Operator::Comma, 45..46),
				Token::new(Operator::Dot, 46..47),
			]
		),
		Err(_) => unreachable!(),
	}
}

#[test]
fn test_tokenizer_literals() {
	use ares_core::Source;

	let contents = "123 1.23 true false \"a string\" identifier".to_string();
	let source = Source::from_string(0, "".to_string(), contents);

	match tokenize(&source) {
		Ok(v) => assert_eq!(
			v,
			vec![
				Token::new(Literal::Int(123), 0..3),
				Token::new(Literal::Float(1.23), 4..8),
				Token::new(Literal::Boolean(true), 9..13),
				Token::new(Literal::Boolean(false), 14..19),
				Token::new(Literal::String("a string".to_string()), 20..30),
				Token::new(Literal::Identifier("identifier".to_string()), 31..41),
			]
		),
		Err(_) => unreachable!(),
	}
}

#[test]
fn test_tokenizer_general() {
	use ares_core::Source;

	let contents = "fn fib(term: Int) -> Int {
	if term == 0 { 0 }
	else if term == 1 { 1 }
	else { fib(term - 1) + fib(term - 2) }
}"
	.to_string();
	println!("{}", contents);
	let source = Source::from_string(0, "".to_string(), contents);

	match tokenize(&source) {
		Ok(v) => assert_eq!(
			v,
			vec![
				Token::new(Keyword::Fn, 0..2),
				Token::new(Literal::Identifier("fib".to_string()), 3..6),
				Token::new(Operator::LParen, 6..7),
				Token::new(Literal::Identifier("term".to_string()), 7..11),
				Token::new(Operator::Colon, 11..12),
				Token::new(Literal::Identifier("Int".to_string()), 13..16),
				Token::new(Operator::RParen, 16..17),
				Token::new(Operator::MinusGt, 18..20),
				Token::new(Literal::Identifier("Int".to_string()), 21..24),
				Token::new(Operator::LBrace, 25..26),
				Token::new(Keyword::If, 28..30),
				Token::new(Literal::Identifier("term".to_string()), 31..35),
				Token::new(Operator::EqEq, 36..38),
				Token::new(Literal::Int(0), 39..40),
				Token::new(Operator::LBrace, 41..42),
				Token::new(Literal::Int(0), 43..44),
				Token::new(Operator::RBrace, 45..46),
				Token::new(Keyword::Else, 48..52),
				Token::new(Keyword::If, 53..55),
				Token::new(Literal::Identifier("term".to_string()), 56..60),
				Token::new(Operator::EqEq, 61..63),
				Token::new(Literal::Int(0), 64..65),
				Token::new(Operator::LBrace, 66..67),
				Token::new(Literal::Int(0), 68..69),
				Token::new(Operator::RBrace, 70..71),
				Token::new(Keyword::Else, 73..77),
				Token::new(Operator::LBrace, 78..79),
				Token::new(Literal::Identifier("fib".to_string()), 80..83),
				Token::new(Operator::LParen, 83..84),
				Token::new(Literal::Identifier("term".to_string()), 84..88),
				Token::new(Operator::Minus, 89..90),
				Token::new(Literal::Int(1), 91..92),
				Token::new(Operator::RParen, 92..93),
				Token::new(Operator::Plus, 94..95),
				Token::new(Literal::Identifier("fib".to_string()), 96..99),
				Token::new(Operator::LParen, 99..100),
				Token::new(Literal::Identifier("term".to_string()), 100..104),
				Token::new(Operator::Minus, 105..106),
				Token::new(Literal::Int(2), 107..108),
				Token::new(Operator::RParen, 108..109),
				Token::new(Operator::RBrace, 110..111),
				Token::new(Operator::RBrace, 112..113),
			]
		),
		Err(_) => unreachable!(),
	}
}
