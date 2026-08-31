//! Lexer and lexical errors
//!
//! You do not usually need to use this crate, as the [parser](super::parser)
//! already creates [`ZircoLexer`] instances for you. However, there are some
//! cases where it may be helpful, so it is kept public.
//!
//! For more information, read the documentation of [`ZircoLexer`].

pub mod token;

use token::{
	Token,
	TokenKind::{self, *},
};
use zrc_diagnostics::{Diagnostic, DiagnosticKind, HelpKind, LabelKind, diagnostic::GenericLabel};
use zrc_utils::span::{Span, Spannable};

/// A lexer for the Zirco programming language.
///
/// A lexer can be created with [`ZircoLexer::new`], and tokens can be consumed
/// using [`ZircoLexer::next_token`]. The lexer will return [`Token`]s until the
/// end of the input is reached.
#[derive(Debug, Clone)]
pub struct ZircoLexer<'input> {
	/// The input string to lex.
	input: &'input str,
	/// The file name of the input string, used for error reporting.
	file_name: &'static str,
	/// The current offset in the input string.
	offset: usize,
}

impl<'input> ZircoLexer<'input> {
	/// Create a new [`ZircoLexer`] given an input string and file name.
	///
	/// ```
	/// use zrc_parser::lexer::{ZircoLexer, token::TokenKind};
	/// let mut lex = ZircoLexer::new("fn main() {}", "sample.zr");
	/// let token = lex.next_token().unwrap().unwrap();
	/// assert_eq!(token.kind, TokenKind::Fn);
	/// ```
	#[must_use]
	pub const fn new(input: &'input str, file_name: &'static str) -> Self {
		Self {
			input,
			file_name,
			offset: 0,
		}
	}

	/// Lex the next token.
	///
	/// # Errors
	///
	/// Returns [`None`] when the end of the input is reached. If a lexical
	/// error is encountered, a [`Diagnostic`] is returned.
	#[expect(clippy::result_large_err)]
	pub fn next_token(&mut self) -> Result<Option<Token<'input>>, Diagnostic> {
		self.skip_whitespace();
		self.skip_comments();

		let start = self.offset;

		let Some(ch) = self.peek() else {
			return Ok(None);
		};

		let kind = match ch {
			';' => self.simple(Semicolon),
			',' => self.simple(Comma),
			'?' => self.simple(QuestionMark),
			'(' => self.simple(LeftParen),
			')' => self.simple(RightParen),
			'[' => self.simple(LeftBracket),
			']' => self.simple(RightBracket),
			'{' => self.simple(LeftBrace),
			'}' => self.simple(RightBrace),
			'~' => self.simple(BitwiseNot),
			'.' => self.lex_dot(),
			':' => self.lex_colon(),
			'+' => self.lex_plus(),
			'-' => self.lex_minus(),
			'<' => self.lex_left_angle(),
			'&' => self.lex_ampersand(),
			'|' => self.lex_pipe(),
			'=' => self.lex_eq()?,
			'!' => self.lex_not()?,
			'/' => self.lex_optional_assign(Slash, SlashAssign),
			'*' => self.lex_optional_assign(Star, StarAssign),
			'%' => self.lex_optional_assign(Percent, PercentAssign),
			'^' => self.lex_optional_assign(BitwiseXor, BitwiseXorAssign),
			'>' => self.lex_optional_assign(Greater, GreaterEq),

			'"' => self.lex_string_literal()?,
			'\'' => self.lex_char_literal()?,

			'0'..='9' => self.lex_number_literal()?,
			ch if ZircoLexer::is_identifier_start(ch) => self.lex_identifier(),

			ch => {
				let span = Span::from_positions_and_file(
					start,
					self.offset + ch.len_utf8(),
					self.file_name,
				);
				return Err(DiagnosticKind::UnknownToken(ch.to_string())
					.error_in(span)
					.with_label(GenericLabel::error(
						LabelKind::UnknownToken(ch.to_string()).in_span(span),
					)));
			}
		};

		Ok(Some(Token {
			kind,
			span: Span::from_positions_and_file(start, self.offset, self.file_name),
			literal: &self.input[start..self.offset],
		}))
	}

	/// Look at the next character in the input without consuming it.
	fn peek(&self) -> Option<char> {
		self.input[self.offset..].chars().next()
	}

	/// Peek the nth character in the input without consuming it.
	fn peek_nth(&self, n: usize) -> Option<char> {
		self.input[self.offset..].chars().nth(n)
	}

	/// Consume the next character in the input and return it.
	fn bump(&mut self) -> Option<char> {
		let ch = self.peek()?;
		self.offset += ch.len_utf8();
		Some(ch)
	}

	/// Consume the next character in the input if it matches the given
	/// character.
	///
	/// Returns `true` if the character was consumed, or `false` if it did not
	/// match.
	fn eat(&mut self, ch: char) -> bool {
		if self.peek() == Some(ch) {
			self.bump();
			true
		} else {
			false
		}
	}

	/// Consume the next few characters in the input if they match the given
	/// string.
	///
	/// Returns `true` if the string was consumed, or `false` if it did not
	/// match.
	fn eat_str(&mut self, s: &str) -> bool {
		if self.input[self.offset..].starts_with(s) {
			self.offset += s.len();
			true
		} else {
			false
		}
	}

	/// Bump offset until a non-whitespace character is found.
	fn skip_whitespace(&mut self) {
		while self.peek().is_some_and(char::is_whitespace) {
			self.bump();
		}
	}

	/// Eat the next character and return the provided [`TokenKind`].
	fn simple(&mut self, kind: TokenKind) -> TokenKind {
		self.bump();
		kind
	}

	fn skip_comments(&mut self) {
		// Skip line comments (//) or block comments (/* */)
		// Block comments support nesting. Unterminated blocks create a diagnostic.
		while let Some(ch) = self.peek() {
			if ch == '/' {
				if self.peek_nth(1) == Some('/') {
					// Line comment
					self.bump(); // consume '/'
					self.bump(); // consume second '/'
					while let Some(ch) = self.peek() {
						if ch == '\n' {
							break;
						}
						self.bump();
					}
				} else if self.peek_nth(1) == Some('*') {
					// Block comment
					self.bump(); // consume '/'
					self.bump(); // consume '*'
					let mut depth = 1;
					while let Some(ch) = self.peek() {
						if ch == '/' && self.peek_nth(1) == Some('*') {
							depth += 1;
							self.bump();
							self.bump();
						} else if ch == '*' && self.peek_nth(1) == Some('/') {
							depth -= 1;
							self.bump();
							self.bump();
							if depth == 0 {
								break;
							}
						} else {
							self.bump();
						}
					}
				} else {
					break;
				}
			} else if ch.is_whitespace() {
				self.skip_whitespace();
			} else {
				break;
			}
		}
	}

	/// If the peeked token is a `.`, lex a `.` or `...`.
	fn lex_dot(&mut self) -> TokenKind {
		self.bump();
		if self.eat_str("..") { Ellipsis } else { Dot }
	}

	/// Lex some operator that may be followed by an `=`
	fn lex_optional_assign(&mut self, normal_kind: TokenKind, assign_kind: TokenKind) -> TokenKind {
		self.bump();
		if self.eat('=') {
			assign_kind
		} else {
			normal_kind
		}
	}

	/// Lex a `=` token.
	#[expect(clippy::result_large_err)]
	fn lex_eq(&mut self) -> Result<TokenKind, Diagnostic> {
		let start = self.offset;
		self.bump();
		if self.eat('=') {
			if self.eat('=') {
				let span = Span::from_positions_and_file(start, self.offset, self.file_name);
				return Err(DiagnosticKind::JavascriptUserDetected
					.error_in(span)
					.with_label(GenericLabel::error(
						LabelKind::JavascriptUserDetected.in_span(span),
					))
					.with_help(HelpKind::JavascriptUserDetected("==")));
			}
			Ok(EqEq)
		} else if self.eat('>') {
			Ok(FatArrow)
		} else {
			Ok(Assign)
		}
	}

	/// Lex a `!` token.
	#[expect(clippy::result_large_err)]
	fn lex_not(&mut self) -> Result<TokenKind, Diagnostic> {
		self.bump();
		if self.eat('=') {
			if self.eat('=') {
				let span =
					Span::from_positions_and_file(self.offset - 3, self.offset, self.file_name);
				return Err(DiagnosticKind::JavascriptUserDetected
					.error_in(span)
					.with_label(GenericLabel::error(
						LabelKind::JavascriptUserDetected.in_span(span),
					))
					.with_help(HelpKind::JavascriptUserDetected("!=")));
			}
			Ok(NotEq)
		} else {
			Ok(LogicalNot)
		}
	}

	/// Lex a `+` token.
	fn lex_plus(&mut self) -> TokenKind {
		self.bump();
		if self.eat('=') {
			PlusAssign
		} else if self.eat('+') {
			PlusPlus
		} else {
			Plus
		}
	}

	/// Lex a `-` token.
	fn lex_minus(&mut self) -> TokenKind {
		self.bump();
		if self.eat('=') {
			MinusAssign
		} else if self.eat('-') {
			MinusMinus
		} else if self.eat('>') {
			SmallArrow
		} else {
			Minus
		}
	}

	/// Lex a `<` token.
	fn lex_left_angle(&mut self) -> TokenKind {
		self.bump();
		if self.eat('=') {
			LessEq
		} else if self.eat('-') {
			SmallArrowBack
		} else {
			Less
		}
	}

	/// Lex a `&` token.
	fn lex_ampersand(&mut self) -> TokenKind {
		self.bump();
		if self.eat('=') {
			BitwiseAndAssign
		} else if self.eat('&') {
			LogicalAnd
		} else {
			BitwiseAnd
		}
	}

	/// Lex a `|` token.
	fn lex_pipe(&mut self) -> TokenKind {
		self.bump();
		if self.eat('=') {
			BitwiseOrAssign
		} else if self.eat('|') {
			LogicalOr
		} else {
			BitwiseOr
		}
	}

	/// Lex a `:` token.
	fn lex_colon(&mut self) -> TokenKind {
		self.bump();
		if self.eat(':') { ColonColon } else { Colon }
	}

	/// Lex a string literal.
	#[expect(clippy::result_large_err)]
	fn lex_string_literal(&mut self) -> Result<TokenKind, Diagnostic> {
		let start = self.offset;
		self.bump();
		// all we care about here is finding the end. escape sequences can be handled
		// later.
		while let Some(ch) = self.peek() {
			self.bump();
			#[expect(clippy::else_if_without_else)]
			if ch == '"' {
				return Ok(StringLiteral);
			} else if ch == '\\' {
				if self.peek().is_none() {
					break;
				}
				self.bump();
			}
		}
		let span = Span::from_positions_and_file(start, self.offset, self.file_name);
		Err(DiagnosticKind::UnterminatedStringLiteral
			.error_in(span)
			.with_label(GenericLabel::error(
				LabelKind::UnterminatedStringLiteral.in_span(span),
			)))
	}

	/// Lex a character literal.
	#[expect(clippy::result_large_err)]
	fn lex_char_literal(&mut self) -> Result<TokenKind, Diagnostic> {
		let start = self.offset;
		self.bump();
		// all we care about here is finding the end. escape sequences can be handled
		// later.
		while let Some(ch) = self.peek() {
			self.bump();

			#[expect(clippy::else_if_without_else)]
			if ch == '\'' {
				return Ok(CharLiteral);
			} else if ch == '\\' {
				if self.peek().is_none() {
					break;
				}
				self.bump();
			}
		}
		let span = Span::from_positions_and_file(start, self.offset, self.file_name);
		Err(DiagnosticKind::UnterminatedStringLiteral
			.error_in(span)
			.with_label(GenericLabel::error(
				LabelKind::UnterminatedStringLiteral.in_span(span),
			)))
	}

	fn lex_number_literal(&mut self) -> Result<TokenKind, Diagnostic> {
		// This can be any of the following:
		// 12345
		// 123_456.789
		// 0x1234ab_CD
		// 0b10101010
		// we do not support octal.

		if self.eat_str("0x") {
			while let Some(ch) = self.peek() {
				if ch.is_ascii_hexdigit() || ch == '_' {
					self.bump();
				} else {
					break;
				}
			}

			Ok(NumberLiteral)
		} else if self.eat_str("0b") {
			while let Some(ch) = self.peek() {
				if ch == '0' || ch == '1' || ch == '_' {
					self.bump();
				} else {
					break;
				}
			}

			Ok(NumberLiteral)
		} else {
			while let Some(ch) = self.peek() {
				if ch.is_ascii_digit() || ch == '_' {
					self.bump();
				} else {
					break;
				}
			}

			if self.peek_after_dot_is_digit() {
				self.bump(); // consume the dot
				while let Some(ch) = self.peek() {
					if ch.is_ascii_digit() || ch == '_' {
						self.bump();
					} else {
						break;
					}
				}
			}

			Ok(NumberLiteral)
		}
	}

	fn peek_after_dot_is_digit(&self) -> bool {
		self.input[self.offset..]
			.strip_prefix('.')
			.and_then(|rest| rest.chars().next())
			.is_some_and(|c| c.is_ascii_digit())
	}

	/// Check if a character is a valid start of an identifier.
	fn is_identifier_start(ch: char) -> bool {
		ch.is_ascii_alphabetic() || ch == '_'
	}

	/// Check if a character is a valid continuation of an identifier.
	fn is_identifier_continue(ch: char) -> bool {
		ch.is_ascii_alphanumeric() || ch == '_'
	}

	/// Lex an identifier or keyword.
	fn lex_identifier(&mut self) -> TokenKind {
		let start = self.offset;
		while let Some(ch) = self.peek() {
			if ZircoLexer::is_identifier_continue(ch) {
				self.bump();
			} else {
				break;
			}
		}

		match &self.input[start..self.offset] {
			"true" => True,
			"false" => False,
			"if" => If,
			"else" => Else,
			"while" => While,
			"do" => Do,
			"for" => For,
			"four" => Four,
			"break" => Break,
			"continue" => Continue,
			"return" => Return,
			"let" => Let,
			"const" => Const,
			"fn" => Fn,
			"as" => As,
			"struct" => Struct,
			"union" => Union,
			"enum" => Enum,
			"match" => Match,
			"sizeof" => SizeOf,
			"type" => Type,
			"switch" => Switch,
			"default" => Default,
			"new" => New,
			"unreachable" => Unreachable,
			"packed" => Packed,
			_ => Identifier,
		}
	}

	/// Collect all tokens until the end of the input is reached.
	///
	/// # Errors
	///
	/// Returns [`Err`] if a lexical error is encountered.
	#[expect(clippy::result_large_err)]
	pub fn collect_all_tokens(&mut self) -> Result<Vec<Token<'input>>, Diagnostic> {
		let mut tokens = Vec::new();
		while let Some(token) = self.next_token()? {
			tokens.push(token);
		}
		Ok(tokens)
	}
}

/// Determine if all delimiters in a string are balanced.
/// Useful for quick checks before passing to the parser in things like zrepl.
#[must_use]
pub fn are_delimiters_balanced(input: &str) -> bool {
	todo!()
}

#[cfg(test)]
mod tests;
