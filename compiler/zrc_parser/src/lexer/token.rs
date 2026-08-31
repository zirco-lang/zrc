//! Tokens in the Zirco lexer

use std::fmt::Display;

use zrc_utils::span::{Span, Spanned};

/// Enum representing all of the possible kinds for tokens in Zirco
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[expect(missing_docs)]
pub enum TokenKind {
	PlusPlus,
	MinusMinus,
	Plus,
	Minus,
	Star,
	Slash,
	Percent,
	EqEq,
	NotEq,
	Greater,
	GreaterEq,
	Less,
	LessEq,
	LogicalAnd,
	LogicalOr,
	LogicalNot,
	BitwiseAnd,
	BitwiseOr,
	BitwiseXor,
	BitwiseNot,
	Assign,
	PlusAssign,
	MinusAssign,
	StarAssign,
	SlashAssign,
	PercentAssign,
	BitwiseAndAssign,
	BitwiseOrAssign,
	BitwiseXorAssign,
	Semicolon,
	Comma,
	Dot,
	Colon,
	ColonColon,
	QuestionMark,
	LeftParen,
	RightParen,
	LeftBracket,
	RightBracket,
	LeftBrace,
	RightBrace,
	True,
	False,
	If,
	Else,
	While,
	Do,
	For,
	Four,
	Break,
	Continue,
	Return,
	Let,
	Const,
	Fn,
	As,
	Struct,
	Union,
	Enum,
	Match,
	SizeOf,
	Type,
	Switch,
	Default,
	New,
	Unreachable,
	Packed,
	SmallArrow,
	SmallArrowBack,
	FatArrow,
	Ellipsis,
	CharLiteral,
	StringLiteral,
	NumberLiteral,
	Identifier,
}

/// A token in Zirco.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'input> {
	/// The kind of token
	pub kind: TokenKind,
	/// The span of the token in the source code
	pub span: Span,
	/// The literal value of the token, if applicable
	pub literal: &'input str,
}

impl<'input> Token<'input> {
	/// Get this token's literal value within its span
	#[must_use]
	pub const fn spanned_literal(&self) -> Spanned<&'input str> {
		self.span.containing(self.literal)
	}

	/// Convert a kind and spanned literal into a token
	#[must_use]
	pub const fn from_kind_and_literal(
		kind: TokenKind,
		spanned_literal: Spanned<&'input str>,
	) -> Self {
		Self {
			kind,
			span: spanned_literal.span(),
			literal: spanned_literal.value(),
		}
	}
}

impl Display for Token<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.literal)
	}
}
