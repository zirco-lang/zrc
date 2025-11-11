//! Lexer and lexical errors
//!
//! This module contains the token types and lexical error types for the Zirco
//! lexer. The actual lexer implementation is in [`crate::custom_lexer`].

use derive_more::Display;
use zrc_utils::span::{Span, Spannable, Spanned};

/// The error enum for internal lexing errors.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum InternalLexicalError {
    /// A generic lexing error. This is later converted to
    /// [`LexicalError::UnknownToken`].
    #[default]
    NoMatchingRule,
    /// A string literal was left unterminated.
    UnterminatedStringLiteral,
    /// A block comment ran to the end of the file. Remind the user that block
    /// comments nest.
    UnterminatedBlockComment,
    /// An invalid escape sequence was found in a string literal
    /// The included [`Span`] is the specific span of the invalid sequence
    UnknownEscapeSequence(Span),
    /// `===` or `!==` was found in the input
    /// Parameter will be `==` or `!=` for what was expected.
    JavascriptUserDetected(&'static str),
}

/// An error encountered during lexing. You will usually find this wrapped in a
/// [`Spanned<LexicalError>`].
///
/// Does not implement [`std::error::Error`] because it should be converted to a
/// [`zrc_diagnostics::Diagnostic`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexicalError<'input> {
    /// An unknown token was encountered.
    UnknownToken(&'input str),
    /// A string literal was left unterminated.
    UnterminatedStringLiteral,
    /// A block comment ran to the end of the file. Remind the user that block
    /// comments nest.
    UnterminatedBlockComment,
    /// Produced from [`InternalLexicalError::NoMatchingRule`]
    UnknownEscapeSequence,
    /// `===` or `!==` was found in the input
    /// Parameter will be `==` or `!=` for what was expected.
    JavascriptUserDetected(&'static str),
}

/// A valid number literal in Zirco
#[derive(Debug, Clone, PartialEq, Eq, Display)]
pub enum NumberLiteral<'input> {
    /// A decimal number literal
    #[display("{_0}")]
    Decimal(&'input str),
    /// A hexadecimal number literal
    #[display("0x{_0}")]
    Hexadecimal(&'input str),
    /// A binary number literal
    #[display("0b{_0}")]
    Binary(&'input str),
}
impl<'input> NumberLiteral<'input> {
    /// Get the text content of this [`NumberLiteral`]
    #[must_use]
    pub const fn text_content(&self) -> &'input str {
        match self {
            Self::Decimal(n) | Self::Hexadecimal(n) | Self::Binary(n) => n,
        }
    }

    /// Convert a [`NumberLiteral`] into its radix (2, 10, or 16)
    #[must_use]
    pub const fn radix(&self) -> u32 {
        match self {
            Self::Decimal(_) => 10,
            Self::Hexadecimal(_) => 16,
            Self::Binary(_) => 2,
        }
    }
}

/// Enum representing all of the result tokens in the Zirco lexer
#[derive(Debug, Clone, PartialEq, Eq, Display)]
pub enum Tok<'input> {
    // === ARITHMETIC OPERATORS ===
    /// The token `++`
    #[display("++")]
    PlusPlus,
    /// The token `--`
    #[display("--")]
    MinusMinus,
    /// The token `+`
    #[display("+")]
    Plus,
    /// The token `-`
    #[display("-")]
    Minus,
    /// The token `*`
    #[display("*")]
    Star,
    /// The token `/`
    #[display("/")]
    Slash,
    /// The token `%`
    #[display("%")]
    Percent,

    // === COMPARISON OPERATORS ===
    /// The token `==`
    #[display("==")]
    EqEq,
    /// The token `!=`
    #[display("!=")]
    NotEq,
    /// The token `>`
    #[display(">")]
    Greater,
    /// The token `>=`
    #[display(">=")]
    GreaterEq,
    /// The token `<`
    #[display("<")]
    Less,
    /// The token `<=`
    #[display("<=")]
    LessEq,

    // === LOGICAL OPERATORS ===
    /// The token `&&`
    #[display("&&")]
    LogicalAnd,
    /// The token `||`
    #[display("||")]
    LogicalOr,
    /// The token `!`
    #[display("!")]
    LogicalNot,

    // === BITWISE OPERATORS ===
    /// The token `&`
    #[display("&")]
    BitwiseAnd,
    /// The token `|`
    #[display("|")]
    BitwiseOr,
    /// The token `^`
    #[display("^")]
    BitwiseXor,
    /// The token `~`
    #[display("~")]
    BitwiseNot,
    /// The token `<<`
    #[display("<<")]
    BitwiseLeftShift,
    /// The token `>>`
    #[display(">>")]
    BitwiseRightShift,

    // === ASSIGNMENT OPERATORS ===
    /// The token `=`
    #[display("=")]
    Assign,
    /// The token `+=`
    #[display("+=")]
    PlusAssign,
    /// The token `-=`
    #[display("-=")]
    MinusAssign,
    /// The token `*=`
    #[display("*=")]
    StarAssign,
    /// The token `/=`
    #[display("/=")]
    SlashAssign,
    /// The token `%=`
    #[display("%=")]
    PercentAssign,
    /// The token `&=`
    #[display("&=")]
    BitwiseAndAssign,
    /// The token `|=`
    #[display("|=")]
    BitwiseOrAssign,
    /// The token `^=`
    #[display("^=")]
    BitwiseXorAssign,
    /// The token `<<=`
    #[display("<<=")]
    BitwiseLeftShiftAssign,
    /// The token `>>=`
    #[display(">>=")]
    BitwiseRightShiftAssign,

    // === OTHER TOKENS ===
    /// The token `;`
    #[display(";")]
    Semicolon,
    /// The token `,`
    #[display(",")]
    Comma,
    /// The token `.`
    #[display(".")]
    Dot,
    /// The token `:`
    #[display(":")]
    Colon,
    /// The token `::`
    #[display("::")]
    ColonColon,
    /// The token `?`
    #[display("?")]
    QuestionMark,

    // === GROUPING ===
    /// The token `(` (left parenthesis)
    #[display("(")]
    LeftParen,
    /// The token `)` (right parenthesis)
    #[display(")")]
    RightParen,
    /// The token `[` (left square bracket)
    #[display("[")]
    LeftBracket,
    /// The token `]` (right square bracket)
    #[display("]")]
    RightBracket,
    /// The token `{` (left curly brace)
    #[display("{{")]
    LeftBrace,
    /// The token `}` (right curly brace)
    #[display("}}")]
    RightBrace,

    // === KEYWORDS & BUILT-INS ===
    /// The boolean `true`
    #[display("true")]
    True,
    /// The boolean `false`
    #[display("false")]
    False,
    /// The keyword `if`
    #[display("if")]
    If,
    /// The keyword `else`
    #[display("else")]
    Else,
    /// The keyword `while`
    #[display("while")]
    While,
    /// The keyword `do`
    #[display("do")]
    Do,
    /// The keyword `for`
    #[display("for")]
    For,
    /// The keyword `break`
    #[display("break")]
    Break,
    /// The keyword `continue`
    #[display("continue")]
    Continue,
    /// The keyword `return`
    #[display("return")]
    Return,
    /// The keyword `let`
    #[display("let")]
    Let,
    /// The keyword `fn`
    #[display("fn")]
    Fn,
    /// The keyword `as`
    #[display("as")]
    As,
    /// The keyword `struct`
    #[display("struct")]
    Struct,
    /// The keyword `union`
    #[display("union")]
    Union,
    /// The keyword `enum`
    #[display("enum")]
    Enum,
    /// The keyword `match`
    #[display("match")]
    Match,
    /// The keyword `sizeof`
    #[display("sizeof")]
    SizeOf,
    /// The keyword `type`
    #[display("type")]
    Type,
    /// The keyword `switch`
    #[display("switch")]
    Switch,
    /// The keyword `default`
    #[display("default")]
    Default,
    /// The keyword `new`
    #[display("new")]
    New,
    /// The keyword `unreachable`
    #[display("unreachable")]
    Unreachable,
    /// The operator `->`
    #[display("->")]
    SmallArrow,
    /// The operator `=>`
    #[display("=>")]
    FatArrow,
    /// The `...` for variadic functions
    #[display("...")]
    Ellipsis,

    // === SPECIAL ===
    /// Any character literal
    #[display("'{_0}'")]
    CharLiteral(StringTok<'input>),
    /// Any string literal
    #[display("\"{_0}\"")]
    StringLiteral(ZrcString<'input>),
    /// Any number literal
    #[display("{_0}")]
    NumberLiteral(NumberLiteral<'input>),
    /// Any identifier
    #[display("{_0}")]
    Identifier(&'input str),
}

/// The compiler's representation of a string literal in Zirco
///
/// Enum representing the lexed contents of a string literal
#[derive(Debug, Clone, PartialEq, Eq, Display)]
pub enum StringTok<'input> {
    /// `\n`
    #[display("\\n")]
    EscapedNewline,

    /// `\r`
    #[display("\\r")]
    EscapedCr,

    /// `\t`
    #[display("\\t")]
    EscapedTab,

    /// `\0`
    #[display("\\0")]
    EscapedNull,

    /// `\xXX` with `XX` being a hex literal
    #[display("\\x{_0}")]
    EscapedHexByte(&'input str),

    /// '\u{X...} with `X...` being a hex literal from `0`..`10FFFF`
    #[display("\\u{{{_0}}}")]
    EscapedUnicode(&'input str),

    /// `\\`
    #[display("\\\\")]
    EscapedBackslash,

    /// `\"`
    #[display("\\\"")]
    EscapedDoubleQuote,

    /// Any other text fragment
    #[display("{_0}")]
    Text(&'input str),
}
impl StringTok<'_> {
    /// Convert a [`StringTok`] into its literal [`char`] representation
    ///
    /// # Panics
    /// Panics if the [`StringTok`] is invalid
    #[must_use]
    pub fn as_byte(&self) -> char {
        match self {
            StringTok::EscapedBackslash => '\\',
            StringTok::EscapedCr => '\r',
            StringTok::EscapedNewline => '\n',
            StringTok::EscapedTab => '\t',
            StringTok::EscapedNull => '\0',
            StringTok::EscapedHexByte(byte) => {
                char::from_u32(u32::from_str_radix(byte, 16).expect("invalid byte"))
                    .expect("invalid char")
            }
            StringTok::EscapedUnicode(hex) => {
                char::from_u32(u32::from_str_radix(hex, 16).expect("Invalid hex"))
                    .expect("invalid char")
            }
            StringTok::EscapedDoubleQuote => '"',
            StringTok::Text(text) => {
                assert!(
                    text.chars().count() == 1,
                    "Char literal must be exactly one character"
                );
                text.chars()
                    .next()
                    .expect("char literal should have a first character")
            }
        }
    }
}

/// A representation of a string literal in the source code
#[derive(Debug, Clone, PartialEq, Eq, Display)]
#[display("{}", self.0.iter().map(ToString::to_string).collect::<String>())]
pub struct ZrcString<'input>(pub Vec<StringTok<'input>>);
impl ZrcString<'_> {
    /// Convert a [`ZrcString`] into a [`String`] for its REAL byte
    /// representation
    ///
    /// See also: [`StringTok::as_byte`]
    #[must_use]
    pub fn as_bytes(&self) -> String {
        self.0.iter().map(StringTok::as_byte).collect()
    }
}

/// A lexer for the Zirco programming language
///
/// This is re-exported from [`crate::custom_lexer::CustomZircoLexer`]
pub type ZircoLexer<'input> = crate::custom_lexer::CustomZircoLexer<'input>;
