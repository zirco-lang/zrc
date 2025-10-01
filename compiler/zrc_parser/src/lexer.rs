//! Lexer and lexical errors
//!
//! This module contains a wrapper around a [logos] lexer that splits an input
//! Zirco text into its individual tokens, which can be then passed into the
//! internal Zirco parser.
//!
//! You do not usually need to use this crate, as the [parser](super::parser)
//! already creates [`ZircoLexer`] instances for you before passing them to the
//! internal parser. However, there are some cases where it may be helpful, so
//! it is kept public.
//!
//! # Example
//! ```
//! use zrc_parser::lexer::{ZircoLexer, Tok, NumberLiteral};
//! use zrc_utils::{span::{Span, Spanned}, spanned};
//!
//! let mut lex = ZircoLexer::new("2 + 2");
//! assert_eq!(lex.next(), Some(spanned!(
//!     0,
//!     Ok(Tok::NumberLiteral(NumberLiteral::Decimal("2"))),
//!     1
//! )));
//! assert_eq!(lex.next(), Some(spanned!(2, Ok(Tok::Plus), 3)));
//! assert_eq!(lex.next(), Some(spanned!(
//!     4,
//!     Ok(Tok::NumberLiteral(NumberLiteral::Decimal("2"))),
//!     5
//! )));
//! assert_eq!(lex.next(), None);
//! ```
//!
//! For more information, read the documentation of [`ZircoLexer`].

use derive_more::Display;
use logos::{Lexer, Logos};
use zrc_utils::span::{Span, Spannable, Spanned};

/// The error enum passed to the internal logos [`Lexer`]. Will be converted to
/// a [`LexicalError`] later on by [`ZircoLexer`].
///
/// Do not use publicly. This cannot be made private because the Tok enum is
/// public and derives Lexer.
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

/// A lexer callback helper to obtain the currently matched token slice.
fn lexer_slice<'input, T: Logos<'input>>(
    lex: &Lexer<'input, T>,
) -> <<T as Logos<'input>>::Source as logos::Source>::Slice<'input> {
    lex.slice()
}

/// A lexer callback helper specifically meant for handling `\xFF` to `FF` in
/// [`StringTok::EscapedHexByte`]
fn escaped_byte_slice<'input>(lex: &Lexer<'input, StringTok<'input>>) -> &'input str {
    let slice = lex.slice();

    // \xFF
    //   ^^ return this

    &slice[2..]
}

/// A lexer callback header to convert a captured span to a [`Vec`]tor of
/// [`StringTok`]s.
fn lex_string_contents<'input>(
    lex: &Lexer<'input, Tok<'input>>,
) -> Result<Vec<StringTok<'input>>, InternalLexicalError> {
    let slice = lex.slice();
    let start_offset = lex.span().start + 1; // account for the opening quote

    // trim off the quotes at the start and end
    let contents = &slice[1..slice.len() - 1];

    let mut string_lexer = StringTok::lexer(contents);
    let mut tokens = Vec::new();

    loop {
        let Some(token) = string_lexer.next() else {
            break;
        };
        let span = string_lexer.span();

        match token {
            Ok(token_inner) => tokens.push(token_inner),
            Err(()) => {
                return Err(InternalLexicalError::UnknownEscapeSequence(
                    Span::from_positions(span.start + start_offset, span.end + start_offset),
                ));
            }
        }
    }

    Ok(tokens)
}

/// Zirco uses nested block comments -- a regular expression can't match this
/// without recursion, so our approach is to use a custom callback which takes
/// the lexer and basically consumes characters in our input until we reach the
/// end of the comment. See also: [logos#307](https://github.com/maciejhirsz/logos/issues/307)
/// See also: [zrc#14](https://github.com/zirco-lang/zrc/pull/14)
fn handle_block_comment_start<'input>(
    lex: &mut Lexer<'input, Tok<'input>>,
) -> logos::FilterResult<(), InternalLexicalError> {
    let mut depth = 1;
    // This contains all of the remaining tokens in our input except for the opening
    // to this comment -- that's already been consumed.
    let mut chars = lex.remainder().chars().peekable();

    // We iterate over all of the remaining characters in the input...
    while let Some(char) = chars.next() {
        // ...tell the Lexer this token spans into this character...
        lex.bump(1);

        // and perform some action for each sequence of 2 characters:
        match (char, chars.peek()) {
            // If it's "/*", we're starting a new comment, consume the '*' and increase our depth...
            ('/', Some(&'*')) => {
                chars.next();
                lex.bump(1);
                depth += 1;
            }
            // And the inverse for */...
            ('*', Some(&'/')) => {
                chars.next();
                lex.bump(1);
                depth -= 1;
            }
            // Any other sequence can be ignored.
            _ => {}
        }

        // If we've exited our last comment, we're all done!
        if depth == 0 {
            break;
        }
    }

    if depth == 0 {
        // We've reached the end of this block comment - because we attach the
        // handle_block_comment_start callback to basically any token variant
        // (to keep the Tok enum clean of useless variants), we should simply
        // skip this token. This will skip from the beginning of our Span to the end
        // that was given through all of the calls to lex.bump().
        logos::FilterResult::Skip
    } else {
        // This means we've reached the end of our input still in a comment.
        // We can throw an error here.
        logos::FilterResult::Error(InternalLexicalError::UnterminatedBlockComment)
    }
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
///
/// Do not use `Tok::lexer` publicly. Use [`ZircoLexer`] instead.
#[derive(Logos, Debug, Clone, PartialEq, Eq, Display)]
#[logos(
    error = InternalLexicalError,
    skip r"[ \t\r\n\f]+",         // whitespace
    skip r"//[^\r\n]*(\r\n|\n)?", // single-line comments
    // multi-line comments are handled by a callback: see handle_block_comment_start.
)]
pub enum Tok<'input> {
    // Handle nested block comments -- this does not need its own token type and can be attached
    // to whatever token is directly below this. The handle_block_comment_start will either Skip
    // the matched characters or throw an error. It will never return a token.
    // Read the doc comment above handle_block_comment_start for more information.
    #[token("/*", handle_block_comment_start)]
    // === ARITHMETIC OPERATORS ===
    /// The token `+`
    #[token("+")]
    #[display("+")]
    Plus,
    /// The token `-`s
    #[token("-")]
    #[display("-")]
    Minus,
    /// The token `*`
    #[token("*")]
    #[display("*")]
    Star,
    /// The token `/`
    #[token("/")]
    #[display("/")]
    Slash,
    /// The token `%`
    #[token("%")]
    #[display("%")]
    Percent,

    // === COMPARISON OPERATORS ===
    // Silly little error we raise if JavaScript-like equality operators are used
    #[token("===", |_lex| {
        Err::<Tok,InternalLexicalError>(InternalLexicalError::JavascriptUserDetected("=="))
    })]
    #[token("!==", |_lex| {
        Err::<Tok,InternalLexicalError>(InternalLexicalError::JavascriptUserDetected("!="))
    })]
    /// The token `==`
    #[token("==")]
    #[display("==")]
    EqEq,
    /// The token `!=`
    #[token("!=")]
    #[display("!=")]
    NotEq,
    /// The token `>`
    #[token(">")]
    #[display(">")]
    Greater,
    /// The token `>=`
    #[token(">=")]
    #[display(">=")]
    GreaterEq,
    /// The token `<`
    #[token("<")]
    #[display("<")]
    Less,
    /// The token `<=`
    #[token("<=")]
    #[display("<=")]
    LessEq,

    // === LOGICAL OPERATORS ===
    /// The token `&&`
    #[token("&&")]
    #[display("&&")]
    LogicalAnd,
    /// The token `||`
    #[token("||")]
    #[display("||")]
    LogicalOr,
    /// The token `!`
    #[token("!")]
    #[display("!")]
    LogicalNot,

    // === BITWISE OPERATORS ===
    /// The token `&`
    #[token("&")]
    #[display("&")]
    BitwiseAnd,
    /// The token `|`
    #[token("|")]
    #[display("|")]
    BitwiseOr,
    /// The token `^`
    #[token("^")]
    #[display("^")]
    BitwiseXor,
    /// The token `~`
    #[token("~")]
    #[display("~")]
    BitwiseNot,
    /// The token `<<`
    #[token("<<")]
    #[display("<<")]
    BitwiseLeftShift,
    // FIXME: The lexer could treat Foo<Bar>> as Foo < Bar >>, not Foo < Bar > >.
    //        This is the classic Java generics problem. This might be a Logos
    //        limitation and might require a custom state-machine lexer integrated
    //        into the parser -- we'll see when we get to generics.
    /// The token `>>`
    #[token(">>")]
    #[display(">>")]
    BitwiseRightShift,

    // === ASSIGNMENT OPERATORS ===
    /// The token `=`
    #[token("=")]
    #[display("=")]
    Assign,
    /// The token `+=`
    #[token("+=")]
    #[display("+=")]
    PlusAssign,
    /// The token `-=`
    #[token("-=")]
    #[display("-=")]
    MinusAssign,
    /// The token `*=`
    #[token("*=")]
    #[display("*=")]
    StarAssign,
    /// The token `/=`
    #[token("/=")]
    #[display("/=")]
    SlashAssign,
    /// The token `%=`
    #[token("%=")]
    #[display("%=")]
    PercentAssign,
    /// The token `&=`
    #[token("&=")]
    #[display("&=")]
    BitwiseAndAssign,
    /// The token `|=`
    #[token("|=")]
    #[display("|=")]
    BitwiseOrAssign,
    /// The token `^=`
    #[token("^=")]
    #[display("^=")]
    BitwiseXorAssign,
    /// The token `<<=`
    #[token("<<=")]
    #[display("<<=")]
    BitwiseLeftShiftAssign,
    /// The token `>>=`
    #[token(">>=")]
    #[display(">>=")]
    BitwiseRightShiftAssign,

    // === OTHER TOKENS ===
    /// The token `;`
    #[token(";")]
    #[display(";")]
    Semicolon,
    /// The token `,`
    #[token(",")]
    #[display(",")]
    Comma,
    /// The token `.`
    #[token(".")]
    #[display(".")]
    Dot,
    /// The token `:`
    #[token(":")]
    #[display(":")]
    Colon,
    /// The token `::`
    #[token("::")]
    #[display("::")]
    ColonColon,
    /// The token `?`
    #[token("?")]
    #[display("?")]
    QuestionMark,

    // === GROUPING ===
    /// The token `(` (left parenthesis)
    #[token("(")]
    #[display("(")]
    LeftParen,
    /// The token `)` (right parenthesis)
    #[token(")")]
    #[display(")")]
    RightParen,
    /// The token `[` (left square bracket)
    #[token("[")]
    #[display("[")]
    LeftBracket,
    /// The token `]` (right square bracket)
    #[token("]")]
    #[display("]")]
    RightBracket,
    /// The token `{` (left curly brace)
    #[token("{")]
    #[display("{{")]
    LeftBrace,
    /// The token `}` (right curly brace)
    #[token("}")]
    #[display("}}")]
    RightBrace,

    // === KEYWORDS & BUILT-INS ===
    /// The boolean `true`
    #[token("true")]
    #[display("true")]
    True,
    /// The boolean `false`
    #[token("false")]
    #[display("false")]
    False,
    /// The keyword `if`
    #[token("if")]
    #[display("if")]
    If,
    /// The keyword `else`
    #[token("else")]
    #[display("else")]
    Else,
    /// The keyword `while`
    #[token("while")]
    #[display("while")]
    While,
    /// The keyword `do`
    #[token("do")]
    #[display("do")]
    Do,
    /// The keyword `for`
    #[token("for")]
    #[display("for")]
    For,
    /// The keyword `break`
    #[token("break")]
    #[display("break")]
    Break,
    /// The keyword `continue`
    #[token("continue")]
    #[display("continue")]
    Continue,
    /// The keyword `return`
    #[token("return")]
    #[display("return")]
    Return,
    /// The keyword `let`
    #[token("let")]
    #[display("let")]
    Let,
    /// The keyword `fn`
    #[token("fn")]
    #[display("fn")]
    Fn,
    /// The keyword `as`
    #[token("as")]
    #[display("as")]
    As,
    /// The keyword `struct`
    #[token("struct")]
    #[display("struct")]
    Struct,
    /// The keyword `union`
    #[token("union")]
    #[display("union")]
    Union,
    /// The keyword `sizeof`
    #[token("sizeof")]
    #[display("sizeof")]
    SizeOf,
    /// The keyword `type`
    #[token("type")]
    #[display("type")]
    Type,
    /// The keyword `switch`
    #[token("switch")]
    #[display("switch")]
    Switch,
    /// The keyword `default`
    #[token("default")]
    #[display("default")]
    Default,
    /// The keyword `void`
    #[token("void")]
    #[display("void")]
    Void,
    /// The operator `->`
    #[token("->")]
    #[display("->")]
    SmallArrow,
    /// The operator `=>`
    #[token("=>")]
    #[display("=>")]
    FatArrow,
    /// The `...` for variadic functions
    #[token("...")]
    #[display("...")]
    Ellipsis,

    // === SPECIAL ===
    /// Any character literal
    #[regex(r"'([^'\\]|\\.)'", |lex| {
        lex_string_contents(lex).map(|contents| {
            assert!(contents.len() == 1, "Char literal must be exactly one character");
            contents[0].clone()
        })
    })]
    #[regex(r"'([^'\\]|\\.)", |_lex| {
        Err(InternalLexicalError::UnterminatedStringLiteral)
    })]
    #[display("'{_0}'")]
    CharLiteral(StringTok<'input>),
    /// Any string literal
    #[regex(r#""([^"\\]|\\.)*""#, |lex| lex_string_contents(lex).map(ZrcString))]
    #[regex(r#""([^"\\]|\\.)*"#, |_lex| {
        Err(InternalLexicalError::UnterminatedStringLiteral)
    })]
    #[display("\"{_0}\"")]
    StringLiteral(ZrcString<'input>),
    /// Any number literal
    // FIXME: Do not accept multiple decimal points like "123.456.789"
    #[regex(r"[0-9][0-9\._]*", |lex| NumberLiteral::Decimal(lex.slice()))]
    #[regex(r"0x[0-9a-fA-F_]+", |lex| NumberLiteral::Hexadecimal(&lex.slice()[2..]))]
    #[regex(r"0b[01_]+", |lex| NumberLiteral::Binary(&lex.slice()[2..]))]
    #[display("{_0}")]
    NumberLiteral(NumberLiteral<'input>),
    /// Any identifier
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", lexer_slice)]
    #[display("{_0}")]
    Identifier(&'input str),
}

/// The compiler's representation of a string literal in Zirco
///
/// Enum representing the lexed contents of a string literal
#[derive(Logos, Debug, Clone, PartialEq, Eq, Display)]
pub enum StringTok<'input> {
    /// `\n`
    #[token("\\n")]
    #[display("\\n")]
    EscapedNewline,

    /// `\r`
    #[token("\\r")]
    #[display("\\r")]
    EscapedCr,

    /// `\t`
    #[token("\\t")]
    #[display("\\t")]
    EscapedTab,

    /// `\0`
    #[token("\\0")]
    #[display("\\0")]
    EscapedNull,

    /// `\xXX` with `XX` being a hex literal
    #[regex(r"\\x[0-9a-fA-F]{2}", escaped_byte_slice)]
    #[display("\\x{_0}")]
    EscapedHexByte(&'input str),

    /// `\\`
    #[token("\\\\")]
    #[display("\\\\")]
    EscapedBackslash,

    /// `\"`
    #[token("\\\"")]
    #[display("\\\"")]
    EscapedDoubleQuote,

    /// Any other text fragment
    #[regex(r"[^\\]", lexer_slice)]
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
                char::from_u32(byte.parse::<u32>().expect("invalid byte")).expect("invalid char")
            }
            StringTok::EscapedDoubleQuote => '"',
            StringTok::Text(text) => {
                assert!(
                    text.len() == 1,
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
#[derive(Debug, Clone)]
pub struct ZircoLexer<'input> {
    /// The internal [`Lexer`] we wrap
    lex: Lexer<'input, Tok<'input>>,
}

impl<'input> ZircoLexer<'input> {
    /// Create a new [`ZircoLexer`] given an input string
    #[must_use]
    pub fn new(input: &'input str) -> Self {
        ZircoLexer {
            lex: Tok::lexer(input),
        }
    }
}

impl<'input> Iterator for ZircoLexer<'input> {
    type Item = Spanned<Result<Tok<'input>, LexicalError<'input>>>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.lex.next()?;
        let logos_span = self.lex.span();
        let span = Span::from_positions(logos_span.start, logos_span.end);

        Some(
            token
                .map_err(|err| match err {
                    InternalLexicalError::NoMatchingRule => {
                        LexicalError::UnknownToken(self.lex.slice())
                    }
                    InternalLexicalError::UnterminatedBlockComment => {
                        LexicalError::UnterminatedBlockComment
                    }
                    InternalLexicalError::UnterminatedStringLiteral => {
                        LexicalError::UnterminatedStringLiteral
                    }
                    InternalLexicalError::UnknownEscapeSequence(_) => {
                        LexicalError::UnknownEscapeSequence
                    }
                    InternalLexicalError::JavascriptUserDetected(expected) => {
                        LexicalError::JavascriptUserDetected(expected)
                    }
                })
                .in_span(span),
        )
    }
}

#[cfg(test)]
mod tests {
    use zrc_utils::spanned;

    use super::*;

    #[test]
    fn whitespace_should_be_skipped() {
        let lexer = ZircoLexer::new(" t\t e \n\ns\nt\r\n  s");
        let tokens: Vec<_> = lexer
            .map(|x| x.transpose().expect("lexing should succeed"))
            .collect();
        assert_eq!(
            tokens,
            vec![
                spanned!(1, Tok::Identifier("t"), 2),
                spanned!(4, Tok::Identifier("e"), 5),
                spanned!(8, Tok::Identifier("s"), 9),
                spanned!(10, Tok::Identifier("t"), 11),
                spanned!(15, Tok::Identifier("s"), 16),
            ]
        );
    }

    #[test]
    fn unclosed_strings_should_error() {
        let lexer = ZircoLexer::new("\"abc");
        let tokens: Vec<_> = lexer.collect();
        assert_eq!(
            tokens,
            vec![spanned!(0, Err(LexicalError::UnterminatedStringLiteral), 4),]
        );
    }

    /// Tests that all tokens can be properly lexed, and that they all impl
    /// [`Display`] correctly.
    #[test]
    fn all_tokens_lex_and_display_correctly() {
        let input = concat!(
            "+ - * / % == != > >= < <= && || ! & | ^ ~ << >> = += -= *= /= %= &= |= ^= <<= >>= ; ,",
            " . : :: ? ( ) [ ] { } true false if else while do for break continue return let fn as",
            r#" struct union sizeof type switch default void -> => "str" 7_000 0xF_A 0b1_0 abc"#
        );
        let tokens: Vec<Tok> = vec![
            Tok::Plus,
            Tok::Minus,
            Tok::Star,
            Tok::Slash,
            Tok::Percent,
            Tok::EqEq,
            Tok::NotEq,
            Tok::Greater,
            Tok::GreaterEq,
            Tok::Less,
            Tok::LessEq,
            Tok::LogicalAnd,
            Tok::LogicalOr,
            Tok::LogicalNot,
            Tok::BitwiseAnd,
            Tok::BitwiseOr,
            Tok::BitwiseXor,
            Tok::BitwiseNot,
            Tok::BitwiseLeftShift,
            Tok::BitwiseRightShift,
            Tok::Assign,
            Tok::PlusAssign,
            Tok::MinusAssign,
            Tok::StarAssign,
            Tok::SlashAssign,
            Tok::PercentAssign,
            Tok::BitwiseAndAssign,
            Tok::BitwiseOrAssign,
            Tok::BitwiseXorAssign,
            Tok::BitwiseLeftShiftAssign,
            Tok::BitwiseRightShiftAssign,
            Tok::Semicolon,
            Tok::Comma,
            Tok::Dot,
            Tok::Colon,
            Tok::ColonColon,
            Tok::QuestionMark,
            Tok::LeftParen,
            Tok::RightParen,
            Tok::LeftBracket,
            Tok::RightBracket,
            Tok::LeftBrace,
            Tok::RightBrace,
            Tok::True,
            Tok::False,
            Tok::If,
            Tok::Else,
            Tok::While,
            Tok::Do,
            Tok::For,
            Tok::Break,
            Tok::Continue,
            Tok::Return,
            Tok::Let,
            Tok::Fn,
            Tok::As,
            Tok::Struct,
            Tok::Union,
            Tok::SizeOf,
            Tok::Type,
            Tok::Switch,
            Tok::Default,
            Tok::Void,
            Tok::SmallArrow,
            Tok::FatArrow,
            Tok::StringLiteral(ZrcString(vec![
                StringTok::Text("s"),
                StringTok::Text("t"),
                StringTok::Text("r"),
            ])),
            Tok::NumberLiteral(NumberLiteral::Decimal("7_000")),
            Tok::NumberLiteral(NumberLiteral::Hexadecimal("F_A")),
            Tok::NumberLiteral(NumberLiteral::Binary("1_0")),
            Tok::Identifier("abc"),
        ];

        assert_eq!(
            ZircoLexer::new(input)
                .map(|x| x.transpose().expect("lexing should succeed").into_value())
                .collect::<Vec<_>>(),
            tokens
        );
        assert_eq!(
            tokens
                .into_iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join(" "),
            input
        );
    }

    /// Comment support
    mod comments {
        use super::*;

        /// Simple single-line comments should work as expected
        #[test]
        fn single_line_comments_are_skipped() {
            let lexer = ZircoLexer::new(concat!(
                "a\n",
                "//abc\n",
                "b\n",
                "// def\n",
                "c // ghi\n",
                "// jkl",
            ));
            let tokens: Vec<_> = lexer
                .map(|x| x.transpose().expect("lexing should succeed"))
                .collect();
            assert_eq!(
                tokens,
                vec![
                    spanned!(0, Tok::Identifier("a"), 1),
                    spanned!(8, Tok::Identifier("b"), 9),
                    spanned!(17, Tok::Identifier("c"), 18),
                ]
            );
        }

        /// Non-nested multi-line comments work as expected
        #[test]
        fn multiline_comments_are_skipped() {
            let lexer = ZircoLexer::new("a\nb/* abc */c/*\naaa\n*/d");
            let tokens: Vec<_> = lexer
                .map(|x| x.transpose().expect("lexing should succeed"))
                .collect();
            assert_eq!(
                tokens,
                vec![
                    spanned!(0, Tok::Identifier("a"), 1),
                    spanned!(2, Tok::Identifier("b"), 3),
                    spanned!(12, Tok::Identifier("c"), 13),
                    spanned!(22, Tok::Identifier("d"), 23),
                ]
            );
        }

        /// Nested multi-line comments work as expected
        #[test]
        fn nested_multiline_comments_are_skipped() {
            let lexer = ZircoLexer::new("a/* /* */ */b"); // should lex OK
            let tokens: Vec<_> = lexer
                .map(|x| x.transpose().expect("lexing should succeed"))
                .collect();
            assert_eq!(
                tokens,
                vec![
                    spanned!(0, Tok::Identifier("a"), 1),
                    spanned!(12, Tok::Identifier("b"), 13),
                ]
            );
        }

        /// Unclosed nested comments produce the correct error
        #[test]
        fn unclosed_multiline_comments_fail() {
            let lexer = ZircoLexer::new("a /* /*");
            let tokens: Vec<_> = lexer.collect();
            assert_eq!(
                tokens,
                vec![
                    spanned!(0, Ok(Tok::Identifier("a")), 1),
                    spanned!(2, Err(LexicalError::UnterminatedBlockComment), 7),
                ]
            );
        }
    }
}
