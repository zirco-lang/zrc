//! Lexer and lexical errors
//!
//! This module contains a wrapper around a [logos] lexer that splits an input Zirco text into its individual tokens, which can be then passed into the internal Zirco parser.
//!
//! You do not usually need to use this crate, as the [parser](super::parser) already creates [`ZircoLexer`] instances for you before passing them to the internal parser. However, there are some cases where it may be helpful, so it is kept public.
//!
//! # Example
//! ```
//! use zrc_parser::lexer::{ZircoLexer, Tok};
//! let mut lex = ZircoLexer::new("2 + 2");
//! assert_eq!(lex.next(), Some(Ok((0, Tok::NumberLiteral("2".to_string()), 1))));
//! assert_eq!(lex.next(), Some(Ok((2, Tok::Plus, 3))));
//! assert_eq!(lex.next(), Some(Ok((4, Tok::NumberLiteral("2".to_string()), 5))));
//! assert_eq!(lex.next(), None);
//! ```
//!
//! For more information, read the documentation of [`ZircoLexer`].

use std::fmt::Display;

use logos::{Lexer, Logos};

/// Represents a lexer token within a certain span, or an error.
pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

/// The error enum passed to the internal logos [`Lexer`]. Will be converted to a [`LexicalError`] by [`ZircoLexer`].
///
/// Do not use publicly.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum InternalLexicalError {
    /// A generic lexing error. This is later converted to [`LexicalError::UnknownToken`].
    #[default]
    NoMatchingRule,
    /// A string literal was left unterminated.
    UnterminatedStringLiteral(usize),
    /// A block comment ran to the end of the file. Remind the user that block
    /// comments nest.
    UnterminatedBlockComment,
}

/// An error encountered during lexing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexicalError {
    /// An unknown token was encountered.
    UnknownToken(usize, char),
    /// A string literal was left unterminated.
    UnterminatedStringLiteral(usize),
    /// A block comment ran to the end of the file. Remind the user that block
    /// comments nest.
    UnterminatedBlockComment,
}
impl Display for LexicalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnknownToken(pos, char) => write!(f, "Unknown token '{char}' at position {pos}"),
            Self::UnterminatedStringLiteral(pos) => {
                write!(f, "Unterminated string literal at position {pos}")
            }
            Self::UnterminatedBlockComment => write!(f, "Unterminated block comment"),
        }
    }
}

/// A lexer callback helper to obtain the currently matched token slice.
fn string_slice(lex: &Lexer<'_, Tok>) -> String {
    lex.slice().to_string()
}

/// Zirco uses nested block comments -- a regular expression can't match this
/// without recursion, so our approach is to use a custom callback which takes
/// the lexer and basically consumes characters in our input until we reach the
/// end of the comment. See also: [logos#307](https://github.com/maciejhirsz/logos/issues/307)
/// See also: [zrc#14](https://github.com/zirco-lang/zrc/pull/14)
fn handle_block_comment_start(
    lex: &mut Lexer<'_, Tok>,
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

/// Enum representing all of the result tokens in the Zirco lexer
///
/// Do not use `Tok::lexer` publicly. Use [`ZircoLexer`] instead.
#[derive(Logos, Debug, Clone, PartialEq, Eq)]
#[logos(
    error = InternalLexicalError,
    skip r"[ \t\r\n\f]+",         // whitespace
    skip r"//[^\r\n]*(\r\n|\n)?", // single-line comments
    // multi-line comments are handled by a callback: see handle_block_comment_start.
)]
pub enum Tok {
    // Handle nested block comments -- this does not need its own token type and can be attached
    // to whatever token is directly below this. The handle_block_comment_start will either Skip
    // the matched characters or throw an error. It will never return a token.
    // Read the doc comment above handle_block_comment_start for more information.
    #[token("/*", handle_block_comment_start)]
    // === ARITHMETIC OPERATORS ===
    /// The token `+`
    #[token("+")]
    Plus,
    /// The token `-`s
    #[token("-")]
    Minus,
    /// The token `*`
    #[token("*")]
    Star,
    /// The token `/`
    #[token("/")]
    Slash,
    /// The token `%`
    #[token("%")]
    Percent,

    // === COMPARISON OPERATORS ===
    /// The token `==`
    #[token("==")]
    EqEq,
    /// The token `!=`
    #[token("!=")]
    NotEq,
    /// The token `>`
    #[token(">")]
    Greater,
    /// The token `>=`
    #[token(">=")]
    GreaterEq,
    /// The token `<`
    #[token("<")]
    Less,
    /// The token `<=`
    #[token("<=")]
    LessEq,

    // === LOGICAL OPERATORS ===
    /// The token `&&`
    #[token("&&")]
    LogicalAnd,
    /// The token `||`
    #[token("||")]
    LogicalOr,
    /// The token `!`
    #[token("!")]
    LogicalNot,

    // === BITWISE OPERATORS ===
    /// The token `&`
    #[token("&")]
    BitwiseAnd,
    /// The token `|`
    #[token("|")]
    BitwiseOr,
    /// The token `^`
    #[token("^")]
    BitwiseXor,
    /// The token `~`
    #[token("~")]
    BitwiseNot,
    /// The token `<<`
    #[token("<<")]
    BitwiseLeftShift,
    // FIXME: The lexer could treat Foo<Bar>> as Foo < Bar >>, not Foo < Bar > >.
    //        This is the classic Java generics problem. This might be a Logos
    //        limitation and might require a custom state-machine lexer integrated
    //        into the parser -- we'll see when we get to generics.
    /// The token `>>`
    #[token(">>")]
    BitwiseRightShift,

    // === ASSIGNMENT OPERATORS ===
    /// The token `=`
    #[token("=")]
    Assign,
    /// The token `+=`
    #[token("+=")]
    PlusAssign,
    /// The token `-=`
    #[token("-=")]
    MinusAssign,
    /// The token `*=`
    #[token("*=")]
    StarAssign,
    /// The token `/=`
    #[token("/=")]
    SlashAssign,
    /// The token `%=`
    #[token("%=")]
    PercentAssign,
    /// The token `&=`
    #[token("&=")]
    BitwiseAndAssign,
    /// The token `|=`
    #[token("|=")]
    BitwiseOrAssign,
    /// The token `^=`
    #[token("^=")]
    BitwiseXorAssign,
    /// The token `<<=`
    #[token("<<=")]
    BitwiseLeftShiftAssign,
    /// The token `>>=`
    #[token(">>=")]
    BitwiseRightShiftAssign,

    // === OTHER TOKENS ===
    /// The token `;`
    #[token(";")]
    Semicolon,
    /// The token `,`
    #[token(",")]
    Comma,
    /// The token `.`
    #[token(".")]
    Dot,
    /// The token `:`
    #[token(":")]
    Colon,
    /// The token `::`
    #[token("::")]
    ColonColon,
    /// The token `?`
    #[token("?")]
    QuestionMark,

    // === GROUPING ===
    /// The token `(` (left parenthesis)
    #[token("(")]
    LeftParen,
    /// The token `)` (right parenthesis)
    #[token(")")]
    RightParen,
    /// The token `[` (left square bracket)
    #[token("[")]
    LeftBracket,
    /// The token `]` (right square bracket)
    #[token("]")]
    RightBracket,
    /// The token `{` (left curly brace)
    #[token("{")]
    LeftBrace,
    /// The token `}` (right curly brace)
    #[token("}")]
    RightBrace,

    // === KEYWORDS & BUILTINS ===
    /// The boolean `true`
    #[token("true")]
    True,
    /// The boolean `false`
    #[token("false")]
    False,
    /// The keyword `if`
    #[token("if")]
    If,
    /// The keyword `else`
    #[token("else")]
    Else,
    /// The keyword `while`
    #[token("while")]
    While,
    /// The keyword `for`
    #[token("for")]
    For,
    /// The keyword `break`
    #[token("break")]
    Break,
    /// The keyword `continue`
    #[token("continue")]
    Continue,
    /// The keyword `return`
    #[token("return")]
    Return,
    /// The keyword `let`
    #[token("let")]
    Let,
    /// The keyword `fn`
    #[token("fn")]
    Fn,
    /// The keyword `as`
    #[token("as")]
    As,
    /// The keyword `struct`
    #[token("struct")]
    Struct,
    /// The operator `->`
    #[token("->")]
    SmallArrow,

    // === SPECIAL ===
    /// Any string literal
    #[regex(r#""([^"\\]|\\.)*""#, string_slice)]
    #[regex(r#""([^"\\]|\\.)*"#, |lex| {
        Err(InternalLexicalError::UnterminatedStringLiteral(lex.span().start))
    })]
    StringLiteral(String),
    /// Any number literal
    // FIXME: Do not accept multiple decimal points like "123.456.789"
    #[regex(r"[0-9][0-9\._]*", string_slice)]
    #[regex(r"0x[0-9a-fA-F_]+", string_slice)]
    #[regex(r"0b[01_]+", string_slice)]
    NumberLiteral(String),
    /// Any identifier
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", string_slice)]
    Identifier(String),
}
impl Display for Tok {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::As => "as".to_string(),
                Self::Break => "break".to_string(),
                Self::Assign => "=".to_string(),
                Self::Colon => ":".to_string(),
                Self::ColonColon => "::".to_string(),
                Self::Comma => ",".to_string(),
                Self::Continue => "continue".to_string(),
                Self::Dot => ".".to_string(),
                Self::Else => "else".to_string(),
                Self::False => "false".to_string(),
                Self::Fn => "fn".to_string(),
                Self::For => "for".to_string(),
                Self::If => "if".to_string(),
                Self::LeftBrace => "{".to_string(),
                Self::LeftBracket => "[".to_string(),
                Self::LeftParen => "(".to_string(),
                Self::Let => "let".to_string(),
                Self::LogicalAnd => "&&".to_string(),
                Self::LogicalNot => "!".to_string(),
                Self::LogicalOr => "||".to_string(),
                Self::Minus => "-".to_string(),
                Self::MinusAssign => "-=".to_string(),
                Self::NotEq => "!=".to_string(),
                Self::NumberLiteral(n) => n.clone(),
                Self::Percent => "%".to_string(),
                Self::PercentAssign => "%=".to_string(),
                Self::Plus => "+".to_string(),
                Self::PlusAssign => "+=".to_string(),
                Self::QuestionMark => "?".to_string(),
                Self::Return => "return".to_string(),
                Self::RightBrace => "}".to_string(),
                Self::RightBracket => "]".to_string(),
                Self::RightParen => ")".to_string(),
                Self::Semicolon => ";".to_string(),
                Self::Slash => "/".to_string(),
                Self::SlashAssign => "/=".to_string(),
                Self::SmallArrow => "->".to_string(),
                Self::Star => "*".to_string(),
                Self::StarAssign => "*=".to_string(),
                Self::StringLiteral(s) => s.clone(),
                Self::Struct => "struct".to_string(),
                Self::True => "true".to_string(),
                Self::While => "while".to_string(),
                Self::BitwiseAnd => "&".to_string(),
                Self::BitwiseAndAssign => "&=".to_string(),
                Self::BitwiseLeftShift => "<<".to_string(),
                Self::BitwiseLeftShiftAssign => "<<=".to_string(),
                Self::BitwiseNot => "~".to_string(),
                Self::BitwiseOr => "|".to_string(),
                Self::BitwiseOrAssign => "|=".to_string(),
                Self::BitwiseRightShift => ">>".to_string(),
                Self::BitwiseRightShiftAssign => ">>=".to_string(),
                Self::BitwiseXor => "^".to_string(),
                Self::BitwiseXorAssign => "^=".to_string(),
                Self::EqEq => "==".to_string(),
                Self::Greater => ">".to_string(),
                Self::GreaterEq => ">=".to_string(),
                Self::Less => "<".to_string(),
                Self::LessEq => "<=".to_string(),
                Self::Identifier(i) => i.clone(),
            }
        )
    }
}

/// A lexer for the Zirco programming language
#[allow(clippy::module_name_repetitions)]
pub struct ZircoLexer<'input> {
    /// The internal [`Lexer`] we wrap
    lex: Lexer<'input, Tok>,
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
    type Item = Spanned<Tok, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.lex.next()?;
        let span = self.lex.span();
        let slice = self.lex.slice().to_string();
        match token {
            Err(InternalLexicalError::NoMatchingRule) => {
                let char = slice.chars().next().unwrap();
                Some(Err(LexicalError::UnknownToken(span.start, char)))
            }
            Err(InternalLexicalError::UnterminatedBlockComment) => {
                Some(Err(LexicalError::UnterminatedBlockComment))
            }
            Err(InternalLexicalError::UnterminatedStringLiteral(p)) => {
                Some(Err(LexicalError::UnterminatedStringLiteral(p)))
            }
            Ok(token) => Some(Ok((span.start, token, span.end))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Whitespace skipping works as expected
    #[test]
    fn whitespace_skipping() {
        let lexer = ZircoLexer::new(" t\t e \n\ns\nt\r\n  s");
        let tokens: Vec<_> = lexer.map(|x| x.unwrap().1).collect();
        assert_eq!(
            tokens,
            vec![
                Tok::Identifier("t".to_string()),
                Tok::Identifier("e".to_string()),
                Tok::Identifier("s".to_string()),
                Tok::Identifier("t".to_string()),
                Tok::Identifier("s".to_string()),
            ]
        );
    }

    /// Unclosed strings
    #[test]
    fn unclosed_string() {
        let lexer = ZircoLexer::new("\"abc");
        let tokens: Vec<_> = lexer.collect();
        assert_eq!(
            tokens,
            vec![Err(LexicalError::UnterminatedStringLiteral(0))]
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
            let tokens: Vec<_> = lexer.collect();
            assert_eq!(
                tokens,
                vec![
                    Ok((0, Tok::Identifier("a".to_string()), 1)),
                    Ok((8, Tok::Identifier("b".to_string()), 9)),
                    Ok((17, Tok::Identifier("c".to_string()), 18)),
                ]
            );
        }

        /// Non-nested multi-line comments work as expected
        #[test]
        fn multiline_comments_are_skipped() {
            let lexer = ZircoLexer::new("a\nb/* abc */c/*\naaa\n*/d");
            let tokens: Vec<_> = lexer.collect();
            assert_eq!(
                tokens,
                vec![
                    Ok((0, Tok::Identifier("a".to_string()), 1)),
                    Ok((2, Tok::Identifier("b".to_string()), 3)),
                    Ok((12, Tok::Identifier("c".to_string()), 13)),
                    Ok((22, Tok::Identifier("d".to_string()), 23))
                ]
            );
        }

        /// Nested multi-line comments work as expected
        #[test]
        fn nested_multiline_comments_are_skipped() {
            let lexer = ZircoLexer::new("a/* /* */ */b"); // should lex OK
            let tokens: Vec<_> = lexer.collect();
            assert_eq!(
                tokens,
                vec![
                    Ok((0, Tok::Identifier("a".to_string()), 1)),
                    Ok((12, Tok::Identifier("b".to_string()), 13))
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
                    Ok((0, Tok::Identifier("a".to_string()), 1)),
                    Err(LexicalError::UnterminatedBlockComment)
                ]
            );
        }
    }
}
