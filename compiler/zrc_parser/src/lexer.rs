use logos::{Lexer, Logos};

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(Debug, Clone, PartialEq, Default)]
pub enum LexicalError {
    #[default]
    NoMatchingRule,
    IDK((usize, char), String),
    UnterminatedStringLiteral(usize),
}

fn string_slice(lex: &mut Lexer<'_, Tok>) -> String {
    lex.slice().to_string()
}

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(error = LexicalError)]
pub enum Tok {
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
    /// The token `++`
    #[token("++")]
    PlusPlus,
    /// The token `--`
    #[token("--")]
    MinusMinus,

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
    //        This is the classic Java generics problem.
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
    /// The operator `->`
    #[token("->")]
    SmallArrow,

    // === SPECIAL ===
    /// Any string literal
    #[regex(r#""([^"\\]|\\.)*""#, string_slice)]
    #[regex(r#""([^"\\]|\\.)*"#, |lex| {
        Err(LexicalError::UnterminatedStringLiteral(lex.span().start))
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

    /// Whitespace
    #[regex(r"[ \t\r\n\f]+", logos::skip)]
    // C-style single-line comments
    #[regex(r"//[^\r\n]*(\r\n|\n)?", logos::skip)]
    // C-style multi-line comments
    // TODO: I'm unable to figure out how to implement nested comments:
    // Real behavior:    /* /* abc */ */
    //                   ------------ ^^
    //                   comment      normal tokens
    // Desired behavior: /* /* abc */ */
    //                   -- ^^^^^^^^^ --
    //                   |  \nested-/  |
    //                   \- outer -----/
    #[regex(r"/\*([^*]|\*[^/])+\*/", logos::skip)]
    Ignored,
}

pub struct ZircoLexer<'input> {
    lex: Lexer<'input, Tok>,
}

impl<'input> ZircoLexer<'input> {
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
            Err(LexicalError::NoMatchingRule) => {
                let char = slice.chars().next().unwrap();
                Some(Err(LexicalError::IDK(
                    (span.start, char),
                    format!("Internal error: Unknown token '{char}'"),
                )))
            }
            Err(e) => Some(Err(e)),
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

    /// Simple line comments
    #[test]
    fn comments_1() {
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

    /// Multiline comments
    #[test]
    fn comments_2() {
        let lexer = ZircoLexer::new(concat!("a\n", "/* abc */ b\n", "c /* def */ d",));
        let tokens: Vec<_> = lexer.collect();
        assert_eq!(
            tokens,
            vec![
                Ok((0, Tok::Identifier("a".to_string()), 1)),
                Ok((12, Tok::Identifier("b".to_string()), 13)),
                Ok((14, Tok::Identifier("c".to_string()), 15)),
                Ok((26, Tok::Identifier("d".to_string()), 27))
            ]
        );
    }

    /// Nested comments
    /// This test is to test for a regression reguarding the current behavior
    /// of nested comments. Currently, comment nesting is not supported -- in the future,
    /// we'd like this to be a feature.
    #[test]
    fn comments_3() {
        let lexer = ZircoLexer::new("a/* /* */b"); // should lex OK
        let tokens: Vec<_> = lexer.collect();
        assert_eq!(
            tokens,
            vec![
                Ok((0, Tok::Identifier("a".to_string()), 1)),
                Ok((9, Tok::Identifier("b".to_string()), 10))
            ]
        );
        // Future expected case:
        // a/* /*  */ *b/ with this failing
    }

    // TODO: What does the error look like for an unclosed comment? It might be
    // an ugly IDK() error and I don't like that.
}
