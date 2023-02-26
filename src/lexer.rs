use peekmore::{PeekMore, PeekMoreIterator};
use std::str::CharIndices;

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(Debug, Clone, PartialEq)]
pub enum LexicalError {
    IDK((usize, char), String),
    UnterminatedStringLiteral(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Tok {
    // === ARITHMETIC OPERATORS ===
    /// The token `+`
    Plus,
    /// The token `-`
    Minus,
    /// The token `*`
    Star,
    /// The token `/`
    Slash,
    /// The token `%`
    Percent,
    /// The token `++`
    PlusPlus,
    /// The token `--`
    MinusMinus,

    // === COMPARISON OPERATORS ===
    /// The token `==`
    EqEq,
    /// The token `!=`
    NotEq,
    /// The token `>`
    Greater,
    /// The token `>=`
    GreaterEq,
    /// The token `<`
    Less,
    /// The token `<=`
    LessEq,

    // === LOGICAL OPERATORS ===
    /// The token `&&`
    LogicalAnd,
    /// The token `||`
    LogicalOr,
    /// The token `!`
    LogicalNot,

    // === BITWISE OPERATORS ===
    /// The token `&`
    BitwiseAnd,
    /// The token `|`
    BitwiseOr,
    /// The token `^`
    BitwiseXor,
    /// The token `~`
    BitwiseNot,
    /// The token `<<`
    BitwiseLeftShift,
    // FIXME: The lexer could treat Foo<Bar>> as Foo < Bar >>, not Foo < Bar > >.
    //        This is the classic Java generics problem.
    /// The token `>>`
    BitwiseRightShift,

    // === ASSIGNMENT OPERATORS ===
    /// The token `=`
    Assign,
    /// The token `+=`
    PlusAssign,
    /// The token `-=`
    MinusAssign,
    /// The token `*=`
    StarAssign,
    /// The token `/=`
    SlashAssign,
    /// The token `%=`
    PercentAssign,
    /// The token `&=`
    BitwiseAndAssign,
    /// The token `|=`
    BitwiseOrAssign,
    /// The token `^=`
    BitwiseXorAssign,
    /// The token `<<=`
    BitwiseLeftShiftAssign,
    /// The token `>>=`
    BitwiseRightShiftAssign,

    // === OTHER TOKENS ===
    /// The token `;`
    Semicolon,
    /// The token `,`
    Comma,
    /// The token `.`
    Dot,
    /// The token `:`
    Colon,
    /// The token `::`
    ColonColon,
    /// The token `?`
    QuestionMark,

    // === GROUPING ===
    /// The token `(` (left parenthesis)
    LeftParen,
    /// The token `)` (right parenthesis)
    RightParen,
    /// The token `[` (left square bracket)
    LeftBracket,
    /// The token `]` (right square bracket)
    RightBracket,
    /// The token `{` (left curly brace)
    LeftBrace,
    /// The token `}` (right curly brace)
    RightBrace,

    // === KEYWORDS & BUILTINS ===
    /// The boolean `true`
    True, // TODO
    /// The boolean `false`
    False, // TODO

    // === SPECIAL ===
    /// Any string literal
    StringLiteral(String),
    /// Any number literal
    NumberLiteral(String),
    /// Any identifier
    Identifier(String), // TODO
}

pub struct Lexer<'input> {
    chars: PeekMoreIterator<CharIndices<'input>>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer {
            chars: input.char_indices().peekmore(),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Tok, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.chars.next() {
                None => return None, // handle EOF

                // Skip whitespace
                Some((_, ' ')) => continue,
                Some((_, '\n')) => continue,
                Some((_, '\t')) => continue,
                Some((_, '\r')) => continue,

                Some((i, '(')) => return Some(Ok((i, Tok::LeftParen, i + 1))),
                Some((i, ')')) => return Some(Ok((i, Tok::RightParen, i + 1))),
                Some((i, '[')) => return Some(Ok((i, Tok::LeftBracket, i + 1))),
                Some((i, ']')) => return Some(Ok((i, Tok::RightBracket, i + 1))),
                Some((i, '{')) => return Some(Ok((i, Tok::LeftBrace, i + 1))),
                Some((i, '}')) => return Some(Ok((i, Tok::RightBrace, i + 1))),

                Some((i, ';')) => return Some(Ok((i, Tok::LeftParen, i + 1))),
                Some((i, '.')) => return Some(Ok((i, Tok::Dot, i + 1))),
                Some((i, '?')) => return Some(Ok((i, Tok::QuestionMark, i + 1))),
                Some((i, ',')) => return Some(Ok((i, Tok::Comma, i + 1))),

                Some((i, '~')) => return Some(Ok((i, Tok::BitwiseNot, i + 1))),

                Some((i, '!')) if self.chars.peek() == Some(&(i + 1, '=')) => {
                    self.chars.next();
                    return Some(Ok((i, Tok::NotEq, i + 2)));
                }
                Some((i, '!')) => return Some(Ok((i, Tok::LogicalNot, i + 1))),

                Some((i, '=')) if self.chars.peek() == Some(&(i + 1, '=')) => {
                    self.chars.next();
                    return Some(Ok((i, Tok::EqEq, i + 2)));
                }
                Some((i, '=')) => return Some(Ok((i, Tok::Assign, i + 1))),

                // FIXME: I think this changes `:::` into `:: :` which might not be what we want
                Some((i, ':')) if self.chars.peek() == Some(&(i + 1, ':')) => {
                    self.chars.next();
                    return Some(Ok((i, Tok::ColonColon, i + 2)));
                }
                Some((i, ':')) => return Some(Ok((i, Tok::Colon, i + 1))),

                Some((i, '+')) if self.chars.peek() == Some(&(i + 1, '=')) => {
                    self.chars.next();
                    return Some(Ok((i, Tok::PlusAssign, i + 2)));
                }
                Some((i, '+')) if self.chars.peek() == Some(&(i + 1, '+')) => {
                    self.chars.next();
                    return Some(Ok((i, Tok::PlusPlus, i + 2)));
                }
                Some((i, '+')) => return Some(Ok((i, Tok::Plus, i + 1))),

                Some((i, '-')) if self.chars.peek() == Some(&(i + 1, '=')) => {
                    self.chars.next();
                    return Some(Ok((i, Tok::MinusAssign, i + 2)));
                }
                Some((i, '-')) if self.chars.peek() == Some(&(i + 1, '-')) => {
                    self.chars.next();
                    return Some(Ok((i, Tok::MinusMinus, i + 2)));
                }
                Some((i, '-')) => return Some(Ok((i, Tok::Minus, i + 1))),

                Some((i, '*')) if self.chars.peek() == Some(&(i + 1, '=')) => {
                    self.chars.next();
                    return Some(Ok((i, Tok::StarAssign, i + 2)));
                }
                Some((i, '*')) => return Some(Ok((i, Tok::Star, i + 1))),

                Some((i, '/')) if self.chars.peek() == Some(&(i + 1, '=')) => {
                    self.chars.next();
                    return Some(Ok((i, Tok::SlashAssign, i + 2)));
                }
                Some((i, '/')) => return Some(Ok((i, Tok::Slash, i + 1))),

                Some((i, '%')) if self.chars.peek() == Some(&(i + 1, '=')) => {
                    self.chars.next();
                    return Some(Ok((i, Tok::PercentAssign, i + 2)));
                }
                Some((i, '%')) => return Some(Ok((i, Tok::Percent, i + 1))),

                Some((i, '^')) if self.chars.peek() == Some(&(i + 1, '=')) => {
                    self.chars.next();
                    return Some(Ok((i, Tok::BitwiseXorAssign, i + 2)));
                }
                Some((i, '^')) => return Some(Ok((i, Tok::BitwiseXor, i + 1))),

                Some((i, '<')) if self.chars.peek() == Some(&(i + 1, '=')) => {
                    self.chars.next();
                    return Some(Ok((i, Tok::LessEq, i + 2)));
                }
                Some((i, '<')) if self.chars.peek() == Some(&(i + 1, '<')) => {
                    self.chars.next();
                    self.chars.advance_cursor();
                    if let Some((_, '=')) = self.chars.peek() {
                        self.chars.next();
                        return Some(Ok((i, Tok::BitwiseLeftShiftAssign, i + 3)));
                    }
                    self.chars.move_cursor_back().unwrap();
                    return Some(Ok((i, Tok::BitwiseLeftShift, i + 2)));
                }

                Some((i, '<')) => return Some(Ok((i, Tok::Less, i + 1))),

                Some((i, '>')) if self.chars.peek() == Some(&(i + 1, '=')) => {
                    self.chars.next();
                    return Some(Ok((i, Tok::GreaterEq, i + 2)));
                }
                Some((i, '>')) if self.chars.peek() == Some(&(i + 1, '>')) => {
                    self.chars.next();
                    self.chars.advance_cursor();
                    if let Some((_, '=')) = self.chars.peek() {
                        self.chars.next();
                        return Some(Ok((i, Tok::BitwiseRightShiftAssign, i + 3)));
                    }
                    self.chars.move_cursor_back().unwrap();
                    return Some(Ok((i, Tok::BitwiseRightShift, i + 2)));
                }
                Some((i, '>')) => return Some(Ok((i, Tok::Greater, i + 1))),

                Some((i, '&')) if self.chars.peek() == Some(&(i + 1, '=')) => {
                    self.chars.next();
                    return Some(Ok((i, Tok::BitwiseAndAssign, i + 2)));
                }
                Some((i, '&')) if self.chars.peek() == Some(&(i + 1, '&')) => {
                    self.chars.next();
                    return Some(Ok((i, Tok::LogicalAnd, i + 2)));
                }
                Some((i, '&')) => return Some(Ok((i, Tok::BitwiseAnd, i + 1))),

                Some((i, '|')) if self.chars.peek() == Some(&(i + 1, '=')) => {
                    self.chars.next();
                    return Some(Ok((i, Tok::BitwiseOrAssign, i + 2)));
                }
                Some((i, '|')) if self.chars.peek() == Some(&(i + 1, '|')) => {
                    self.chars.next();
                    return Some(Ok((i, Tok::LogicalOr, i + 2)));
                }
                Some((i, '|')) => return Some(Ok((i, Tok::BitwiseOr, i + 1))),

                Some((i, '"')) => {
                    let mut s = String::new();
                    loop {
                        match self.chars.next() {
                            None => return Some(Err(LexicalError::UnterminatedStringLiteral(i))),
                            Some((_, '\\')) => {
                                // Account for this character AND whatever follows it, even if it's a ".
                                s.push('\\');
                                if let Some((_, c)) = self.chars.next() {
                                    s.push(c);
                                } else {
                                    return Some(Err(LexicalError::UnterminatedStringLiteral(i)));
                                }
                            }
                            Some((_, '"')) => return Some(Ok((i, Tok::StringLiteral(s), i + 1))),
                            Some((_, c)) => s.push(c),
                        }
                    }
                }

                Some((i, '0')) => {
                    let base = match self.chars.next() {
                        Some((_, 'x')) => 16,
                        Some((_, 'o')) => 8,
                        Some((_, 'b')) => 2,
                        Some((_, '0'..='9')) => 10,
                        Some((_, c)) => {
                            return Some(Err(LexicalError::IDK(
                                (i, c),
                                format!("I have no idea what the token '{}' should mean!", c),
                            )))
                        }
                        None => return Some(Ok((i, Tok::NumberLiteral("0".to_string()), i + 1))),
                    };
                    let mut s = String::new();
                    s.push('0');
                    while let Some((_, c)) = self.chars.peek() {
                        if c.is_digit(base) {
                            s.push(*c);
                            self.chars.next();
                        } else {
                            break;
                        }
                    }
                }

                Some((i, c)) if c.is_digit(10) => {
                    let mut s = String::new();
                    s.push(c);
                    while let Some((_, c)) = self.chars.peek() {
                        if c.is_digit(10) {
                            s.push(*c);
                            self.chars.next();
                        } else {
                            break;
                        }
                    }
                    return Some(Ok((i, Tok::NumberLiteral(s), i + 1)));
                }

                // TODO: Identifiers
                // TODO: true/false
                Some((i, c)) => {
                    return Some(Err(LexicalError::IDK(
                        (i, c),
                        format!("I have no idea what the token '{}' should mean!", c),
                    )))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Whitespace skipping works as expected
    #[test]
    fn whitespace_skipping() {
        let lexer = Lexer::new(" t\t e \n\ns\nt\r\n  s");
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
}
