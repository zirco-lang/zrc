//! Custom hand-written lexer for Zirco
//!
//! This module contains a hand-written lexer that replaces the logos-based
//! implementation. It manually scans through the input text and produces tokens.

use zrc_utils::span::{Span, Spannable, Spanned};

use crate::lexer::{
    InternalLexicalError, LexicalError, NumberLiteral, StringTok, Tok, ZrcString,
};

/// A custom hand-written lexer for the Zirco programming language
#[derive(Debug, Clone)]
pub struct CustomZircoLexer<'input> {
    /// The input source code
    input: &'input str,
    /// Current position in the input (byte offset)
    pos: usize,
    /// The file name for span creation
    file_name: &'static str,
}

impl<'input> CustomZircoLexer<'input> {
    /// Create a new [`CustomZircoLexer`] given an input string and file name
    #[must_use]
    pub fn new(input: &'input str, file_name: &'static str) -> Self {
        Self {
            input,
            pos: 0,
            file_name,
        }
    }

    /// Peek at the current character without consuming it
    #[must_use]
    fn peek(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    /// Peek ahead n characters
    #[must_use]
    fn peek_ahead(&self, n: usize) -> Option<char> {
        self.input[self.pos..].chars().nth(n)
    }

    /// Check if we're at the end of input
    #[must_use]
    fn is_eof(&self) -> bool {
        self.pos >= self.input.len()
    }

    /// Advance by one character and return it
    fn advance(&mut self) -> Option<char> {
        if self.is_eof() {
            return None;
        }
        
        let ch = self.peek()?;
        self.pos += ch.len_utf8();
        Some(ch)
    }

    /// Skip whitespace characters
    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    /// Try to read a single-line comment, returns true if a comment was consumed
    fn try_read_single_line_comment(&mut self) -> bool {
        if self.peek() == Some('/') && self.peek_ahead(1) == Some('/') {
            // Consume the //
            self.advance();
            self.advance();
            
            // Consume until end of line
            while let Some(ch) = self.peek() {
                if ch == '\n' || ch == '\r' {
                    break;
                }
                self.advance();
            }
            
            return true;
        }
        
        false
    }

    /// Try to read a block comment (with nesting support)
    /// Returns Ok(true) if comment was consumed, Ok(false) if not a comment,
    /// Err if unterminated
    fn try_read_block_comment(&mut self) -> Result<bool, InternalLexicalError> {
        if self.peek() == Some('/') && self.peek_ahead(1) == Some('*') {
            // Consume the /*
            self.advance();
            self.advance();
            
            let mut depth = 1;
            
            while depth > 0 {
                match self.peek() {
                    None => {
                        return Err(InternalLexicalError::UnterminatedBlockComment);
                    }
                    Some('/') => {
                        self.advance();
                        if self.peek() == Some('*') {
                            self.advance();
                            depth += 1;
                        }
                    }
                    Some('*') => {
                        self.advance();
                        if self.peek() == Some('/') {
                            self.advance();
                            depth -= 1;
                        }
                    }
                    Some(_) => {
                        self.advance();
                    }
                }
            }
            
            return Ok(true);
        }
        
        Ok(false)
    }

    /// Read an identifier or keyword starting at current position
    fn read_identifier_or_keyword(&mut self) -> Result<Tok<'input>, InternalLexicalError> {
        let start = self.pos;
        
        // First character must be alphabetic or underscore (already checked by caller)
        self.advance();
        
        // Continue reading alphanumeric or underscore
        while let Some(ch) = self.peek() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                self.advance();
            } else {
                break;
            }
        }
        
        let end = self.pos;
        let text = &self.input[start..end];
        
        // Check if it's a keyword
        let token = match text {
            "true" => Tok::True,
            "false" => Tok::False,
            "if" => Tok::If,
            "else" => Tok::Else,
            "while" => Tok::While,
            "do" => Tok::Do,
            "for" => Tok::For,
            "break" => Tok::Break,
            "continue" => Tok::Continue,
            "return" => Tok::Return,
            "let" => Tok::Let,
            "fn" => Tok::Fn,
            "as" => Tok::As,
            "struct" => Tok::Struct,
            "union" => Tok::Union,
            "enum" => Tok::Enum,
            "match" => Tok::Match,
            "sizeof" => Tok::SizeOf,
            "type" => Tok::Type,
            "switch" => Tok::Switch,
            "default" => Tok::Default,
            "new" => Tok::New,
            "unreachable" => Tok::Unreachable,
            _ => Tok::Identifier(text),
        };
        
        Ok(token)
    }

    /// Read a number literal (decimal, hexadecimal, or binary)
    fn read_number(&mut self) -> Result<Tok<'input>, InternalLexicalError> {
        let start = self.pos;
        
        // Check for hex or binary prefix
        if self.peek() == Some('0') {
            let saved_pos = self.pos;
            self.advance();
            
            if let Some(next_ch) = self.peek() {
                if next_ch == 'x' || next_ch == 'X' {
                    // Hexadecimal
                    self.advance(); // consume 'x'
                    let hex_start = self.pos;
                    
                    while let Some(ch) = self.peek() {
                        if ch.is_ascii_hexdigit() || ch == '_' {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    
                    let hex_end = self.pos;
                    return Ok(Tok::NumberLiteral(NumberLiteral::Hexadecimal(
                        &self.input[hex_start..hex_end],
                    )));
                } else if next_ch == 'b' || next_ch == 'B' {
                    // Binary
                    self.advance(); // consume 'b'
                    let bin_start = self.pos;
                    
                    while let Some(ch) = self.peek() {
                        if ch == '0' || ch == '1' || ch == '_' {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    
                    let bin_end = self.pos;
                    return Ok(Tok::NumberLiteral(NumberLiteral::Binary(
                        &self.input[bin_start..bin_end],
                    )));
                }
            }
            
            // Not hex or binary, restore position and fall through to decimal
            self.pos = saved_pos;
        }
        
        // Decimal number
        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() || ch == '_' || ch == '.' {
                self.advance();
            } else {
                break;
            }
        }
        
        let end = self.pos;
        Ok(Tok::NumberLiteral(NumberLiteral::Decimal(
            &self.input[start..end],
        )))
    }

    /// Read a string literal
    fn read_string(&mut self) -> Result<Tok<'input>, InternalLexicalError> {
        let string_start = self.pos;
        self.advance(); // consume opening quote
        
        let content_start = self.pos;
        let mut escaped = false;
        
        while let Some(ch) = self.peek() {
            if ch == '"' && !escaped {
                // End of string
                let content_end = self.pos;
                self.advance(); // consume closing quote
                
                // Parse the string content
                let content = &self.input[content_start..content_end];
                let string_tokens = self.parse_string_content(content, content_start)?;
                
                return Ok(Tok::StringLiteral(ZrcString(string_tokens)));
            }
            
            if ch == '\\' && !escaped {
                escaped = true;
                self.advance();
            } else {
                escaped = false;
                self.advance();
            }
        }
        
        Err(InternalLexicalError::UnterminatedStringLiteral)
    }

    /// Read a character literal
    fn read_char(&mut self) -> Result<Tok<'input>, InternalLexicalError> {
        self.advance(); // consume opening quote
        
        let content_start = self.pos;
        let mut char_len = 0;
        
        // Read the character (which may be an escape sequence)
        if self.peek() == Some('\\') {
            self.advance();
            char_len += 1;
            if self.peek().is_some() {
                self.advance();
                char_len += 1;
                
                // Handle \xXX and \u{...} which need more characters
                let prev_ch = self.input[self.pos - 1..].chars().next();
                if prev_ch == Some('x') {
                    // \xXX - read 2 hex digits
                    for _ in 0..2 {
                        if self.peek().is_some() {
                            self.advance();
                            char_len += 1;
                        }
                    }
                } else if prev_ch == Some('u') {
                    // \u{...} - read until }
                    if self.peek() == Some('{') {
                        self.advance();
                        char_len += 1;
                        while let Some(ch) = self.peek() {
                            self.advance();
                            char_len += 1;
                            if ch == '}' {
                                break;
                            }
                        }
                    }
                }
            }
        } else if self.peek().is_some() {
            self.advance();
            char_len = 1;
        }
        
        if self.peek() == Some('\'') {
            let content_end = self.pos;
            self.advance(); // consume closing quote
            
            // Parse the character content
            let content = &self.input[content_start..content_end];
            let string_tokens = self.parse_string_content(content, content_start)?;
            
            if string_tokens.len() == 1 {
                return Ok(Tok::CharLiteral(string_tokens[0].clone()));
            }
        }
        
        Err(InternalLexicalError::UnterminatedStringLiteral)
    }

    /// Parse string content into StringTok tokens
    fn parse_string_content(
        &self,
        content: &'input str,
        offset: usize,
    ) -> Result<Vec<StringTok<'input>>, InternalLexicalError> {
        let mut tokens = Vec::new();
        let mut i = 0;
        let bytes = content.as_bytes();
        
        while i < bytes.len() {
            if bytes[i] == b'\\' && i + 1 < bytes.len() {
                // Escape sequence
                i += 1;
                let escape_ch = bytes[i] as char;
                
                let token = match escape_ch {
                    'n' => StringTok::EscapedNewline,
                    'r' => StringTok::EscapedCr,
                    't' => StringTok::EscapedTab,
                    '0' => StringTok::EscapedNull,
                    '\\' => StringTok::EscapedBackslash,
                    '"' => StringTok::EscapedDoubleQuote,
                    'x' => {
                        // \xXX - read two hex digits
                        if i + 2 < bytes.len() {
                            i += 1;
                            let hex_start = i;
                            i += 2;
                            let hex = &content[hex_start..i];
                            StringTok::EscapedHexByte(hex)
                        } else {
                            return Err(InternalLexicalError::UnknownEscapeSequence(
                                Span::from_positions_and_file(
                                    offset + i - 1,
                                    offset + i + 1,
                                    self.file_name,
                                ),
                            ));
                        }
                    }
                    'u' => {
                        // \u{...} - read unicode escape
                        if i + 1 < bytes.len() && bytes[i + 1] == b'{' {
                            i += 2; // skip 'u{'
                            let unicode_start = i;
                            
                            while i < bytes.len() && bytes[i] != b'}' {
                                i += 1;
                            }
                            
                            if i < bytes.len() {
                                let hex = &content[unicode_start..i];
                                i += 1; // skip '}'
                                StringTok::EscapedUnicode(hex)
                            } else {
                                return Err(InternalLexicalError::UnknownEscapeSequence(
                                    Span::from_positions_and_file(
                                        offset + unicode_start - 3,
                                        offset + i,
                                        self.file_name,
                                    ),
                                ));
                            }
                        } else {
                            return Err(InternalLexicalError::UnknownEscapeSequence(
                                Span::from_positions_and_file(
                                    offset + i - 1,
                                    offset + i + 1,
                                    self.file_name,
                                ),
                            ));
                        }
                    }
                    _ => {
                        return Err(InternalLexicalError::UnknownEscapeSequence(
                            Span::from_positions_and_file(
                                offset + i - 1,
                                offset + i + 1,
                                self.file_name,
                            ),
                        ));
                    }
                };
                
                tokens.push(token);
                i += 1;
            } else {
                // Regular character
                let ch_start = i;
                let ch = content[i..].chars().next().expect("should have character");
                i += ch.len_utf8();
                let text = &content[ch_start..i];
                tokens.push(StringTok::Text(text));
            }
        }
        
        Ok(tokens)
    }

    /// Read an operator or punctuation token
    fn read_operator_or_punctuation(&mut self) -> Result<Tok<'input>, InternalLexicalError> {
        let ch = self.advance().ok_or(InternalLexicalError::NoMatchingRule)?;
        
        let token = match ch {
            '+' => {
                if self.peek() == Some('+') {
                    self.advance();
                    Tok::PlusPlus
                } else if self.peek() == Some('=') {
                    self.advance();
                    Tok::PlusAssign
                } else {
                    Tok::Plus
                }
            }
            '-' => {
                if self.peek() == Some('-') {
                    self.advance();
                    Tok::MinusMinus
                } else if self.peek() == Some('=') {
                    self.advance();
                    Tok::MinusAssign
                } else if self.peek() == Some('>') {
                    self.advance();
                    Tok::SmallArrow
                } else {
                    Tok::Minus
                }
            }
            '*' => {
                if self.peek() == Some('=') {
                    self.advance();
                    Tok::StarAssign
                } else {
                    Tok::Star
                }
            }
            '/' => {
                if self.peek() == Some('=') {
                    self.advance();
                    Tok::SlashAssign
                } else {
                    Tok::Slash
                }
            }
            '%' => {
                if self.peek() == Some('=') {
                    self.advance();
                    Tok::PercentAssign
                } else {
                    Tok::Percent
                }
            }
            '=' => {
                if self.peek() == Some('=') {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        return Err(InternalLexicalError::JavascriptUserDetected("=="));
                    }
                    Tok::EqEq
                } else if self.peek() == Some('>') {
                    self.advance();
                    Tok::FatArrow
                } else {
                    Tok::Assign
                }
            }
            '!' => {
                if self.peek() == Some('=') {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        return Err(InternalLexicalError::JavascriptUserDetected("!="));
                    }
                    Tok::NotEq
                } else {
                    Tok::LogicalNot
                }
            }
            '>' => {
                if self.peek() == Some('=') {
                    self.advance();
                    Tok::GreaterEq
                } else if self.peek() == Some('>') {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        Tok::BitwiseRightShiftAssign
                    } else {
                        Tok::BitwiseRightShift
                    }
                } else {
                    Tok::Greater
                }
            }
            '<' => {
                if self.peek() == Some('=') {
                    self.advance();
                    Tok::LessEq
                } else if self.peek() == Some('<') {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        Tok::BitwiseLeftShiftAssign
                    } else {
                        Tok::BitwiseLeftShift
                    }
                } else {
                    Tok::Less
                }
            }
            '&' => {
                if self.peek() == Some('&') {
                    self.advance();
                    Tok::LogicalAnd
                } else if self.peek() == Some('=') {
                    self.advance();
                    Tok::BitwiseAndAssign
                } else {
                    Tok::BitwiseAnd
                }
            }
            '|' => {
                if self.peek() == Some('|') {
                    self.advance();
                    Tok::LogicalOr
                } else if self.peek() == Some('=') {
                    self.advance();
                    Tok::BitwiseOrAssign
                } else {
                    Tok::BitwiseOr
                }
            }
            '^' => {
                if self.peek() == Some('=') {
                    self.advance();
                    Tok::BitwiseXorAssign
                } else {
                    Tok::BitwiseXor
                }
            }
            '~' => Tok::BitwiseNot,
            ';' => Tok::Semicolon,
            ',' => Tok::Comma,
            '.' => {
                if self.peek() == Some('.') && self.peek_ahead(1) == Some('.') {
                    self.advance();
                    self.advance();
                    Tok::Ellipsis
                } else {
                    Tok::Dot
                }
            }
            ':' => {
                if self.peek() == Some(':') {
                    self.advance();
                    Tok::ColonColon
                } else {
                    Tok::Colon
                }
            }
            '?' => Tok::QuestionMark,
            '(' => Tok::LeftParen,
            ')' => Tok::RightParen,
            '[' => Tok::LeftBracket,
            ']' => Tok::RightBracket,
            '{' => Tok::LeftBrace,
            '}' => Tok::RightBrace,
            _ => return Err(InternalLexicalError::NoMatchingRule),
        };
        
        Ok(token)
    }

    /// Read the next token from the input
    fn next_token(&mut self) -> Option<(usize, Result<Tok<'input>, InternalLexicalError>, usize)> {
        // Skip whitespace and comments
        loop {
            self.skip_whitespace();
            
            if self.try_read_single_line_comment() {
                continue;
            }
            
            match self.try_read_block_comment() {
                Ok(true) => continue,
                Ok(false) => break,
                Err(e) => {
                    let start = self.pos;
                    let end = self.input.len();
                    return Some((start, Err(e), end));
                }
            }
        }
        
        if self.is_eof() {
            return None;
        }
        
        let start_pos = self.pos;
        let ch = self.peek()?;
        
        // Try to read different token types
        let result = if ch.is_ascii_alphabetic() || ch == '_' {
            self.read_identifier_or_keyword()
        } else if ch.is_ascii_digit() {
            self.read_number()
        } else if ch == '"' {
            self.read_string()
        } else if ch == '\'' {
            self.read_char()
        } else {
            // Operators and punctuation
            self.read_operator_or_punctuation()
        };
        
        let end_pos = self.pos;
        
        Some((start_pos, result, end_pos))
    }
}

impl<'input> Iterator for CustomZircoLexer<'input> {
    type Item = Spanned<Result<Tok<'input>, LexicalError<'input>>>;

    fn next(&mut self) -> Option<Self::Item> {
        let (start_pos, token_result, end_pos) = self.next_token()?;
        
        let span = Span::from_positions_and_file(start_pos, end_pos, self.file_name);
        
        Some(
            token_result
                .map_err(|err| match err {
                    InternalLexicalError::NoMatchingRule => {
                        LexicalError::UnknownToken(&self.input[start_pos..end_pos])
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
    use super::*;
    use zrc_utils::spanned;

    #[test]
    fn whitespace_should_be_skipped() {
        let lexer = CustomZircoLexer::new(" t\t e \n\ns\nt\r\n  s", "<test>");
        let tokens: Vec<_> = lexer
            .map(|x| x.transpose().expect("lexing should succeed"))
            .collect();
        assert_eq!(
            tokens,
            vec![
                spanned!(1, Tok::Identifier("t"), 2, "<test>"),
                spanned!(4, Tok::Identifier("e"), 5, "<test>"),
                spanned!(8, Tok::Identifier("s"), 9, "<test>"),
                spanned!(10, Tok::Identifier("t"), 11, "<test>"),
                spanned!(15, Tok::Identifier("s"), 16, "<test>"),
            ]
        );
    }

    #[test]
    fn simple_tokens() {
        let lexer = CustomZircoLexer::new("+ - * /", "<test>");
        let tokens: Vec<_> = lexer
            .map(|x| x.transpose().expect("lexing should succeed").into_value())
            .collect();
        
        assert_eq!(
            tokens,
            vec![Tok::Plus, Tok::Minus, Tok::Star, Tok::Slash]
        );
    }

    #[test]
    fn keywords() {
        let lexer = CustomZircoLexer::new("if else while", "<test>");
        let tokens: Vec<_> = lexer
            .map(|x| x.transpose().expect("lexing should succeed").into_value())
            .collect();
        
        assert_eq!(tokens, vec![Tok::If, Tok::Else, Tok::While]);
    }

    #[test]
    fn identifiers() {
        let lexer = CustomZircoLexer::new("foo bar baz123", "<test>");
        let tokens: Vec<_> = lexer
            .map(|x| x.transpose().expect("lexing should succeed").into_value())
            .collect();
        
        assert_eq!(
            tokens,
            vec![
                Tok::Identifier("foo"),
                Tok::Identifier("bar"),
                Tok::Identifier("baz123")
            ]
        );
    }

    #[test]
    fn single_line_comments_are_skipped() {
        let lexer = CustomZircoLexer::new(
            concat!("a\n", "//abc\n", "b\n", "// def\n", "c // ghi\n", "// jkl",),
            "<test>",
        );
        let tokens: Vec<_> = lexer
            .map(|x| x.transpose().expect("lexing should succeed"))
            .collect();
        assert_eq!(
            tokens,
            vec![
                spanned!(0, Tok::Identifier("a"), 1, "<test>"),
                spanned!(8, Tok::Identifier("b"), 9, "<test>"),
                spanned!(17, Tok::Identifier("c"), 18, "<test>"),
            ]
        );
    }

    #[test]
    fn multiline_comments_are_skipped() {
        let lexer = CustomZircoLexer::new("a\nb/* abc */c/*\naaa\n*/d", "<test>");
        let tokens: Vec<_> = lexer
            .map(|x| x.transpose().expect("lexing should succeed"))
            .collect();
        assert_eq!(
            tokens,
            vec![
                spanned!(0, Tok::Identifier("a"), 1, "<test>"),
                spanned!(2, Tok::Identifier("b"), 3, "<test>"),
                spanned!(12, Tok::Identifier("c"), 13, "<test>"),
                spanned!(22, Tok::Identifier("d"), 23, "<test>"),
            ]
        );
    }

    #[test]
    fn nested_multiline_comments_are_skipped() {
        let lexer = CustomZircoLexer::new("a/* /* */ */b", "<test>");
        let tokens: Vec<_> = lexer
            .map(|x| x.transpose().expect("lexing should succeed"))
            .collect();
        assert_eq!(
            tokens,
            vec![
                spanned!(0, Tok::Identifier("a"), 1, "<test>"),
                spanned!(12, Tok::Identifier("b"), 13, "<test>"),
            ]
        );
    }

    #[test]
    fn unclosed_multiline_comments_fail() {
        let lexer = CustomZircoLexer::new("a /* /*", "<test>");
        let tokens: Vec<_> = lexer.collect();
        assert_eq!(
            tokens,
            vec![
                spanned!(0, Ok(Tok::Identifier("a")), 1, "<test>"),
                spanned!(2, Err(LexicalError::UnterminatedBlockComment), 7, "<test>"),
            ]
        );
    }
}
