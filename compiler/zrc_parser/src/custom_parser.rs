//! Custom hand-written recursive descent parser for Zirco
//!
//! This module contains a hand-written recursive descent parser that replaces
//! the LALRPOP-generated parser.

use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_utils::span::{Span, Spannable, Spanned};

use crate::{
    ast::{
        expr::*,
        stmt::*,
        ty::*,
    },
    lexer::{LexicalError, NumberLiteral, Tok},
};

/// Parser error type
#[derive(Debug, Clone, PartialEq)]
pub enum ParseError<'input> {
    /// Unexpected end of file
    UnexpectedEof {
        /// Expected tokens
        expected: Vec<String>,
    },
    /// Unexpected token
    UnexpectedToken {
        /// The token that was found
        found: String,
        /// Expected tokens  
        expected: Vec<String>,
    },
    /// Lexical error
    LexicalError(LexicalError<'input>),
}

/// Recursive descent parser for Zirco
pub struct CustomParser<'input> {
    /// The tokens to parse
    tokens: Vec<Spanned<Result<Tok<'input>, LexicalError<'input>>>>,
    /// Current position in token stream
    pos: usize,
    /// File name for error reporting
    file_name: &'static str,
}

impl<'input> CustomParser<'input> {
    /// Create a new parser from a token iterator
    pub fn new(
        tokens: impl Iterator<Item = Spanned<Result<Tok<'input>, LexicalError<'input>>>>,
        file_name: &'static str,
    ) -> Self {
        Self {
            tokens: tokens.collect(),
            pos: 0,
            file_name,
        }
    }

    /// Peek at the current token without consuming it
    fn peek(&self) -> Option<&Spanned<Result<Tok<'input>, LexicalError<'input>>>> {
        self.tokens.get(self.pos)
    }

    /// Peek at the token kind without error handling
    fn peek_kind(&self) -> Option<&Tok<'input>> {
        self.peek().and_then(|t| t.value().ok())
    }

    /// Check if we're at the end of input
    fn is_eof(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    /// Advance to the next token and return the current one
    fn advance(&mut self) -> Option<Spanned<Result<Tok<'input>, LexicalError<'input>>>> {
        if self.is_eof() {
            return None;
        }
        
        let token = self.tokens[self.pos].clone();
        self.pos += 1;
        Some(token)
    }

    /// Expect a specific token kind, consume it if present
    fn expect(&mut self, expected: &Tok<'input>) -> Result<Spanned<Tok<'input>>, ParseError<'input>> {
        let token = self.advance().ok_or_else(|| ParseError::UnexpectedEof {
            expected: vec![format!("{expected}")],
        })?;
        
        match token.value() {
            Ok(tok) if std::mem::discriminant(tok) == std::mem::discriminant(expected) => {
                Ok(token.map(|t| t.expect("already checked")))
            }
            Ok(tok) => Err(ParseError::UnexpectedToken {
                found: tok.to_string(),
                expected: vec![format!("{expected}")],
            }),
            Err(e) => Err(ParseError::LexicalError(e.clone())),
        }
    }

    /// Try to consume a specific token, return true if consumed
    fn try_consume(&mut self, expected: &Tok<'input>) -> bool {
        if let Some(Ok(tok)) = self.peek_kind() {
            if std::mem::discriminant(tok) == std::mem::discriminant(expected) {
                self.advance();
                return true;
            }
        }
        false
    }

    /// Get current position for span creation
    fn current_pos(&self) -> usize {
        self.peek()
            .map(|t| t.span().start())
            .unwrap_or(self.tokens.last().map_or(0, |t| t.span().end()))
    }

    /// Create a span from start position to current position
    fn span_from(&self, start: usize) -> Span {
        let end = self.tokens.get(self.pos.saturating_sub(1))
            .map_or(start, |t| t.span().end());
        Span::from_positions_and_file(start, end, self.file_name)
    }

    /// Parse a complete program (list of declarations)
    pub fn parse_program(&mut self) -> Result<Vec<Spanned<Declaration<'input>>>, ParseError<'input>> {
        let mut declarations = Vec::new();
        
        while !self.is_eof() {
            declarations.push(self.parse_declaration()?);
        }
        
        Ok(declarations)
    }

    /// Parse a single declaration
    fn parse_declaration(&mut self) -> Result<Spanned<Declaration<'input>>, ParseError<'input>> {
        let start = self.current_pos();
        
        match self.peek_kind() {
            Some(Tok::Fn) => {
                let decl = self.parse_function_declaration()?;
                Ok(decl.in_span(self.span_from(start)))
            }
            Some(Tok::Type) => {
                let decl = self.parse_type_alias_declaration()?;
                Ok(decl.in_span(self.span_from(start)))
            }
            Some(Tok::Struct) | Some(Tok::Union) | Some(Tok::Enum) => {
                let decl = self.parse_struct_union_enum_declaration()?;
                Ok(decl.in_span(self.span_from(start)))
            }
            Some(Tok::Let) => {
                let let_decl = self.parse_let_declaration()?;
                Ok(Declaration::GlobalLetDeclaration(let_decl).in_span(self.span_from(start)))
            }
            _ => {
                Err(ParseError::UnexpectedToken {
                    found: self.peek_kind().map_or("EOF".to_string(), |t| t.to_string()),
                    expected: vec!["fn".to_string(), "type".to_string(), "struct".to_string(), "let".to_string()],
                })
            }
        }
    }

    /// Parse a function declaration
    fn parse_function_declaration(&mut self) -> Result<Declaration<'input>, ParseError<'input>> {
        self.expect(&Tok::Fn)?;
        
        let name_token = self.advance().ok_or_else(|| ParseError::UnexpectedEof {
            expected: vec!["identifier".to_string()],
        })?;
        
        let name = match name_token.value() {
            Ok(Tok::Identifier(id)) => name_token.map(|_| *id),
            _ => return Err(ParseError::UnexpectedToken {
                found: format!("{name_token:?}"),
                expected: vec!["identifier".to_string()],
            }),
        };
        
        self.expect(&Tok::LeftParen)?;
        let params_start = self.current_pos();
        let parameters = if self.try_consume(&Tok::RightParen) {
            ArgumentDeclarationList::empty().in_span(Span::from_positions_and_file(params_start, params_start, self.file_name))
        } else {
            let params = self.parse_argument_declaration_list()?;
            self.expect(&Tok::RightParen)?;
            params.in_span(self.span_from(params_start))
        };
        
        let return_type = if self.try_consume(&Tok::SmallArrow) {
            Some(self.parse_type()?)
        } else {
            None
        };
        
        let body = if self.peek_kind() == Some(&Tok::Semicolon) {
            self.advance();
            None
        } else {
            self.expect(&Tok::LeftBrace)?;
            let body_start = self.current_pos();
            let stmts = if self.peek_kind() == Some(&Tok::RightBrace) {
                Vec::new()
            } else {
                self.parse_stmt_list()?
            };
            self.expect(&Tok::RightBrace)?;
            Some(stmts.in_span(self.span_from(body_start)))
        };
        
        Ok(Declaration::FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
        })
    }

    /// Parse an argument declaration list
    fn parse_argument_declaration_list(&mut self) -> Result<ArgumentDeclarationList<'input>, ParseError<'input>> {
        let mut args = Vec::new();
        
        loop {
            let arg_start = self.current_pos();
            
            // Check for ellipsis (variadic)
            if self.try_consume(&Tok::Ellipsis) {
                return Ok(ArgumentDeclarationList::Variadic(args));
            }
            
            // Parse argument name
            let name_token = self.advance().ok_or_else(|| ParseError::UnexpectedEof {
                expected: vec!["identifier".to_string()],
            })?;
            
            let name = match name_token.value() {
                Ok(Tok::Identifier(id)) => name_token.map(|_| *id),
                _ => return Err(ParseError::UnexpectedToken {
                    found: format!("{name_token:?}"),
                    expected: vec!["identifier".to_string()],
                }),
            };
            
            self.expect(&Tok::Colon)?;
            let ty = self.parse_type()?;
            
            args.push(ArgumentDeclaration { name, ty }.in_span(self.span_from(arg_start)));
            
            if !self.try_consume(&Tok::Comma) {
                break;
            }
            
            // Check if ellipsis follows comma
            if self.peek_kind() == Some(&Tok::Ellipsis) {
                self.advance();
                return Ok(ArgumentDeclarationList::Variadic(args));
            }
        }
        
        Ok(ArgumentDeclarationList::NonVariadic(args))
    }

    /// Parse a type alias declaration
    fn parse_type_alias_declaration(&mut self) -> Result<Declaration<'input>, ParseError<'input>> {
        self.expect(&Tok::Type)?;
        
        let name_token = self.advance().ok_or_else(|| ParseError::UnexpectedEof {
            expected: vec!["identifier".to_string()],
        })?;
        
        let name = match name_token.value() {
            Ok(Tok::Identifier(id)) => name_token.map(|_| *id),
            _ => return Err(ParseError::UnexpectedToken {
                found: format!("{name_token:?}"),
                expected: vec!["identifier".to_string()],
            }),
        };
        
        self.expect(&Tok::Assign)?;
        let ty = self.parse_type_in_declaration()?;
        self.expect(&Tok::Semicolon)?;
        
        Ok(Declaration::TypeAliasDeclaration { name, ty })
    }

    /// Parse struct/union/enum declaration
    fn parse_struct_union_enum_declaration(&mut self) -> Result<Declaration<'input>, ParseError<'input>> {
        let start = self.current_pos();
        
        let kind_token = self.advance().expect("checked by caller");
        let kind = kind_token.value().expect("checked by caller");
        
        let name_token = self.advance().ok_or_else(|| ParseError::UnexpectedEof {
            expected: vec!["identifier".to_string()],
        })?;
        
        let name = match name_token.value() {
            Ok(Tok::Identifier(id)) => name_token.map(|_| *id),
            _ => return Err(ParseError::UnexpectedToken {
                found: format!("{name_token:?}"),
                expected: vec!["identifier".to_string()],
            }),
        };
        
        let mapping = self.parse_key_type_mapping()?;
        
        let ty_kind = match kind {
            Tok::Struct => TypeKind::Struct(mapping),
            Tok::Union => TypeKind::Union(mapping),
            Tok::Enum => TypeKind::Enum(mapping),
            _ => unreachable!("already checked"),
        };
        
        Ok(Declaration::TypeAliasDeclaration {
            name,
            ty: Type(ty_kind.in_span(self.span_from(start))),
        })
    }

    /// Parse key-type mapping for struct/union/enum
    fn parse_key_type_mapping(&mut self) -> Result<KeyTypeMapping<'input>, ParseError<'input>> {
        self.expect(&Tok::LeftBrace)?;
        
        let list_start = self.current_pos();
        let mut fields = Vec::new();
        
        while self.peek_kind() != Some(&Tok::RightBrace) {
            let field_start = self.current_pos();
            
            let field_name_token = self.advance().ok_or_else(|| ParseError::UnexpectedEof {
                expected: vec!["identifier".to_string()],
            })?;
            
            let field_name = match field_name_token.value() {
                Ok(Tok::Identifier(id)) => field_name_token.map(|_| *id),
                _ => return Err(ParseError::UnexpectedToken {
                    found: format!("{field_name_token:?}"),
                    expected: vec!["identifier".to_string()],
                }),
            };
            
            self.expect(&Tok::Colon)?;
            let field_type = self.parse_type_in_declaration()?;
            
            fields.push((field_name, field_type).in_span(self.span_from(field_start)));
            
            if !self.try_consume(&Tok::Comma) {
                break;
            }
        }
        
        self.expect(&Tok::RightBrace)?;
        
        Ok(KeyTypeMapping(fields.in_span(self.span_from(list_start))))
    }

    /// Parse a type (with parentheses support in declaration contexts)
    fn parse_type_in_declaration(&mut self) -> Result<Type<'input>, ParseError<'input>> {
        if self.try_consume(&Tok::LeftParen) {
            let ty = self.parse_type_in_declaration()?;
            self.expect(&Tok::RightParen)?;
            Ok(ty)
        } else {
            self.parse_type()
        }
    }

    /// Parse a type
    fn parse_type(&mut self) -> Result<Type<'input>, ParseError<'input>> {
        let start = self.current_pos();
        
        match self.peek_kind() {
            Some(Tok::Identifier(_)) => {
                let id_token = self.advance().expect("just peeked");
                let id = match id_token.value() {
                    Ok(Tok::Identifier(name)) => id_token.map(|_| *name),
                    _ => unreachable!(),
                };
                Ok(Type(TypeKind::Identifier(id).in_span(self.span_from(start))))
            }
            Some(Tok::Star) => {
                self.advance();
                let inner = self.parse_type_or_parenthesized()?;
                Ok(Type(TypeKind::Ptr(Box::new(inner)).in_span(self.span_from(start))))
            }
            Some(Tok::Struct) => {
                self.advance();
                let mapping = self.parse_key_type_mapping()?;
                Ok(Type(TypeKind::Struct(mapping).in_span(self.span_from(start))))
            }
            Some(Tok::Union) => {
                self.advance();
                let mapping = self.parse_key_type_mapping()?;
                Ok(Type(TypeKind::Union(mapping).in_span(self.span_from(start))))
            }
            Some(Tok::Enum) => {
                self.advance();
                let mapping = self.parse_key_type_mapping()?;
                Ok(Type(TypeKind::Enum(mapping).in_span(self.span_from(start))))
            }
            _ => Err(ParseError::UnexpectedToken {
                found: self.peek_kind().map_or("EOF".to_string(), |t| t.to_string()),
                expected: vec!["type".to_string()],
            }),
        }
    }

    /// Parse a type or parenthesized type
    fn parse_type_or_parenthesized(&mut self) -> Result<Type<'input>, ParseError<'input>> {
        if self.try_consume(&Tok::LeftParen) {
            let ty = self.parse_type_or_parenthesized()?;
            self.expect(&Tok::RightParen)?;
            Ok(ty)
        } else {
            self.parse_type()
        }
    }

    /// Parse a let declaration
    fn parse_let_declaration(&mut self) -> Result<Spanned<Vec<Spanned<LetDeclaration<'input>>>>, ParseError<'input>> {
        let start = self.current_pos();
        self.expect(&Tok::Let)?;
        
        let mut declarations = Vec::new();
        
        loop {
            let decl_start = self.current_pos();
            
            let name_token = self.advance().ok_or_else(|| ParseError::UnexpectedEof {
                expected: vec!["identifier".to_string()],
            })?;
            
            let name = match name_token.value() {
                Ok(Tok::Identifier(id)) => name_token.map(|_| *id),
                _ => return Err(ParseError::UnexpectedToken {
                    found: format!("{name_token:?}"),
                    expected: vec!["identifier".to_string()],
                }),
            };
            
            let ty = if self.try_consume(&Tok::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };
            
            let value = if self.try_consume(&Tok::Assign) {
                Some(self.parse_assignment_expr()?)
            } else {
                None
            };
            
            declarations.push(LetDeclaration { name, ty, value }.in_span(self.span_from(decl_start)));
            
            if !self.try_consume(&Tok::Comma) {
                break;
            }
        }
        
        self.expect(&Tok::Semicolon)?;
        
        Ok(declarations.in_span(self.span_from(start)))
    }

    /// Parse a statement list
    pub fn parse_stmt_list(&mut self) -> Result<Vec<Stmt<'input>>, ParseError<'input>> {
        let mut stmts = Vec::new();
        
        while !self.is_eof() && self.peek_kind() != Some(&Tok::RightBrace) {
            stmts.push(self.parse_stmt()?);
        }
        
        Ok(stmts)
    }

    /// Parse a statement
    fn parse_stmt(&mut self) -> Result<Stmt<'input>, ParseError<'input>> {
        let start = self.current_pos();
        
        // Try to parse open or closed statement
        let stmt_kind = self.parse_stmt_kind()?;
        
        Ok(Stmt(stmt_kind.in_span(self.span_from(start))))
    }

    /// Parse statement kind (handles dangling else)
    fn parse_stmt_kind(&mut self) -> Result<StmtKind<'input>, ParseError<'input>> {
        match self.peek_kind() {
            Some(Tok::If) => self.parse_if_stmt(),
            Some(Tok::While) => self.parse_while_stmt(),
            Some(Tok::Do) => self.parse_do_while_stmt(),
            Some(Tok::For) => self.parse_for_stmt(),
            _ => self.parse_simple_stmt(),
        }
    }

    /// Parse an if statement (handles dangling else)
    fn parse_if_stmt(&mut self) -> Result<StmtKind<'input>, ParseError<'input>> {
        self.expect(&Tok::If)?;
        self.expect(&Tok::LeftParen)?;
        let condition = self.parse_expr()?;
        self.expect(&Tok::RightParen)?;
        
        let then_branch = Box::new(self.parse_stmt()?);
        
        let else_branch = if self.try_consume(&Tok::Else) {
            Some(Box::new(self.parse_stmt()?))
        } else {
            None
        };
        
        Ok(StmtKind::IfStmt(condition, then_branch, else_branch))
    }

    /// Parse a while statement
    fn parse_while_stmt(&mut self) -> Result<StmtKind<'input>, ParseError<'input>> {
        self.expect(&Tok::While)?;
        self.expect(&Tok::LeftParen)?;
        let condition = self.parse_expr()?;
        self.expect(&Tok::RightParen)?;
        let body = Box::new(self.parse_stmt()?);
        
        Ok(StmtKind::WhileStmt(condition, body))
    }

    /// Parse a do-while statement
    fn parse_do_while_stmt(&mut self) -> Result<StmtKind<'input>, ParseError<'input>> {
        self.expect(&Tok::Do)?;
        let body = Box::new(self.parse_stmt()?);
        self.expect(&Tok::While)?;
        self.expect(&Tok::LeftParen)?;
        let condition = self.parse_expr()?;
        self.expect(&Tok::RightParen)?;
        self.expect(&Tok::Semicolon)?;
        
        Ok(StmtKind::DoWhileStmt(body, condition))
    }

    /// Parse a for statement
    fn parse_for_stmt(&mut self) -> Result<StmtKind<'input>, ParseError<'input>> {
        self.expect(&Tok::For)?;
        self.expect(&Tok::LeftParen)?;
        
        let init = if self.peek_kind() == Some(&Tok::Let) {
            Some(Box::new(self.parse_let_declaration()?))
        } else if self.try_consume(&Tok::Semicolon) {
            None
        } else {
            return Err(ParseError::UnexpectedToken {
                found: self.peek_kind().map_or("EOF".to_string(), |t| t.to_string()),
                expected: vec!["let".to_string(), ";".to_string()],
            });
        };
        
        let cond = if self.peek_kind() == Some(&Tok::Semicolon) {
            None
        } else {
            Some(self.parse_expr()?)
        };
        self.expect(&Tok::Semicolon)?;
        
        let post = if self.peek_kind() == Some(&Tok::RightParen) {
            None
        } else {
            Some(self.parse_expr()?)
        };
        self.expect(&Tok::RightParen)?;
        
        let body = Box::new(self.parse_stmt()?);
        
        Ok(StmtKind::ForStmt { init, cond, post, body })
    }

    /// Parse a simple statement
    fn parse_simple_stmt(&mut self) -> Result<StmtKind<'input>, ParseError<'input>> {
        match self.peek_kind() {
            Some(Tok::Semicolon) => {
                self.advance();
                Ok(StmtKind::EmptyStmt)
            }
            Some(Tok::LeftBrace) => {
                self.advance();
                let stmts = if self.peek_kind() == Some(&Tok::RightBrace) {
                    Vec::new()
                } else {
                    self.parse_stmt_list()?
                };
                self.expect(&Tok::RightBrace)?;
                Ok(StmtKind::BlockStmt(stmts))
            }
            Some(Tok::Break) => {
                self.advance();
                self.expect(&Tok::Semicolon)?;
                Ok(StmtKind::BreakStmt)
            }
            Some(Tok::Continue) => {
                self.advance();
                self.expect(&Tok::Semicolon)?;
                Ok(StmtKind::ContinueStmt)
            }
            Some(Tok::Return) => {
                self.advance();
                let value = if self.peek_kind() == Some(&Tok::Semicolon) {
                    None
                } else {
                    Some(self.parse_expr()?)
                };
                self.expect(&Tok::Semicolon)?;
                Ok(StmtKind::ReturnStmt(value))
            }
            Some(Tok::Unreachable) => {
                self.advance();
                self.expect(&Tok::Semicolon)?;
                Ok(StmtKind::UnreachableStmt)
            }
            Some(Tok::Switch) => self.parse_switch_stmt(),
            Some(Tok::Match) => self.parse_match_stmt(),
            Some(Tok::Let) => {
                let decl = self.parse_let_declaration()?;
                Ok(StmtKind::DeclarationList(decl))
            }
            _ => {
                // Expression statement
                let expr = self.parse_expr()?;
                self.expect(&Tok::Semicolon)?;
                Ok(StmtKind::ExprStmt(expr))
            }
        }
    }

    /// Parse a switch statement
    fn parse_switch_stmt(&mut self) -> Result<StmtKind<'input>, ParseError<'input>> {
        self.expect(&Tok::Switch)?;
        self.expect(&Tok::LeftParen)?;
        let scrutinee = self.parse_expr()?;
        self.expect(&Tok::RightParen)?;
        self.expect(&Tok::LeftBrace)?;
        
        let mut cases = Vec::new();
        
        while self.peek_kind() != Some(&Tok::RightBrace) {
            let case_start = self.current_pos();
            
            let trigger = if self.try_consume(&Tok::Default) {
                SwitchTrigger::Default
            } else {
                let expr = self.parse_expr()?;
                SwitchTrigger::Expr(expr)
            };
            
            self.expect(&Tok::FatArrow)?;
            let body = self.parse_stmt()?;
            
            cases.push(SwitchCase(trigger, body).in_span(self.span_from(case_start)));
        }
        
        self.expect(&Tok::RightBrace)?;
        
        Ok(StmtKind::SwitchCase { scrutinee, cases })
    }

    /// Parse a match statement
    fn parse_match_stmt(&mut self) -> Result<StmtKind<'input>, ParseError<'input>> {
        self.expect(&Tok::Match)?;
        self.expect(&Tok::LeftParen)?;
        let scrutinee = self.parse_expr()?;
        self.expect(&Tok::RightParen)?;
        self.expect(&Tok::LeftBrace)?;
        
        let mut cases = Vec::new();
        
        while self.peek_kind() != Some(&Tok::RightBrace) {
            let case_start = self.current_pos();
            
            let variant_token = self.advance().ok_or_else(|| ParseError::UnexpectedEof {
                expected: vec!["identifier".to_string()],
            })?;
            
            let variant = match variant_token.value() {
                Ok(Tok::Identifier(id)) => variant_token.map(|_| *id),
                _ => return Err(ParseError::UnexpectedToken {
                    found: format!("{variant_token:?}"),
                    expected: vec!["identifier".to_string()],
                }),
            };
            
            self.expect(&Tok::Colon)?;
            
            let var_token = self.advance().ok_or_else(|| ParseError::UnexpectedEof {
                expected: vec!["identifier".to_string()],
            })?;
            
            let var = match var_token.value() {
                Ok(Tok::Identifier(id)) => var_token.map(|_| *id),
                _ => return Err(ParseError::UnexpectedToken {
                    found: format!("{var_token:?}"),
                    expected: vec!["identifier".to_string()],
                }),
            };
            
            self.expect(&Tok::FatArrow)?;
            let body = self.parse_stmt()?;
            
            cases.push(MatchCase { variant, var, body }.in_span(self.span_from(case_start)));
        }
        
        self.expect(&Tok::RightBrace)?;
        
        Ok(StmtKind::Match { scrutinee, cases })
    }

    /// Parse an expression
    pub fn parse_expr(&mut self) -> Result<Expr<'input>, ParseError<'input>> {
        self.parse_comma_expr()
    }

    /// Parse comma expression (lowest precedence)
    fn parse_comma_expr(&mut self) -> Result<Expr<'input>, ParseError<'input>> {
        let start = self.current_pos();
        let mut left = self.parse_assignment_expr()?;
        
        while self.try_consume(&Tok::Comma) {
            let right = self.parse_assignment_expr()?;
            left = Expr(ExprKind::Comma(Box::new(left), Box::new(right)).in_span(self.span_from(start)));
        }
        
        Ok(left)
    }

    /// Parse assignment expression
    fn parse_assignment_expr(&mut self) -> Result<Expr<'input>, ParseError<'input>> {
        let start = self.current_pos();
        let left = self.parse_ternary_expr()?;
        
        // Check for assignment operators
        let op = match self.peek_kind() {
            Some(Tok::Assign) => {
                self.advance();
                Some(Assignment::Standard)
            }
            Some(Tok::PlusAssign) => {
                self.advance();
                Some(Assignment::Arithmetic(Arithmetic::Addition))
            }
            Some(Tok::MinusAssign) => {
                self.advance();
                Some(Assignment::Arithmetic(Arithmetic::Subtraction))
            }
            Some(Tok::StarAssign) => {
                self.advance();
                Some(Assignment::Arithmetic(Arithmetic::Multiplication))
            }
            Some(Tok::SlashAssign) => {
                self.advance();
                Some(Assignment::Arithmetic(Arithmetic::Division))
            }
            Some(Tok::PercentAssign) => {
                self.advance();
                Some(Assignment::Arithmetic(Arithmetic::Modulo))
            }
            Some(Tok::BitwiseAndAssign) => {
                self.advance();
                Some(Assignment::BinaryBitwise(BinaryBitwise::And))
            }
            Some(Tok::BitwiseOrAssign) => {
                self.advance();
                Some(Assignment::BinaryBitwise(BinaryBitwise::Or))
            }
            Some(Tok::BitwiseXorAssign) => {
                self.advance();
                Some(Assignment::BinaryBitwise(BinaryBitwise::Xor))
            }
            Some(Tok::BitwiseLeftShiftAssign) => {
                self.advance();
                Some(Assignment::BinaryBitwise(BinaryBitwise::Shl))
            }
            Some(Tok::BitwiseRightShiftAssign) => {
                self.advance();
                Some(Assignment::BinaryBitwise(BinaryBitwise::Shr))
            }
            _ => None,
        };
        
        if let Some(op) = op {
            let right = self.parse_assignment_expr()?;
            Ok(Expr(ExprKind::Assignment(op, Box::new(left), Box::new(right)).in_span(self.span_from(start))))
        } else {
            Ok(left)
        }
    }

    /// Parse ternary expression
    fn parse_ternary_expr(&mut self) -> Result<Expr<'input>, ParseError<'input>> {
        let start = self.current_pos();
        let condition = self.parse_logical_or_expr()?;
        
        if self.try_consume(&Tok::QuestionMark) {
            let then_branch = self.parse_expr()?;
            self.expect(&Tok::Colon)?;
            let else_branch = self.parse_ternary_expr()?;
            Ok(Expr(ExprKind::Ternary(Box::new(condition), Box::new(then_branch), Box::new(else_branch)).in_span(self.span_from(start))))
        } else {
            Ok(condition)
        }
    }

    /// Parse logical OR expression
    fn parse_logical_or_expr(&mut self) -> Result<Expr<'input>, ParseError<'input>> {
        let start = self.current_pos();
        let mut left = self.parse_logical_and_expr()?;
        
        while self.try_consume(&Tok::LogicalOr) {
            let right = self.parse_logical_and_expr()?;
            left = Expr(ExprKind::Logical(Logical::Or, Box::new(left), Box::new(right)).in_span(self.span_from(start)));
        }
        
        Ok(left)
    }

    /// Parse logical AND expression
    fn parse_logical_and_expr(&mut self) -> Result<Expr<'input>, ParseError<'input>> {
        let start = self.current_pos();
        let mut left = self.parse_bitwise_or_expr()?;
        
        while self.try_consume(&Tok::LogicalAnd) {
            let right = self.parse_bitwise_or_expr()?;
            left = Expr(ExprKind::Logical(Logical::And, Box::new(left), Box::new(right)).in_span(self.span_from(start)));
        }
        
        Ok(left)
    }

    /// Parse bitwise OR expression
    fn parse_bitwise_or_expr(&mut self) -> Result<Expr<'input>, ParseError<'input>> {
        let start = self.current_pos();
        let mut left = self.parse_bitwise_xor_expr()?;
        
        while self.try_consume(&Tok::BitwiseOr) {
            let right = self.parse_bitwise_xor_expr()?;
            left = Expr(ExprKind::BinaryBitwise(BinaryBitwise::Or, Box::new(left), Box::new(right)).in_span(self.span_from(start)));
        }
        
        Ok(left)
    }

    /// Parse bitwise XOR expression
    fn parse_bitwise_xor_expr(&mut self) -> Result<Expr<'input>, ParseError<'input>> {
        let start = self.current_pos();
        let mut left = self.parse_bitwise_and_expr()?;
        
        while self.try_consume(&Tok::BitwiseXor) {
            let right = self.parse_bitwise_and_expr()?;
            left = Expr(ExprKind::BinaryBitwise(BinaryBitwise::Xor, Box::new(left), Box::new(right)).in_span(self.span_from(start)));
        }
        
        Ok(left)
    }

    /// Parse bitwise AND expression
    fn parse_bitwise_and_expr(&mut self) -> Result<Expr<'input>, ParseError<'input>> {
        let start = self.current_pos();
        let mut left = self.parse_equality_expr()?;
        
        while self.try_consume(&Tok::BitwiseAnd) {
            let right = self.parse_equality_expr()?;
            left = Expr(ExprKind::BinaryBitwise(BinaryBitwise::And, Box::new(left), Box::new(right)).in_span(self.span_from(start)));
        }
        
        Ok(left)
    }

    /// Parse equality expression
    fn parse_equality_expr(&mut self) -> Result<Expr<'input>, ParseError<'input>> {
        let start = self.current_pos();
        let mut left = self.parse_comparison_expr()?;
        
        loop {
            let op = match self.peek_kind() {
                Some(Tok::EqEq) => {
                    self.advance();
                    Equality::Eq
                }
                Some(Tok::NotEq) => {
                    self.advance();
                    Equality::Neq
                }
                _ => break,
            };
            
            let right = self.parse_comparison_expr()?;
            left = Expr(ExprKind::Equality(op, Box::new(left), Box::new(right)).in_span(self.span_from(start)));
        }
        
        Ok(left)
    }

    /// Parse comparison expression
    fn parse_comparison_expr(&mut self) -> Result<Expr<'input>, ParseError<'input>> {
        let start = self.current_pos();
        let mut left = self.parse_bitshift_expr()?;
        
        loop {
            let op = match self.peek_kind() {
                Some(Tok::Greater) => {
                    self.advance();
                    Comparison::Gt
                }
                Some(Tok::GreaterEq) => {
                    self.advance();
                    Comparison::Gte
                }
                Some(Tok::Less) => {
                    self.advance();
                    Comparison::Lt
                }
                Some(Tok::LessEq) => {
                    self.advance();
                    Comparison::Lte
                }
                _ => break,
            };
            
            let right = self.parse_bitshift_expr()?;
            left = Expr(ExprKind::Comparison(op, Box::new(left), Box::new(right)).in_span(self.span_from(start)));
        }
        
        Ok(left)
    }

    /// Parse bitshift expression
    fn parse_bitshift_expr(&mut self) -> Result<Expr<'input>, ParseError<'input>> {
        let start = self.current_pos();
        let mut left = self.parse_term_expr()?;
        
        loop {
            let op = match self.peek_kind() {
                Some(Tok::BitwiseLeftShift) => {
                    self.advance();
                    BinaryBitwise::Shl
                }
                Some(Tok::BitwiseRightShift) => {
                    self.advance();
                    BinaryBitwise::Shr
                }
                _ => break,
            };
            
            let right = self.parse_term_expr()?;
            left = Expr(ExprKind::BinaryBitwise(op, Box::new(left), Box::new(right)).in_span(self.span_from(start)));
        }
        
        Ok(left)
    }

    /// Parse term expression (addition/subtraction)
    fn parse_term_expr(&mut self) -> Result<Expr<'input>, ParseError<'input>> {
        let start = self.current_pos();
        let mut left = self.parse_factor_expr()?;
        
        loop {
            let op = match self.peek_kind() {
                Some(Tok::Plus) => {
                    self.advance();
                    Arithmetic::Addition
                }
                Some(Tok::Minus) => {
                    self.advance();
                    Arithmetic::Subtraction
                }
                _ => break,
            };
            
            let right = self.parse_factor_expr()?;
            left = Expr(ExprKind::Arithmetic(op, Box::new(left), Box::new(right)).in_span(self.span_from(start)));
        }
        
        Ok(left)
    }

    /// Parse factor expression (multiplication/division/modulo)
    fn parse_factor_expr(&mut self) -> Result<Expr<'input>, ParseError<'input>> {
        let start = self.current_pos();
        let mut left = self.parse_cast_expr()?;
        
        loop {
            let op = match self.peek_kind() {
                Some(Tok::Star) => {
                    self.advance();
                    Arithmetic::Multiplication
                }
                Some(Tok::Slash) => {
                    self.advance();
                    Arithmetic::Division
                }
                Some(Tok::Percent) => {
                    self.advance();
                    Arithmetic::Modulo
                }
                _ => break,
            };
            
            let right = self.parse_cast_expr()?;
            left = Expr(ExprKind::Arithmetic(op, Box::new(left), Box::new(right)).in_span(self.span_from(start)));
        }
        
        Ok(left)
    }

    /// Parse cast expression
    fn parse_cast_expr(&mut self) -> Result<Expr<'input>, ParseError<'input>> {
        let start = self.current_pos();
        let left = self.parse_unary_expr()?;
        
        if self.try_consume(&Tok::As) {
            let ty = self.parse_type()?;
            Ok(Expr(ExprKind::Cast(Box::new(left), ty).in_span(self.span_from(start))))
        } else {
            Ok(left)
        }
    }

    /// Parse unary expression
    fn parse_unary_expr(&mut self) -> Result<Expr<'input>, ParseError<'input>> {
        let start = self.current_pos();
        
        match self.peek_kind() {
            Some(Tok::LogicalNot) => {
                self.advance();
                let expr = self.parse_unary_expr()?;
                Ok(Expr(ExprKind::UnaryNot(Box::new(expr)).in_span(self.span_from(start))))
            }
            Some(Tok::Minus) => {
                self.advance();
                let expr = self.parse_unary_expr()?;
                Ok(Expr(ExprKind::UnaryMinus(Box::new(expr)).in_span(self.span_from(start))))
            }
            Some(Tok::BitwiseNot) => {
                self.advance();
                let expr = self.parse_unary_expr()?;
                Ok(Expr(ExprKind::UnaryBitwiseNot(Box::new(expr)).in_span(self.span_from(start))))
            }
            Some(Tok::BitwiseAnd) => {
                self.advance();
                let expr = self.parse_unary_expr()?;
                Ok(Expr(ExprKind::UnaryAddressOf(Box::new(expr)).in_span(self.span_from(start))))
            }
            Some(Tok::Star) => {
                self.advance();
                let expr = self.parse_unary_expr()?;
                Ok(Expr(ExprKind::UnaryDereference(Box::new(expr)).in_span(self.span_from(start))))
            }
            Some(Tok::PlusPlus) => {
                self.advance();
                let expr = self.parse_unary_expr()?;
                Ok(Expr(ExprKind::PrefixIncrement(Box::new(expr)).in_span(self.span_from(start))))
            }
            Some(Tok::MinusMinus) => {
                self.advance();
                let expr = self.parse_unary_expr()?;
                Ok(Expr(ExprKind::PrefixDecrement(Box::new(expr)).in_span(self.span_from(start))))
            }
            _ => self.parse_postfix_expr(),
        }
    }

    /// Parse postfix expression
    fn parse_postfix_expr(&mut self) -> Result<Expr<'input>, ParseError<'input>> {
        let mut expr = self.parse_primary_expr()?;
        
        loop {
            let start = expr.0.span().start();
            
            match self.peek_kind() {
                Some(Tok::LeftBracket) => {
                    self.advance();
                    let index = self.parse_expr()?;
                    self.expect(&Tok::RightBracket)?;
                    expr = Expr(ExprKind::Index(Box::new(expr), Box::new(index)).in_span(self.span_from(start)));
                }
                Some(Tok::Dot) => {
                    self.advance();
                    let field_token = self.advance().ok_or_else(|| ParseError::UnexpectedEof {
                        expected: vec!["identifier".to_string()],
                    })?;
                    let field = match field_token.value() {
                        Ok(Tok::Identifier(id)) => field_token.map(|_| *id),
                        _ => return Err(ParseError::UnexpectedToken {
                            found: format!("{field_token:?}"),
                            expected: vec!["identifier".to_string()],
                        }),
                    };
                    expr = Expr(ExprKind::Dot(Box::new(expr), field).in_span(self.span_from(start)));
                }
                Some(Tok::SmallArrow) => {
                    self.advance();
                    let field_token = self.advance().ok_or_else(|| ParseError::UnexpectedEof {
                        expected: vec!["identifier".to_string()],
                    })?;
                    let field = match field_token.value() {
                        Ok(Tok::Identifier(id)) => field_token.map(|_| *id),
                        _ => return Err(ParseError::UnexpectedToken {
                            found: format!("{field_token:?}"),
                            expected: vec!["identifier".to_string()],
                        }),
                    };
                    expr = Expr(ExprKind::Arrow(Box::new(expr), field).in_span(self.span_from(start)));
                }
                Some(Tok::LeftParen) => {
                    self.advance();
                    let args_start = self.current_pos();
                    let args = if self.peek_kind() == Some(&Tok::RightParen) {
                        Vec::new()
                    } else {
                        self.parse_argument_list()?
                    };
                    self.expect(&Tok::RightParen)?;
                    let args_spanned = args.in_span(self.span_from(args_start));
                    expr = Expr(ExprKind::Call(Box::new(expr), args_spanned).in_span(self.span_from(start)));
                }
                Some(Tok::PlusPlus) => {
                    self.advance();
                    expr = Expr(ExprKind::PostfixIncrement(Box::new(expr)).in_span(self.span_from(start)));
                }
                Some(Tok::MinusMinus) => {
                    self.advance();
                    expr = Expr(ExprKind::PostfixDecrement(Box::new(expr)).in_span(self.span_from(start)));
                }
                _ => break,
            }
        }
        
        Ok(expr)
    }

    /// Parse primary expression
    fn parse_primary_expr(&mut self) -> Result<Expr<'input>, ParseError<'input>> {
        let start = self.current_pos();
        
        match self.peek_kind() {
            Some(Tok::NumberLiteral(_)) => {
                let num_token = self.advance().expect("just peeked");
                let num = match num_token.value() {
                    Ok(Tok::NumberLiteral(n)) => num_token.map(|_| n.clone()),
                    _ => unreachable!(),
                };
                
                // Check for optional type suffix
                let ty = if let Some(Tok::Identifier(_)) = self.peek_kind() {
                    let ty_token = self.advance().expect("just peeked");
                    let ty_name = match ty_token.value() {
                        Ok(Tok::Identifier(id)) => ty_token.map(|_| *id),
                        _ => unreachable!(),
                    };
                    Some(Type(TypeKind::Identifier(ty_name).in_span(self.span_from(start))))
                } else {
                    None
                };
                
                Ok(Expr(ExprKind::NumberLiteral(num, ty).in_span(self.span_from(start))))
            }
            Some(Tok::StringLiteral(_)) => {
                let str_token = self.advance().expect("just peeked");
                let s = match str_token.value() {
                    Ok(Tok::StringLiteral(zs)) => str_token.map(|_| zs.clone()),
                    _ => unreachable!(),
                };
                Ok(Expr(ExprKind::StringLiteral(s).in_span(self.span_from(start))))
            }
            Some(Tok::CharLiteral(_)) => {
                let char_token = self.advance().expect("just peeked");
                let c = match char_token.value() {
                    Ok(Tok::CharLiteral(ch)) => char_token.map(|_| ch.clone()),
                    _ => unreachable!(),
                };
                Ok(Expr(ExprKind::CharLiteral(c).in_span(self.span_from(start))))
            }
            Some(Tok::Identifier(_)) => {
                let id_token = self.advance().expect("just peeked");
                let id = match id_token.value() {
                    Ok(Tok::Identifier(name)) => id_token.map(|_| *name),
                    _ => unreachable!(),
                };
                Ok(Expr(ExprKind::Identifier(id).in_span(self.span_from(start))))
            }
            Some(Tok::True) => {
                self.advance();
                Ok(Expr(ExprKind::BooleanLiteral(true).in_span(self.span_from(start))))
            }
            Some(Tok::False) => {
                self.advance();
                Ok(Expr(ExprKind::BooleanLiteral(false).in_span(self.span_from(start))))
            }
            Some(Tok::SizeOf) => {
                self.advance();
                
                // Check if followed by '(' for expression or directly type
                if self.peek_kind() == Some(&Tok::LeftParen) {
                    self.advance();
                    
                    // Try to parse as type first, fallback to expression
                    // This is simplified - in a real parser we'd need better lookahead
                    let saved_pos = self.pos;
                    
                    if let Ok(ty) = self.parse_type() {
                        if self.peek_kind() == Some(&Tok::RightParen) {
                            self.expect(&Tok::RightParen)?;
                            return Ok(Expr(ExprKind::SizeOfType(ty).in_span(self.span_from(start))));
                        }
                    }
                    
                    // Restore and parse as expression
                    self.pos = saved_pos;
                    let expr = self.parse_expr()?;
                    self.expect(&Tok::RightParen)?;
                    Ok(Expr(ExprKind::SizeOfExpr(Box::new(expr)).in_span(self.span_from(start))))
                } else {
                    let ty = self.parse_type()?;
                    Ok(Expr(ExprKind::SizeOfType(ty).in_span(self.span_from(start))))
                }
            }
            Some(Tok::New) => self.parse_struct_construction(),
            Some(Tok::LeftParen) => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(&Tok::RightParen)?;
                Ok(expr)
            }
            _ => Err(ParseError::UnexpectedToken {
                found: self.peek_kind().map_or("EOF".to_string(), |t| t.to_string()),
                expected: vec!["expression".to_string()],
            }),
        }
    }

    /// Parse struct construction
    fn parse_struct_construction(&mut self) -> Result<Expr<'input>, ParseError<'input>> {
        let start = self.current_pos();
        self.expect(&Tok::New)?;
        
        let ty = self.parse_type()?;
        
        self.expect(&Tok::LeftBrace)?;
        let fields_start = self.current_pos();
        
        let mut fields = Vec::new();
        
        while self.peek_kind() != Some(&Tok::RightBrace) {
            let field_start = self.current_pos();
            
            let field_name_token = self.advance().ok_or_else(|| ParseError::UnexpectedEof {
                expected: vec!["identifier".to_string()],
            })?;
            
            let field_name = match field_name_token.value() {
                Ok(Tok::Identifier(id)) => field_name_token.map(|_| *id),
                _ => return Err(ParseError::UnexpectedToken {
                    found: format!("{field_name_token:?}"),
                    expected: vec!["identifier".to_string()],
                }),
            };
            
            self.expect(&Tok::Colon)?;
            let field_value = self.parse_assignment_expr()?;
            
            fields.push((field_name, field_value).in_span(self.span_from(field_start)));
            
            if !self.try_consume(&Tok::Comma) {
                break;
            }
        }
        
        self.expect(&Tok::RightBrace)?;
        
        let fields_spanned = fields.in_span(self.span_from(fields_start));
        Ok(Expr(ExprKind::StructConstruction(ty, fields_spanned).in_span(self.span_from(start))))
    }

    /// Parse argument list for function calls
    fn parse_argument_list(&mut self) -> Result<Vec<Expr<'input>>, ParseError<'input>> {
        let mut args = Vec::new();
        
        loop {
            args.push(self.parse_assignment_expr()?);
            
            if !self.try_consume(&Tok::Comma) {
                break;
            }
        }
        
        Ok(args)
    }
}

/// Convert parse error to diagnostic
fn parse_error_to_diagnostic<'input>(error: ParseError<'input>, file_name: &'static str) -> Diagnostic {
    match error {
        ParseError::UnexpectedEof { expected } => {
            DiagnosticKind::UnexpectedEof(expected).error_in(Span::from_positions_and_file(0, 0, file_name))
        }
        ParseError::UnexpectedToken { found, expected } => {
            DiagnosticKind::UnrecognizedToken(found, expected).error_in(Span::from_positions_and_file(0, 0, file_name))
        }
        ParseError::LexicalError(lex_err) => {
            match lex_err {
                LexicalError::UnknownToken(token) => DiagnosticKind::UnknownToken(token.to_string()),
                LexicalError::UnterminatedBlockComment => DiagnosticKind::UnterminatedBlockComment,
                LexicalError::UnterminatedStringLiteral => DiagnosticKind::UnterminatedStringLiteral,
                LexicalError::UnknownEscapeSequence => DiagnosticKind::UnknownEscapeSequence,
                LexicalError::JavascriptUserDetected(expected) => DiagnosticKind::JavascriptUserDetected(expected),
            }.error_in(Span::from_positions_and_file(0, 0, file_name))
        }
    }
}

/// Parse a Zirco program
pub fn parse_program<'input>(
    input: &'input str,
    file_name: &'static str,
) -> Result<Vec<Spanned<Declaration<'input>>>, Diagnostic> {
    let lexer = crate::custom_lexer::CustomZircoLexer::new(input, file_name);
    let mut parser = CustomParser::new(lexer, file_name);
    parser.parse_program().map_err(|e| parse_error_to_diagnostic(e, file_name))
}

/// Parse a statement list
pub fn parse_stmt_list<'input>(
    input: &'input str,
    file_name: &'static str,
) -> Result<Spanned<Vec<Stmt<'input>>>, Diagnostic> {
    let lexer = crate::custom_lexer::CustomZircoLexer::new(input, file_name);
    let mut parser = CustomParser::new(lexer, file_name);
    let stmts = parser.parse_stmt_list().map_err(|e| parse_error_to_diagnostic(e, file_name))?;
    Ok(stmts.in_span(Span::from_positions_and_file(0, input.len(), file_name)))
}

/// Parse an expression
pub fn parse_expr<'input>(
    input: &'input str,
    file_name: &'static str,
) -> Result<Expr<'input>, Diagnostic> {
    let lexer = crate::custom_lexer::CustomZircoLexer::new(input, file_name);
    let mut parser = CustomParser::new(lexer, file_name);
    parser.parse_expr().map_err(|e| parse_error_to_diagnostic(e, file_name))
}

/// Parse a type
pub fn parse_type<'input>(
    input: &'input str,
    file_name: &'static str,
) -> Result<Type<'input>, Diagnostic> {
    let lexer = crate::custom_lexer::CustomZircoLexer::new(input, file_name);
    let mut parser = CustomParser::new(lexer, file_name);
    parser.parse_type().map_err(|e| parse_error_to_diagnostic(e, file_name))
}
