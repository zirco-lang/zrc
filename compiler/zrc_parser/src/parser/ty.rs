//! Type parsing for [`ZircoParser`].

use zrc_diagnostics::{Diagnostic, DiagnosticKind};
use zrc_utils::span::{Span, Spannable, Spanned};

use super::ZircoParser;
use crate::{ast::ty::Type, lexer::token::TokenKind::*};

impl<'input> ZircoParser<'input> {
	/// type ::= identifier
	///        | "*" type
	///        | "[" NUMBER_LITERAL "]" type
	///        | "struct" "{" field_list? "}"
	///        | "packed" "struct" "{" field_list? "}"
	///        | "union" "{" field_list? "}"
	///        | "enum" "{" field_list? "}"
	///        | "fn" "(" field_list? ")" "->" type
	///        | "(" type ")"
	pub fn parse_type(&mut self) -> Result<Type<'input>, Diagnostic> {
		match self.lexer.peek_token_or_eof()?.kind {
			Identifier => {
				let ident_token = self.lexer.next_token_or_eof()?;
				Ok(Type::build_ident(ident_token.spanned_literal()))
			}

			Star => {
				// consume the `*` token
				let star_span = self.lexer.next_token_or_eof()?.span;
				let inner_type = self.parse_type()?;

				Ok(Type::build_ptr(
					Span::merge(star_span, inner_type.0.span()).expect("spans should merge"),
					inner_type,
				))
			}

			LeftParen => {
				// consume the `(` token
				let left_paren_span = self.lexer.next_token_or_eof()?.span;
				let inner_type = self.parse_type()?;
				let right_paren_token = self.lexer.next_token_or_eof()?;
				if right_paren_token.kind != RightParen {}
			}
		}
	}
}
