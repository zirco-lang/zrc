//! Semantic, or TAST-level, visitors for Zirco.

//! Visitor tools used by Zircop lints.

use zrc_typeck::{
	tast::{
		expr::{Place as TcPlace, TypedExpr as TcExpr, TypedExprKind as TcExprKind},
		stmt::{
			ArgumentDeclarationList as TcArgumentDeclarationList, LetDeclaration as TcLetDecl,
			TypedDeclaration as TcDecl, TypedStmt as TcStmt, TypedStmtKind as TcStmtKind,
		},
		ty::Type as TcType,
	},
	typeck::BlockMetadata,
};
use zrc_utils::span::Spanned;

/// Semantic visitors are called over the typed AST ([`zrc_typeck::tast`]).
pub trait SemanticVisit<'input, 'gs> {
	/// Visit a typed place (lvalue). Override to run logic before walking.
	fn visit_place(&mut self, place: &TcPlace<'input>) {
		self.walk_place(place);
	}

	/// Walk a typed place (default: no-op).
	fn walk_place(&mut self, _place: &TcPlace<'input>) {}

	/// Visit a typed expression. Override to run logic before walking.
	fn visit_tc_expr(&mut self, expr: &TcExpr<'input>) {
		self.walk_tc_expr(expr);
	}

	/// Walk a typed expression's children (default traversal).
	fn walk_tc_expr(&mut self, expr: &TcExpr<'input>) {
		match expr.kind.value() {
			TcExprKind::Assignment(place, rhs) => {
				self.visit_place(place.as_ref());
				self.visit_tc_expr(rhs.as_ref());
			}
			TcExprKind::UnaryNot(ex)
			| TcExprKind::UnaryBitwiseNot(ex)
			| TcExprKind::UnaryMinus(ex)
			| TcExprKind::UnaryDereference(ex) => {
				self.visit_tc_expr(ex.as_ref());
			}
			TcExprKind::UnaryAddressOf(place)
			| TcExprKind::PrefixIncrement(place)
			| TcExprKind::PrefixDecrement(place)
			| TcExprKind::PostfixIncrement(place)
			| TcExprKind::PostfixDecrement(place)
			| TcExprKind::Dot(place, _) => {
				self.visit_place(place.as_ref());
			}
			TcExprKind::Comma(lhs, rhs)
			| TcExprKind::BinaryBitwise(_, lhs, rhs)
			| TcExprKind::Logical(_, lhs, rhs)
			| TcExprKind::Equality(_, lhs, rhs)
			| TcExprKind::Comparison(_, lhs, rhs)
			| TcExprKind::Arithmetic(_, lhs, rhs)
			| TcExprKind::Index(lhs, rhs) => {
				self.visit_tc_expr(lhs.as_ref());
				self.visit_tc_expr(rhs.as_ref());
			}
			TcExprKind::Call(place, args) => {
				self.visit_place(place.as_ref());
				for arg in args {
					self.visit_tc_expr(arg);
				}
			}
			TcExprKind::Ternary(cond, if_true, if_false) => {
				self.visit_tc_expr(cond.as_ref());
				self.visit_tc_expr(if_true.as_ref());
				self.visit_tc_expr(if_false.as_ref());
			}
			TcExprKind::Cast(ex, ty) => {
				self.visit_tc_expr(ex.as_ref());
				self.visit_tc_type(ty.value());
			}
			TcExprKind::SizeOf(ty) => self.visit_tc_type(ty),
			TcExprKind::StructConstruction(fields) => {
				for (_name, expr) in fields.iter() {
					self.visit_tc_expr(expr);
				}
			}
			TcExprKind::ArrayLiteral(elements) => {
				for elem in elements {
					self.visit_tc_expr(elem);
				}
			}
			TcExprKind::NumberLiteral(_, _)
			| TcExprKind::StringLiteral(_)
			| TcExprKind::CharLiteral(_)
			| TcExprKind::Identifier(_)
			| TcExprKind::BooleanLiteral(_) => {}
		}
	}

	/// Visit a typed statement. Override to run logic before walking.
	fn visit_tc_stmt(&mut self, stmt: &TcStmt<'input>) {
		self.walk_tc_stmt(stmt);
	}

	/// Walk a typed statement's children (default traversal).
	fn walk_tc_stmt(&mut self, stmt: &TcStmt<'input>) {
		match stmt.kind.value() {
			TcStmtKind::IfStmt(cond, if_true, maybe_false) => {
				self.visit_tc_expr(cond);
				self.visit_tc_block(if_true.value());
				if let Some(if_false) = maybe_false {
					self.visit_tc_block(if_false.value());
				}
			}
			TcStmtKind::WhileStmt(cond, body) => {
				self.visit_tc_expr(cond);
				self.visit_tc_block(body.value());
			}
			TcStmtKind::DoWhileStmt(body, cond) => {
				self.visit_tc_block(body.value());
				self.visit_tc_expr(cond);
			}
			TcStmtKind::ForStmt {
				init,
				cond,
				post,
				body,
			} => {
				if let Some(init_box) = init {
					for let_decl in init_box.as_ref() {
						self.visit_tc_let_decl(let_decl);
					}
				}
				if let Some(cond_expr) = cond {
					self.visit_tc_expr(cond_expr);
				}
				if let Some(post_expr) = post {
					self.visit_tc_expr(post_expr);
				}
				self.visit_tc_block(body.value());
			}
			TcStmtKind::SwitchCase {
				scrutinee,
				default,
				cases,
			} => {
				self.visit_tc_expr(scrutinee);
				for cs in &default.stmts {
					self.visit_tc_stmt(cs);
				}
				for (case_expr, case_stmts) in cases {
					self.visit_tc_expr(case_expr);
					for st in &case_stmts.stmts {
						self.visit_tc_stmt(st);
					}
				}
			}
			TcStmtKind::BlockStmt(stmts) => {
				self.visit_tc_block(stmts);
			}
			TcStmtKind::ExprStmt(expr) => self.visit_tc_expr(expr),
			TcStmtKind::ContinueStmt | TcStmtKind::BreakStmt | TcStmtKind::UnreachableStmt => {}
			TcStmtKind::ReturnStmt(opt) => {
				if let Some(ex) = opt {
					self.visit_tc_expr(ex);
				}
			}
			TcStmtKind::DeclarationList(list) => {
				for let_decl in list {
					self.visit_tc_let_decl(let_decl);
				}
			}
		}
	}

	/// Visit a typed type. Override to run logic before walking.
	fn visit_tc_type(&mut self, ty: &TcType<'input>) {
		self.walk_tc_type(ty);
	}

	/// Walk a typed type (default: no-op).
	fn walk_tc_type(&mut self, _ty: &TcType<'input>) {}

	/// Visit a typed let declaration.
	fn visit_tc_let_decl(&mut self, let_decl: &Spanned<TcLetDecl<'input>>) {
		self.walk_tc_let_decl(let_decl);
	}

	/// Walk a typed let declaration (default traversal).
	fn walk_tc_let_decl(&mut self, let_decl: &Spanned<TcLetDecl<'input>>) {
		if let Some(initializer) = &let_decl.value().value {
			self.visit_tc_expr(initializer);
		}
	}

	/// Visit a global typed let declaration.
	fn visit_tc_global_let_decl(&mut self, decls: &Vec<Spanned<TcLetDecl<'input>>>) {
		self.walk_tc_global_let_decl(decls);
	}

	/// Walk a global typed let declaration (default traversal).
	fn walk_tc_global_let_decl(&mut self, decls: &Vec<Spanned<TcLetDecl<'input>>>) {
		for decl in decls {
			self.visit_tc_let_decl(decl);
		}
	}

	/// Visit a typed function declaration. Override to run logic before
	/// walking.
	fn visit_tc_fn_decl(
		&mut self,
		name: &Spanned<&'input str>,
		parameters: &Spanned<TcArgumentDeclarationList<'input>>,
		return_type: &Spanned<TcType<'input>>,
		body: &Option<Spanned<BlockMetadata<'input>>>,
	) {
		self.walk_tc_fn_decl(name, parameters, return_type, body);
	}

	/// Walk a typed function declaration (default traversal).
	fn walk_tc_fn_decl(
		&mut self,
		_name: &Spanned<&'input str>,
		_parameters: &Spanned<TcArgumentDeclarationList<'input>>,
		_return_type: &Spanned<TcType<'input>>,
		body: &Option<Spanned<BlockMetadata<'input>>>,
	) {
		if let Some(block) = body {
			self.visit_tc_block(block.value());
		}
	}

	/// Visit a typed declaration.
	fn visit_tc_decl(&mut self, decl: &Spanned<TcDecl<'input>>) {
		self.walk_tc_decl(decl);
	}

	/// Walk a typed declaration (default traversal).
	fn walk_tc_decl(&mut self, decl: &Spanned<TcDecl<'input>>) {
		match decl.value() {
			TcDecl::FunctionDeclaration {
				name,
				parameters,
				return_type,
				body,
			} => {
				self.visit_tc_fn_decl(name, parameters, return_type, body);
			}
			TcDecl::GlobalLetDeclaration(decls) => self.visit_tc_global_let_decl(decls),
		}
	}

	/// Visit a typed block of statements.
	fn visit_tc_block(&mut self, block: &BlockMetadata<'input>) {
		self.walk_tc_block(block);
	}

	/// Walk a typed block (default traversal).
	fn walk_tc_block(&mut self, block: &BlockMetadata<'input>) {
		for stmt in &block.stmts {
			self.visit_tc_stmt(stmt);
		}
	}

	/// Visit a typed program (block of declarations).
	fn visit_tc_program(&mut self, program: &[Spanned<TcDecl<'input>>]) {
		self.walk_tc_program(program);
	}

	/// Walk a typed program (default traversal).
	fn walk_tc_program(&mut self, program: &[Spanned<TcDecl<'input>>]) {
		for decl in program {
			self.visit_tc_decl(decl);
		}
	}
}
