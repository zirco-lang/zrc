//! Visitor tools used by Zircop lints.

use zrc_parser::ast::{
    expr::{Expr as AstExpr, ExprKind as AstExprKind},
    stmt::{
        ArgumentDeclarationList, Declaration as AstDecl, LetDeclaration as AstLetDecl,
        Stmt as AstStmt, StmtKind as AstStmtKind, SwitchTrigger,
    },
    ty::Type as AstType,
};
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

/// Syntactic visitors are called over the [`zrc_parser::ast`].
pub trait SyntacticVisit<'input> {
    /// Visit an expression. Implementors should override this to run custom
    /// logic before (or instead of) the default walk.
    fn visit_expr(&mut self, expr: &AstExpr<'input>) {
        self.walk_expr(expr);
    }

    /// Walk an expression's children. Default traversal implementation.
    fn walk_expr(&mut self, expr: &AstExpr<'input>) {
        match expr.0.value() {
            AstExprKind::Comma(lhs, rhs)
            | AstExprKind::Assignment(_, lhs, rhs)
            | AstExprKind::BinaryBitwise(_, lhs, rhs)
            | AstExprKind::Logical(_, lhs, rhs)
            | AstExprKind::Equality(_, lhs, rhs)
            | AstExprKind::Comparison(_, lhs, rhs)
            | AstExprKind::Arithmetic(_, lhs, rhs)
            | AstExprKind::Index(lhs, rhs) => {
                self.visit_expr(lhs.as_ref());
                self.visit_expr(rhs.as_ref());
            }
            AstExprKind::Call(func, args) => {
                self.visit_expr(func.as_ref());
                for arg in args.value() {
                    self.visit_expr(arg);
                }
            }
            AstExprKind::UnaryNot(ex)
            | AstExprKind::UnaryBitwiseNot(ex)
            | AstExprKind::UnaryMinus(ex)
            | AstExprKind::UnaryAddressOf(ex)
            | AstExprKind::UnaryDereference(ex)
            | AstExprKind::PrefixIncrement(ex)
            | AstExprKind::PrefixDecrement(ex)
            | AstExprKind::Dot(ex, _)
            | AstExprKind::Arrow(ex, _)
            | AstExprKind::PostfixIncrement(ex)
            | AstExprKind::PostfixDecrement(ex) => {
                self.visit_expr(ex.as_ref());
            }
            AstExprKind::Ternary(cond, if_true, if_false) => {
                self.visit_expr(cond.as_ref());
                self.visit_expr(if_true.as_ref());
                self.visit_expr(if_false.as_ref());
            }
            AstExprKind::Cast(ex, ty) => {
                self.visit_expr(ex.as_ref());
                self.visit_type(ty);
            }
            AstExprKind::SizeOfType(ty) => self.visit_type(ty),
            AstExprKind::SizeOfExpr(ex) => self.visit_expr(ex.as_ref()),
            AstExprKind::StructConstruction(ty, fields) => {
                self.visit_type(ty);
                for field in fields.value() {
                    let fv = field.value();
                    self.visit_expr(&fv.1);
                }
            }
            AstExprKind::ArrayLiteral(elements) => {
                for elem in elements.value() {
                    self.visit_expr(elem);
                }
            }
            AstExprKind::NumberLiteral(_, ty) => {
                if let Some(ty) = ty.as_ref() {
                    self.visit_type(ty);
                }
            }
            AstExprKind::StringLiteral(_)
            | AstExprKind::CharLiteral(_)
            | AstExprKind::Identifier(_)
            | AstExprKind::BooleanLiteral(_) => {}
        }
    }

    /// Visit a statement. Implementors should override this to run custom
    /// logic before (or instead of) the default walk.
    fn visit_stmt(&mut self, stmt: &AstStmt<'input>) {
        self.walk_stmt(stmt);
    }

    /// Walk a statement's children. Default traversal implementation.
    fn walk_stmt(&mut self, stmt: &AstStmt<'input>) {
        match stmt.0.value() {
            AstStmtKind::IfStmt(cond, if_true, maybe_false) => {
                self.visit_expr(cond);
                self.visit_stmt(if_true.as_ref());
                if let Some(if_false) = maybe_false {
                    self.visit_stmt(if_false.as_ref());
                }
            }
            AstStmtKind::WhileStmt(cond, body) => {
                self.visit_expr(cond);
                self.visit_stmt(body.as_ref());
            }
            AstStmtKind::DoWhileStmt(body, cond) => {
                self.visit_stmt(body.as_ref());
                self.visit_expr(cond);
            }
            AstStmtKind::ForStmt {
                init,
                cond,
                post,
                body,
            } => {
                if let Some(init_box) = init {
                    let init_spanned = init_box.as_ref();
                    for let_decl in init_spanned.value() {
                        self.visit_let_decl(let_decl);
                    }
                }
                if let Some(cond_expr) = cond {
                    self.visit_expr(cond_expr);
                }
                if let Some(post_expr) = post {
                    self.visit_expr(post_expr);
                }
                self.visit_stmt(body.as_ref());
            }
            AstStmtKind::FourStmt(body) => {
                self.visit_stmt(body.as_ref());
            }
            AstStmtKind::BlockStmt(stmts) => {
                self.visit_block(stmts.as_slice());
            }
            AstStmtKind::ExprStmt(expr) => {
                self.visit_expr(expr);
            }
            AstStmtKind::EmptyStmt
            | AstStmtKind::ContinueStmt
            | AstStmtKind::BreakStmt
            | AstStmtKind::UnreachableStmt => {}
            AstStmtKind::ReturnStmt(opt) => {
                if let Some(ex) = opt {
                    self.visit_expr(ex);
                }
            }
            AstStmtKind::DeclarationList(list) => {
                for let_decl in list.value() {
                    self.visit_let_decl(let_decl);
                }
            }
            AstStmtKind::SwitchCase { scrutinee, cases } => {
                self.visit_expr(scrutinee);
                for case in cases {
                    match &case.value().0 {
                        SwitchTrigger::Expr(ex) => self.visit_expr(ex),
                        SwitchTrigger::Default => {}
                    }
                    self.visit_stmt(&case.value().1);
                }
            }
            AstStmtKind::Match { scrutinee, cases } => {
                self.visit_expr(scrutinee);
                for case in cases {
                    let mc = case.value();
                    self.visit_stmt(&mc.body);
                }
            }
        }
    }

    /// Visit a type. Override to run logic before walking into the type.
    fn visit_type(&mut self, ty: &AstType<'input>) {
        self.walk_type(ty);
    }

    /// Walk a type's children (default: do nothing).
    fn walk_type(&mut self, _ty: &AstType<'input>) {}

    /// Visit a function declaration. Override to run logic before walking.
    fn visit_fn_decl(
        &mut self,
        name: &Spanned<&'input str>,
        parameters: &Spanned<ArgumentDeclarationList<'input>>,
        return_type: &Option<AstType<'input>>,
        body: &Option<Spanned<Vec<AstStmt<'input>>>>,
    ) {
        self.walk_fn_decl(name, parameters, return_type, body);
    }

    /// Walk a function declaration's body (default traversal).
    fn walk_fn_decl(
        &mut self,
        _name: &Spanned<&'input str>,
        _parameters: &Spanned<ArgumentDeclarationList<'input>>,
        _return_type: &Option<AstType<'input>>,
        body: &Option<Spanned<Vec<AstStmt<'input>>>>,
    ) {
        if let Some(block) = body {
            self.visit_block(block.value().as_slice());
        }
    }

    /// Visit a type alias declaration.
    fn visit_type_alias_decl(&mut self, name: &Spanned<&'input str>, ty: &AstType<'input>) {
        self.walk_type_alias_decl(name, ty);
    }

    /// Walk a type alias declaration (default behavior: visit the contained
    /// type).
    fn walk_type_alias_decl(&mut self, _name: &Spanned<&'input str>, ty: &AstType<'input>) {
        self.visit_type(ty);
    }

    /// Visit a let declaration.
    fn visit_let_decl(&mut self, let_decl: &Spanned<AstLetDecl<'input>>) {
        self.walk_let_decl(let_decl);
    }

    /// Walk a let declaration (default traversal).
    fn walk_let_decl(&mut self, let_decl: &Spanned<AstLetDecl<'input>>) {
        if let Some(ty) = &let_decl.value().ty {
            self.visit_type(ty);
        }
        if let Some(initializer) = &let_decl.value().value {
            self.visit_expr(initializer);
        }
    }

    /// Visit a global let declaration.
    fn visit_global_let_decl(&mut self, decls: &Spanned<Vec<Spanned<AstLetDecl<'input>>>>) {
        self.walk_global_let_decl(decls);
    }

    /// Walk a global let declaration (default traversal).
    fn walk_global_let_decl(&mut self, decls: &Spanned<Vec<Spanned<AstLetDecl<'input>>>>) {
        for decl in decls.value() {
            self.visit_let_decl(decl);
        }
    }

    /// Visit a declaration.
    fn visit_decl(&mut self, decl: &Spanned<AstDecl<'input>>) {
        self.walk_decl(decl);
    }

    /// Walk a declaration (default traversal).
    fn walk_decl(&mut self, decl: &Spanned<AstDecl<'input>>) {
        match decl.value() {
            AstDecl::FunctionDeclaration {
                name,
                parameters,
                return_type,
                body,
            } => {
                self.visit_fn_decl(name, parameters, return_type, body);
            }
            AstDecl::TypeAliasDeclaration { name, ty } => {
                self.visit_type_alias_decl(name, ty);
            }
            AstDecl::GlobalLetDeclaration(decls) => self.visit_global_let_decl(decls),
        }
    }

    /// Visit a block of statements.
    fn visit_block(&mut self, block: &[AstStmt<'input>]) {
        self.walk_block(block);
    }

    /// Walk a block (default traversal).
    fn walk_block(&mut self, block: &[AstStmt<'input>]) {
        for stmt in block {
            self.visit_stmt(stmt);
        }
    }

    /// Visit a program (block of declarations).
    fn visit_program(&mut self, program: &[Spanned<AstDecl<'input>>]) {
        self.walk_program(program);
    }

    /// Walk a program (default traversal).
    fn walk_program(&mut self, program: &[Spanned<AstDecl<'input>>]) {
        for decl in program {
            self.visit_decl(decl);
        }
    }
}

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
    fn visit_tc_stmt(&mut self, stmt: &TcStmt<'input, 'gs>) {
        self.walk_tc_stmt(stmt);
    }

    /// Walk a typed statement's children (default traversal).
    fn walk_tc_stmt(&mut self, stmt: &TcStmt<'input, 'gs>) {
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
            TcStmtKind::FourStmt(body) => {
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
        body: &Option<Spanned<BlockMetadata<'input, 'gs>>>,
    ) {
        self.walk_tc_fn_decl(name, parameters, return_type, body);
    }

    /// Walk a typed function declaration (default traversal).
    fn walk_tc_fn_decl(
        &mut self,
        _name: &Spanned<&'input str>,
        _parameters: &Spanned<TcArgumentDeclarationList<'input>>,
        _return_type: &Spanned<TcType<'input>>,
        body: &Option<Spanned<BlockMetadata<'input, 'gs>>>,
    ) {
        if let Some(block) = body {
            self.visit_tc_block(block.value());
        }
    }

    /// Visit a typed declaration.
    fn visit_tc_decl(&mut self, decl: &Spanned<TcDecl<'input, 'gs>>) {
        self.walk_tc_decl(decl);
    }

    /// Walk a typed declaration (default traversal).
    fn walk_tc_decl(&mut self, decl: &Spanned<TcDecl<'input, 'gs>>) {
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
    fn visit_tc_block(&mut self, block: &BlockMetadata<'input, 'gs>) {
        self.walk_tc_block(block);
    }

    /// Walk a typed block (default traversal).
    fn walk_tc_block(&mut self, block: &BlockMetadata<'input, 'gs>) {
        for stmt in &block.stmts {
            self.visit_tc_stmt(stmt);
        }
    }

    /// Visit a typed program (block of declarations).
    fn visit_tc_program(&mut self, program: &[Spanned<TcDecl<'input, 'gs>>]) {
        self.walk_tc_program(program);
    }

    /// Walk a typed program (default traversal).
    fn walk_tc_program(&mut self, program: &[Spanned<TcDecl<'input, 'gs>>]) {
        for decl in program {
            self.visit_tc_decl(decl);
        }
    }
}
