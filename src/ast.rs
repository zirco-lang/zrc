#![allow(ambiguous_associated_items)]
use subenum::subenum;

/// Translate into a different sub-enum of Expr
#[macro_export]
macro_rules! into_expr_type {
    ($to:tt,$val:expr) => {
        $crate::ast::$to::try_from($crate::ast::Expr::from($val)).unwrap()
    };
}

#[subenum(
    Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift,
    Term, Factor, Primary, IDENTIFIER
)]
#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    #[subenum(Comma)]
    Comma(Box<Comma>, Box<Assignment>),

    #[subenum(Comma, Assignment)]
    Assignment(Box<Unary>, Box<Assignment>),
    #[subenum(Comma, Assignment)]
    AdditionAssignment(Box<Unary>, Box<Assignment>),
    #[subenum(Comma, Assignment)]
    SubtractionAssignment(Box<Unary>, Box<Assignment>),
    #[subenum(Comma, Assignment)]
    MultiplicationAssignment(Box<Unary>, Box<Assignment>),
    #[subenum(Comma, Assignment)]
    DivisionAssignment(Box<Unary>, Box<Assignment>),
    #[subenum(Comma, Assignment)]
    ModuloAssignment(Box<Unary>, Box<Assignment>),
    #[subenum(Comma, Assignment)]
    BitwiseAndAssignment(Box<Unary>, Box<Assignment>),
    #[subenum(Comma, Assignment)]
    BitwiseOrAssignment(Box<Unary>, Box<Assignment>),
    #[subenum(Comma, Assignment)]
    BitwiseXorAssignment(Box<Unary>, Box<Assignment>),
    #[subenum(Comma, Assignment)]
    BitwiseLeftShiftAssignment(Box<Unary>, Box<Assignment>),
    #[subenum(Comma, Assignment)]
    BitwiseRightShiftAssignment(Box<Unary>, Box<Assignment>),

    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift,
        Term, Factor
    )]
    UnaryNot(Box<Unary>),
    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift,
        Term, Factor
    )]
    UnaryBitwiseNot(Box<Unary>),
    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift,
        Term, Factor
    )]
    UnaryMinus(Box<Unary>),
    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift,
        Term, Factor
    )]
    PreIncrement(Box<Unary>),
    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift,
        Term, Factor
    )]
    PreDecrement(Box<Unary>),

    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift,
        Term, Factor
    )]
    Index(Box<Postfix>, Box<Expr>),
    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift,
        Term, Factor
    )]
    Dot(Box<Postfix>, Box<IDENTIFIER>),
    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift,
        Term, Factor
    )]
    NamespaceAccess(Box<Postfix>, Box<IDENTIFIER>),
    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift,
        Term, Factor
    )]
    PostIncrement(Box<Postfix>),
    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift,
        Term, Factor
    )]
    PostDecrement(Box<Postfix>),
    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift,
        Term, Factor
    )]
    Call(Box<Postfix>, Vec<Assignment>),

    #[subenum(Comma, Assignment, Unary, Postfix, Ternary)]
    Ternary(Box<Logical>, Box<Expr>, Box<Ternary>),

    #[subenum(Comma, Assignment, Unary, Postfix, Ternary, Logical)]
    LogicalAnd(Box<Logical>, Box<Equality>),
    #[subenum(Comma, Assignment, Unary, Postfix, Ternary, Logical)]
    LogicalOr(Box<Logical>, Box<Equality>),

    #[subenum(Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality)]
    Equals(Box<Equality>, Box<Bitwise>),
    #[subenum(Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality)]
    NotEquals(Box<Equality>, Box<Bitwise>),

    #[subenum(Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise)]
    BitwiseAnd(Box<Bitwise>, Box<Comparison>),
    #[subenum(Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise)]
    BitwiseOr(Box<Bitwise>, Box<Comparison>),
    #[subenum(Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise)]
    BitwiseXor(Box<Bitwise>, Box<Comparison>),

    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison
    )]
    GreaterThan(Box<Comparison>, Box<Shift>),
    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison
    )]
    GreaterThanOrEqualTo(Box<Comparison>, Box<Shift>),
    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison
    )]
    LessThan(Box<Comparison>, Box<Shift>),
    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison
    )]
    LessThanOrEqualTo(Box<Comparison>, Box<Shift>),

    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift
    )]
    BitwiseRightShift(Box<Shift>, Box<Term>),
    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift
    )]
    BitwiseLeftShift(Box<Shift>, Box<Term>),

    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift,
        Term
    )]
    Addition(Box<Term>, Box<Factor>),
    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift,
        Term
    )]
    Subtraction(Box<Term>, Box<Factor>),

    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift,
        Term, Factor
    )]
    Multiplication(Box<Factor>, Box<Unary>),
    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift,
        Term, Factor
    )]
    Division(Box<Factor>, Box<Unary>),
    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift,
        Term, Factor
    )]
    Modulo(Box<Factor>, Box<Unary>),

    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift,
        Term, Factor, Primary
    )]
    NumberLiteral(String),
    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift,
        Term, Factor, Primary
    )]
    StringLiteral(String),
    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift,
        Term, Factor, Primary, IDENTIFIER
    )]
    Identifier(String),
    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift,
        Term, Factor, Primary
    )]
    BooleanLiteral(bool),
    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift,
        Term, Factor, Primary
    )]
    Parenthesized(Box<Expr>),

    #[subenum(
        Comma, Assignment, Unary, Postfix, Ternary, Logical, Equality, Bitwise, Comparison, Shift,
        Term, Factor, Primary
    )]
    Error,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Stmt {
    IfStmt(Expr, Box<Stmt>),
    IfElseStmt(Expr, Box<Stmt>, Box<Stmt>),
    WhileStmt(Expr, Box<Stmt>),
    ForStmt(Expr, Expr, Expr, Box<Stmt>),
    BlockStmt(Vec<Stmt>),
    ExprStmt(Expr),
    EmptyStmt,
    ContinueStmt,
    BreakStmt,
    ReturnStmt(Expr),
    EmptyReturnStmt,
    // TODO: Types are IDENTIFIER for now
    DeclarationList(IDENTIFIER, Vec<IDENTIFIER>),
}
