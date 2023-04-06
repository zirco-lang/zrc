use std::fmt::Display;
use subenum::subenum;

/// Translate into a different sub-enum of Expr
#[macro_export]
macro_rules! into_expr_type {
    ($to:tt,$val:expr) => {
        $crate::parse_tree::expr::$to::try_from($crate::parse_tree::expr::Expr::from($val)).unwrap()
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

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::NumberLiteral(n) => write!(f, "{}", n),
            Expr::StringLiteral(s) => write!(f, "{}", s),
            Expr::Identifier(i) => write!(f, "{}", i),
            Expr::BooleanLiteral(b) => write!(f, "{}", b),
            Expr::Parenthesized(e) => write!(f, "({})", e),
            Expr::Error => write!(f, "error"),
            Expr::Assignment(l, r) => write!(f, "{} = {}", l, r),
            Expr::Addition(l, r) => write!(f, "{} + {}", l, r),
            Expr::Subtraction(l, r) => write!(f, "{} - {}", l, r),
            Expr::Multiplication(l, r) => write!(f, "{} * {}", l, r),
            Expr::Division(l, r) => write!(f, "{} / {}", l, r),
            Expr::Modulo(l, r) => write!(f, "{} % {}", l, r),
            Expr::BitwiseAnd(l, r) => write!(f, "{} & {}", l, r),
            Expr::BitwiseOr(l, r) => write!(f, "{} | {}", l, r),
            Expr::BitwiseXor(l, r) => write!(f, "{} ^ {}", l, r),
            Expr::BitwiseLeftShift(l, r) => write!(f, "{} << {}", l, r),
            Expr::BitwiseRightShift(l, r) => write!(f, "{} >> {}", l, r),
            Expr::GreaterThan(l, r) => write!(f, "{} > {}", l, r),
            Expr::GreaterThanOrEqualTo(l, r) => write!(f, "{} >= {}", l, r),
            Expr::LessThan(l, r) => write!(f, "{} < {}", l, r),
            Expr::LessThanOrEqualTo(l, r) => write!(f, "{} <= {}", l, r),
            Expr::Equals(l, r) => write!(f, "{} == {}", l, r),
            Expr::NotEquals(l, r) => write!(f, "{} != {}", l, r),
            Expr::LogicalAnd(l, r) => write!(f, "{} && {}", l, r),
            Expr::LogicalOr(l, r) => write!(f, "{} || {}", l, r),
            Expr::Comma(l, r) => write!(f, "{}, {}", l, r),
            Expr::AdditionAssignment(l, r) => write!(f, "{} += {}", l, r),
            Expr::SubtractionAssignment(l, r) => write!(f, "{} -= {}", l, r),
            Expr::MultiplicationAssignment(l, r) => write!(f, "{} *= {}", l, r),
            Expr::DivisionAssignment(l, r) => write!(f, "{} /= {}", l, r),
            Expr::ModuloAssignment(l, r) => write!(f, "{} %= {}", l, r),
            Expr::BitwiseAndAssignment(l, r) => write!(f, "{} &= {}", l, r),
            Expr::BitwiseOrAssignment(l, r) => write!(f, "{} |= {}", l, r),
            Expr::BitwiseXorAssignment(l, r) => write!(f, "{} ^= {}", l, r),
            Expr::BitwiseLeftShiftAssignment(l, r) => write!(f, "{} <<= {}", l, r),
            Expr::BitwiseRightShiftAssignment(l, r) => write!(f, "{} >>= {}", l, r),
            Expr::UnaryNot(e) => write!(f, "!{}", e),
            Expr::UnaryBitwiseNot(e) => write!(f, "~{}", e),
            Expr::UnaryMinus(e) => write!(f, "-{}", e),
            Expr::PostIncrement(e) => write!(f, "{}++", e),
            Expr::PostDecrement(e) => write!(f, "{}--", e),
            Expr::PreIncrement(e) => write!(f, "++{}", e),
            Expr::PreDecrement(e) => write!(f, "--{}", e),
            Expr::Ternary(l, m, r) => write!(f, "{} ? {} : {}", l, m, r),
            Expr::Index(a, b) => write!(f, "{}[{}]", a, b),
            Expr::Dot(a, b) => write!(f, "{}.{}", a, b),
            Expr::NamespaceAccess(a, b) => write!(f, "{}::{}", a, b),
            Expr::Call(a, b) => write!(
                f,
                "{}({})",
                a,
                b.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

macro_rules! impl_display_for_expr_subenum {
    ($n:ident) => {
        impl Display for $n {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", Expr::from(self.clone()))
            }
        }
    };
}

impl_display_for_expr_subenum!(Comma);
impl_display_for_expr_subenum!(Assignment);
impl_display_for_expr_subenum!(Unary);
impl_display_for_expr_subenum!(Postfix);
impl_display_for_expr_subenum!(Ternary);
impl_display_for_expr_subenum!(Logical);
impl_display_for_expr_subenum!(Equality);
impl_display_for_expr_subenum!(Bitwise);
impl_display_for_expr_subenum!(Comparison);
impl_display_for_expr_subenum!(Shift);
impl_display_for_expr_subenum!(Term);
impl_display_for_expr_subenum!(Factor);
impl_display_for_expr_subenum!(Primary);
impl_display_for_expr_subenum!(IDENTIFIER);
