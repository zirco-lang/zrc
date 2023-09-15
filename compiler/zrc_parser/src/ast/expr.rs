//! Expression representation for the Zirco AST
//!
//! The main thing within this module you will need is the [`Expr`] enum. It contains all the different expression kinds in Zirco.

use std::fmt::Display;

/// The enum representing the different kinds of expressions in Zirco
///
/// This enum represents all the different kinds of expressions in Zirco. It is used by the parser to represent the AST in the expression position.
#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    /// `a, b`
    Comma(Box<Expr>, Box<Expr>),

    /// `a = b`
    Assignment(Box<Expr>, Box<Expr>),
    /// `a += b`
    AdditionAssignment(Box<Expr>, Box<Expr>),
    /// `a -= b`
    SubtractionAssignment(Box<Expr>, Box<Expr>),
    /// `a *= b`
    MultiplicationAssignment(Box<Expr>, Box<Expr>),
    /// `a /= b`
    DivisionAssignment(Box<Expr>, Box<Expr>),
    /// `a %= b`
    ModuloAssignment(Box<Expr>, Box<Expr>),
    /// `a &= b`
    BitwiseAndAssignment(Box<Expr>, Box<Expr>),
    /// `a |= b`
    BitwiseOrAssignment(Box<Expr>, Box<Expr>),
    /// `a ^= b`
    BitwiseXorAssignment(Box<Expr>, Box<Expr>),
    /// `a <<= b`
    BitwiseLeftShiftAssignment(Box<Expr>, Box<Expr>),
    /// `a >>= b`
    BitwiseRightShiftAssignment(Box<Expr>, Box<Expr>),

    /// `!x`
    UnaryNot(Box<Expr>),
    /// `~x`
    UnaryBitwiseNot(Box<Expr>),
    /// `-x`
    UnaryMinus(Box<Expr>),
    /// `&x`
    UnaryAddressOf(Box<Expr>),
    /// `*x`
    UnaryDereference(Box<Expr>),

    /// `a[b]`
    Index(Box<Expr>, Box<Expr>),
    /// `a.b`
    Dot(Box<Expr>, String),
    /// `a->b`
    Arrow(Box<Expr>, String),
    /// `a(b, c, d, ...)`
    Call(Box<Expr>, Vec<Expr>),

    /// `a ? b : c`
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),

    /// `a && b`
    LogicalAnd(Box<Expr>, Box<Expr>),
    /// `a || b`
    LogicalOr(Box<Expr>, Box<Expr>),

    /// `a == b`
    Equals(Box<Expr>, Box<Expr>),
    /// `a != b`
    NotEquals(Box<Expr>, Box<Expr>),

    /// `a & b`
    BitwiseAnd(Box<Expr>, Box<Expr>),
    /// `a | b`
    BitwiseOr(Box<Expr>, Box<Expr>),
    /// `a ^ b`
    BitwiseXor(Box<Expr>, Box<Expr>),

    /// `a > b`
    GreaterThan(Box<Expr>, Box<Expr>),
    /// `a >= b`
    GreaterThanOrEqualTo(Box<Expr>, Box<Expr>),
    /// `a < b`
    LessThan(Box<Expr>, Box<Expr>),
    /// `a <= b`
    LessThanOrEqualTo(Box<Expr>, Box<Expr>),

    /// `a >> b`
    BitwiseRightShift(Box<Expr>, Box<Expr>),
    /// `a << b`
    BitwiseLeftShift(Box<Expr>, Box<Expr>),

    /// `a + b`
    Addition(Box<Expr>, Box<Expr>),
    /// `a - b`
    Subtraction(Box<Expr>, Box<Expr>),

    /// `a * b`
    Multiplication(Box<Expr>, Box<Expr>),
    /// `a / b`
    Division(Box<Expr>, Box<Expr>),
    /// `a % b`
    Modulo(Box<Expr>, Box<Expr>),

    /// Any numeric literal.
    NumberLiteral(String),
    /// Any string literal.
    StringLiteral(String),
    /// Any identifier.
    Identifier(String),
    /// Any boolean literal.
    BooleanLiteral(bool),

    /// An error occurred while parsing.
    Error,
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        match self {
            Expr::NumberLiteral(n) => write!(f, "{}", n),
            Expr::StringLiteral(s) => write!(f, "{}", s),
            Expr::Identifier(i) => write!(f, "{}", i),
            Expr::BooleanLiteral(b) => write!(f, "{}", b),
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
            Expr::UnaryAddressOf(e) => write!(f, "&{}", e),
            Expr::UnaryDereference(e) => write!(f, "*{}", e),
            Expr::Arrow(l, r) => write!(f, "{}->{}", l, r),
            Expr::Ternary(l, m, r) => write!(f, "{} ? {} : {}", l, m, r),
            Expr::Index(a, b) => write!(f, "{}[{}]", a, b),
            Expr::Dot(a, b) => write!(f, "{}.{}", a, b),
            Expr::Call(a, b) => write!(
                f,
                "{}({})",
                a,
                b.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }?;
        write!(f, ")")?;
        Ok(())
    }
}
