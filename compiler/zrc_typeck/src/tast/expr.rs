//! Expression representation for the Zirco [TAST](super)

use std::fmt::Display;

/// An [expression kind](TypedExprKind) with its yielded [result type](super::ty::Type) attached to it.
#[derive(PartialEq, Debug, Clone)]
#[allow(clippy::module_name_repetitions)]
pub struct TypedExpr(pub super::ty::Type, pub TypedExprKind);

/// The kind of a [`TypedExpr`]
#[derive(PartialEq, Debug, Clone)]
pub enum TypedExprKind {
    /// `a, b`
    Comma(Box<TypedExpr>, Box<TypedExpr>),

    /// `a = b`
    Assignment(Box<TypedExpr>, Box<TypedExpr>),

    /// `!x`
    UnaryNot(Box<TypedExpr>),
    /// `~x`
    UnaryBitwiseNot(Box<TypedExpr>),
    /// `-x`
    UnaryMinus(Box<TypedExpr>),
    /// `&x`
    UnaryAddressOf(Box<TypedExpr>),
    /// `*x`
    UnaryDereference(Box<TypedExpr>),

    /// `a[b]`
    Index(Box<TypedExpr>, Box<TypedExpr>),
    /// `a.b`
    Dot(Box<TypedExpr>, String),
    /// `a->b`
    Arrow(Box<TypedExpr>, String),
    /// `a(b, c, d, ...)`
    Call(Box<TypedExpr>, Vec<TypedExpr>),

    /// `a ? b : c`
    Ternary(Box<TypedExpr>, Box<TypedExpr>, Box<TypedExpr>),

    /// `a && b`
    LogicalAnd(Box<TypedExpr>, Box<TypedExpr>),
    /// `a || b`
    LogicalOr(Box<TypedExpr>, Box<TypedExpr>),

    /// `a == b`
    Equals(Box<TypedExpr>, Box<TypedExpr>),
    /// `a != b`
    NotEquals(Box<TypedExpr>, Box<TypedExpr>),

    /// `a & b`
    BitwiseAnd(Box<TypedExpr>, Box<TypedExpr>),
    /// `a | b`
    BitwiseOr(Box<TypedExpr>, Box<TypedExpr>),
    /// `a ^ b`
    BitwiseXor(Box<TypedExpr>, Box<TypedExpr>),

    /// `a > b`
    GreaterThan(Box<TypedExpr>, Box<TypedExpr>),
    /// `a >= b`
    GreaterThanOrEqualTo(Box<TypedExpr>, Box<TypedExpr>),
    /// `a < b`
    LessThan(Box<TypedExpr>, Box<TypedExpr>),
    /// `a <= b`
    LessThanOrEqualTo(Box<TypedExpr>, Box<TypedExpr>),

    /// `a >> b`
    BitwiseRightShift(Box<TypedExpr>, Box<TypedExpr>),
    /// `a << b`
    BitwiseLeftShift(Box<TypedExpr>, Box<TypedExpr>),

    /// `a + b`
    Addition(Box<TypedExpr>, Box<TypedExpr>),
    /// `a - b`
    Subtraction(Box<TypedExpr>, Box<TypedExpr>),

    /// `a * b`
    Multiplication(Box<TypedExpr>, Box<TypedExpr>),
    /// `a / b`
    Division(Box<TypedExpr>, Box<TypedExpr>),
    /// `a % b`
    Modulo(Box<TypedExpr>, Box<TypedExpr>),

    /// `x as T`
    Cast(Box<TypedExpr>, super::ty::Type),

    /// Any numeric literal.
    NumberLiteral(String),
    /// Any string literal.
    StringLiteral(String),
    /// Any identifier.
    Identifier(String),
    /// Any boolean literal.
    BooleanLiteral(bool),
}

impl Display for TypedExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NumberLiteral(n) => write!(f, "{n}"),
            Self::StringLiteral(s) => write!(f, "{s}"),
            Self::Identifier(i) => write!(f, "{i}"),
            Self::BooleanLiteral(b) => write!(f, "{b}"),
            Self::Assignment(l, r) => write!(f, "{l} = {r}"),
            Self::Addition(l, r) => write!(f, "{l} + {r}"),
            Self::Subtraction(l, r) => write!(f, "{l} - {r}"),
            Self::Multiplication(l, r) => write!(f, "{l} * {r}"),
            Self::Division(l, r) => write!(f, "{l} / {r}"),
            Self::Modulo(l, r) => write!(f, "{l} % {r}"),
            Self::BitwiseAnd(l, r) => write!(f, "{l} & {r}"),
            Self::BitwiseOr(l, r) => write!(f, "{l} | {r}"),
            Self::BitwiseXor(l, r) => write!(f, "{l} ^ {r}"),
            Self::BitwiseLeftShift(l, r) => write!(f, "{l} << {r}"),
            Self::BitwiseRightShift(l, r) => write!(f, "{l} >> {r}"),
            Self::GreaterThan(l, r) => write!(f, "{l} > {r}"),
            Self::GreaterThanOrEqualTo(l, r) => write!(f, "{l} >= {r}"),
            Self::LessThan(l, r) => write!(f, "{l} < {r}"),
            Self::LessThanOrEqualTo(l, r) => write!(f, "{l} <= {r}"),
            Self::Equals(l, r) => write!(f, "{l} == {r}"),
            Self::NotEquals(l, r) => write!(f, "{l} != {r}"),
            Self::LogicalAnd(l, r) => write!(f, "{l} && {r}"),
            Self::LogicalOr(l, r) => write!(f, "{l} || {r}"),
            Self::Comma(l, r) => write!(f, "{l}, {r}"),
            Self::UnaryNot(e) => write!(f, "!{e}"),
            Self::Cast(x, t) => write!(f, "{x} as {t}"),
            Self::UnaryBitwiseNot(e) => write!(f, "~{e}"),
            Self::UnaryMinus(e) => write!(f, "-{e}"),
            Self::UnaryAddressOf(e) => write!(f, "&{e}"),
            Self::UnaryDereference(e) => write!(f, "*{e}"),
            Self::Arrow(l, r) => write!(f, "{l}->{r}"),
            Self::Ternary(a, b, c) => write!(f, "{a} ? {b} : {c}"),
            Self::Index(a, b) => write!(f, "{a}[{b}]"),
            Self::Dot(a, b) => write!(f, "{a}.{b}"),
            Self::Call(a, b) => write!(
                f,
                "{a}({})",
                b.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

impl Display for TypedExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(({}) as ({}))", self.1, self.0)
    }
}
