//! Expression representation for the Zirco [TAST](super)

use std::fmt::Display;

pub use zrc_parser::ast::expr::{Arithmetic, BinaryBitwise, Comparison, Equality, Logical};

/// An [expression kind](TypedExprKind) with its yielded [result
/// type](super::ty::Type) attached to it.
#[derive(PartialEq, Debug, Clone)]
#[allow(clippy::module_name_repetitions)]
pub struct TypedExpr(pub super::ty::Type, pub TypedExprKind);

/// The left hand side of an assignment.
#[derive(PartialEq, Debug, Clone)]
pub struct Place(pub super::ty::Type, pub PlaceKind);
/// The valid left-hand-side of a [`TypedExprKind::Assignment`].
///
/// Places may be:
/// - A variable or an property access of a place
/// - A dereference or index into any expression yielding a pointer
#[derive(PartialEq, Debug, Clone)]
pub enum PlaceKind {
    /// `*x`
    Deref(Box<TypedExpr>),
    /// `x`
    Variable(String),
    /// `x[y]`
    Index(Box<TypedExpr>, Box<TypedExpr>),
    /// `x.y`
    Dot(Box<Place>, String),
}

/// The kind of a [`TypedExpr`]
#[derive(PartialEq, Debug, Clone)]
pub enum TypedExprKind {
    /// `a, b`
    Comma(Box<TypedExpr>, Box<TypedExpr>),

    /// `a = b`
    ///
    /// `a += b` is desugared to `a = a + b` by the type checker.
    Assignment(Box<Place>, Box<TypedExpr>),

    /// Bitwise operations
    BinaryBitwise(BinaryBitwise, Box<TypedExpr>, Box<TypedExpr>),
    /// Logical operations
    Logical(Logical, Box<TypedExpr>, Box<TypedExpr>),
    /// Equality checks
    Equality(Equality, Box<TypedExpr>, Box<TypedExpr>),
    /// Comparisons
    Comparison(Comparison, Box<TypedExpr>, Box<TypedExpr>),
    /// Arithmetic operations
    Arithmetic(Arithmetic, Box<TypedExpr>, Box<TypedExpr>),

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
    /// `a(b, c, d, ...)`
    Call(Box<TypedExpr>, Vec<TypedExpr>),

    /// `a ? b : c`
    Ternary(Box<TypedExpr>, Box<TypedExpr>, Box<TypedExpr>),

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
            Self::Equality(operator, lhs, rhs) => write!(f, "{lhs} {operator} {rhs}"),
            Self::Comparison(operator, lhs, rhs) => write!(f, "{lhs} {operator} {rhs}"),
            Self::Arithmetic(operator, lhs, rhs) => write!(f, "{lhs} {operator} {rhs}"),
            Self::BinaryBitwise(op, l, r) => write!(f, "{l} {op} {r}"),
            Self::Logical(op, l, r) => write!(f, "{l} {op} {r}"),
            Self::Comma(l, r) => write!(f, "{l}, {r}"),
            Self::UnaryNot(e) => write!(f, "!{e}"),
            Self::UnaryBitwiseNot(e) => write!(f, "~{e}"),
            Self::UnaryMinus(e) => write!(f, "-{e}"),
            Self::UnaryAddressOf(e) => write!(f, "&{e}"),
            Self::UnaryDereference(e) => write!(f, "*{e}"),
            Self::Ternary(l, m, r) => write!(f, "{l} ? {m} : {r}"),
            Self::Index(a, b) => write!(f, "{a}[{b}]"),
            Self::Dot(a, b) => write!(f, "{a}.{b}"),
            Self::Cast(a, t) => write!(f, "{a} as {t}"),
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
impl Display for PlaceKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Deref(e) => write!(f, "*{e}"),
            Self::Variable(s) => write!(f, "{s}"),
            Self::Index(a, b) => write!(f, "{a}[{b}]"),
            Self::Dot(a, b) => write!(f, "{a}.{b}"),
        }
    }
}
impl Display for Place {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}) as ({})", self.1, self.0)
    }
}

impl Display for TypedExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(({}) as ({}))", self.1, self.0)
    }
}
