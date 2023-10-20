//! Expression representation for the Zirco [TAST](super)

use std::fmt::Display;

pub use zrc_parser::ast::expr::{Arithmetic, BinaryBitwise, Comparison, Equality, Logical};

/// An [expression kind](TypedExprKind) with its yielded [result
/// type](super::ty::Type) attached to it.
#[derive(PartialEq, Debug, Clone)]
#[allow(clippy::module_name_repetitions)]
pub struct TypedExpr<'input>(pub super::ty::Type<'input>, pub TypedExprKind<'input>);

/// The left hand side of an assignment.
#[derive(PartialEq, Debug, Clone)]
pub struct Place<'input>(pub super::ty::Type<'input>, pub PlaceKind<'input>);
/// The valid left-hand-side of a [`TypedExprKind::Assignment`].
///
/// Places may be:
/// - A variable or an property access of a place
/// - A dereference or index into any expression yielding a pointer
#[derive(PartialEq, Debug, Clone)]
pub enum PlaceKind<'input> {
    /// `*x`
    Deref(Box<TypedExpr<'input>>),
    /// `x`
    Variable(&'input str),
    /// `x[y]`
    Index(Box<TypedExpr<'input>>, Box<TypedExpr<'input>>),
    /// `x.y`
    Dot(Box<Place<'input>>, &'input str),
}

/// The kind of a [`TypedExpr`]
#[derive(PartialEq, Debug, Clone)]
pub enum TypedExprKind<'input> {
    /// `a, b`
    Comma(Box<TypedExpr<'input>>, Box<TypedExpr<'input>>),

    /// `a = b`
    ///
    /// `a += b` is desugared to `a = a + b` by the type checker.
    Assignment(Box<Place<'input>>, Box<TypedExpr<'input>>),

    /// Bitwise operations
    BinaryBitwise(
        BinaryBitwise,
        Box<TypedExpr<'input>>,
        Box<TypedExpr<'input>>,
    ),
    /// Logical operations
    Logical(Logical, Box<TypedExpr<'input>>, Box<TypedExpr<'input>>),
    /// Equality checks
    Equality(Equality, Box<TypedExpr<'input>>, Box<TypedExpr<'input>>),
    /// Comparisons
    Comparison(Comparison, Box<TypedExpr<'input>>, Box<TypedExpr<'input>>),
    /// Arithmetic operations
    Arithmetic(Arithmetic, Box<TypedExpr<'input>>, Box<TypedExpr<'input>>),

    /// `!x`
    UnaryNot(Box<TypedExpr<'input>>),
    /// `~x`
    UnaryBitwiseNot(Box<TypedExpr<'input>>),
    /// `-x`
    UnaryMinus(Box<TypedExpr<'input>>),
    /// `&x`
    UnaryAddressOf(Box<Place<'input>>),
    /// `*x`
    UnaryDereference(Box<TypedExpr<'input>>),

    /// `a[b]`
    Index(Box<TypedExpr<'input>>, Box<TypedExpr<'input>>),
    /// `a.b`
    Dot(Box<Place<'input>>, &'input str),
    /// `a(b, c, d, ...)`
    Call(Box<TypedExpr<'input>>, Vec<TypedExpr<'input>>),

    /// `a ? b : c`
    Ternary(
        Box<TypedExpr<'input>>,
        Box<TypedExpr<'input>>,
        Box<TypedExpr<'input>>,
    ),

    /// `x as T`
    Cast(Box<TypedExpr<'input>>, super::ty::Type<'input>),

    /// Any numeric literal.
    NumberLiteral(&'input str),
    /// Any string literal.bool
    StringLiteral(&'input str),
    /// Any identifier.
    Identifier(&'input str),
    /// Any boolean literal.
    BooleanLiteral(bool),
}

impl<'input> Display for TypedExprKind<'input> {
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
impl<'input> Display for PlaceKind<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Deref(e) => write!(f, "*{e}"),
            Self::Variable(s) => write!(f, "{s}"),
            Self::Index(a, b) => write!(f, "{a}[{b}]"),
            Self::Dot(a, b) => write!(f, "{a}.{b}"),
        }
    }
}
impl<'input> Display for Place<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}) as ({})", self.1, self.0)
    }
}

impl<'input> Display for TypedExpr<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(({}) as ({}))", self.1, self.0)
    }
}
