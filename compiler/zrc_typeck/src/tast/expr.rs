//! Expression representation for the Zirco [TAST](super)

use std::{fmt::Display, string::ToString};

pub use zrc_parser::{
    ast::expr::{Arithmetic, BinaryBitwise, Comparison, Equality, Logical},
    lexer::{NumberLiteral, StringTok},
};

use super::ty::Type;

/// The left hand side of an assignment.
#[derive(PartialEq, Debug, Clone)]
pub struct Place<'input> {
    /// The inferred [`Type`] of this node
    pub inferred_type: Type<'input>,
    /// The actual [`PlaceKind`] backing this expression
    pub kind: PlaceKind<'input>,
}
impl<'input> Display for Place<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}) as ({})", self.inferred_type, self.kind)
    }
}

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
impl<'input> Display for PlaceKind<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Deref(expr) => write!(f, "*{expr}"),
            Self::Variable(ident) => write!(f, "{ident}"),
            Self::Index(ptr, idx) => write!(f, "{ptr}[{idx}]"),
            Self::Dot(expr, key) => write!(f, "{expr}.{key}"),
        }
    }
}

/// An [expression kind](TypedExprKind) with its yielded [result
/// type](super::ty::Type) attached to it.
#[derive(PartialEq, Debug, Clone)]
#[allow(clippy::module_name_repetitions)]
pub struct TypedExpr<'input> {
    /// The inferred [`Type`] of this node
    pub inferred_type: Type<'input>,
    /// The actual [`TypedExprKind`] backing this expression
    pub kind: TypedExprKind<'input>,
}
impl<'input> Display for TypedExpr<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(({}) as ({}))", self.inferred_type, self.kind)
    }
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
    Call(Box<Place<'input>>, Vec<TypedExpr<'input>>),

    /// `a ? b : c`
    Ternary(
        Box<TypedExpr<'input>>,
        Box<TypedExpr<'input>>,
        Box<TypedExpr<'input>>,
    ),

    /// `x as T`
    Cast(Box<TypedExpr<'input>>, Type<'input>),
    /// `sizeof(T)`
    SizeOf(Type<'input>),

    /// Any numeric literal.
    NumberLiteral(NumberLiteral<'input>),
    /// Any string literal.bool
    StringLiteral(Vec<StringTok<'input>>),
    /// Any char literal
    CharLiteral(Vec<StringTok<'input>>),
    /// Any identifier.
    Identifier(&'input str),
    /// Any boolean literal.
    BooleanLiteral(bool),
}
impl<'input> Display for TypedExprKind<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NumberLiteral(n) => write!(f, "{n}"),
            Self::StringLiteral(str) => write!(
                f,
                "\"{}\"",
                str.iter().map(ToString::to_string).collect::<String>()
            ),
            Self::CharLiteral(str) => write!(
                f,
                "\'{}\'",
                str.iter().map(ToString::to_string).collect::<String>()
            ),
            Self::Identifier(i) => write!(f, "{i}"),
            Self::BooleanLiteral(value) => write!(f, "{value}"),
            Self::Assignment(place, value) => write!(f, "{place} = {value}"),
            Self::Equality(operator, lhs, rhs) => write!(f, "{lhs} {operator} {rhs}"),
            Self::Comparison(operator, lhs, rhs) => write!(f, "{lhs} {operator} {rhs}"),
            Self::Arithmetic(operator, lhs, rhs) => write!(f, "{lhs} {operator} {rhs}"),
            Self::BinaryBitwise(operator, lhs, rhs) => write!(f, "{lhs} {operator} {rhs}"),
            Self::Logical(operator, lhs, rhs) => write!(f, "{lhs} {operator} {rhs}"),
            Self::Comma(lhs, rhs) => write!(f, "{lhs}, {rhs}"),
            Self::UnaryNot(expr) => write!(f, "!{expr}"),
            Self::UnaryBitwiseNot(expr) => write!(f, "~{expr}"),
            Self::UnaryMinus(expr) => write!(f, "-{expr}"),
            Self::UnaryAddressOf(expr) => write!(f, "&{expr}"),
            Self::UnaryDereference(expr) => write!(f, "*{expr}"),
            Self::Ternary(cond, if_true, if_false) => write!(f, "{cond} ? {if_true} : {if_false}"),
            Self::Index(ptr, idx) => write!(f, "{ptr}[{idx}]"),
            Self::Dot(expr, key) => write!(f, "{expr}.{key}"),
            Self::Cast(expr, ty) => write!(f, "{expr} as {ty}"),
            Self::SizeOf(ty) => write!(f, "sizeof({ty})"),
            Self::Call(expr, args) => write!(
                f,
                "{expr}({})",
                args.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}
