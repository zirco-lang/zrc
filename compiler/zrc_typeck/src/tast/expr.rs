//! Expression representation for the Zirco [TAST](super)

use zrc_parser::lexer::ZrcString;
pub use zrc_parser::{
    ast::expr::{Arithmetic, BinaryBitwise, Comparison, Equality, Logical},
    lexer::{NumberLiteral, StringTok},
};
use zrc_utils::span::Spanned;

use super::ty::Type;

/// The left hand side of an assignment.
#[derive(PartialEq, Debug, Clone)]
pub struct Place<'input> {
    /// The inferred [`Type`] of this node
    pub inferred_type: Type<'input>,
    /// The actual [`PlaceKind`] backing this expression
    pub kind: Spanned<PlaceKind<'input>>,
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
    Dot(Box<Place<'input>>, Spanned<&'input str>),
}

/// An [expression kind](TypedExprKind) with its yielded [result
/// type](super::ty::Type) attached to it.
#[derive(PartialEq, Debug, Clone)]
pub struct TypedExpr<'input> {
    /// The inferred [`Type`] of this node
    pub inferred_type: Type<'input>,
    /// The actual [`TypedExprKind`] backing this expression
    pub kind: Spanned<TypedExprKind<'input>>,
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
    Dot(Box<Place<'input>>, Spanned<&'input str>),
    /// `a(b, c, d, ...)`
    Call(Box<Place<'input>>, Vec<TypedExpr<'input>>),

    /// `a ? b : c`
    Ternary(
        Box<TypedExpr<'input>>,
        Box<TypedExpr<'input>>,
        Box<TypedExpr<'input>>,
    ),

    /// `x as T`
    Cast(Box<TypedExpr<'input>>, Spanned<Type<'input>>),
    /// `sizeof(T)`
    SizeOf(Type<'input>),

    /// Any numeric literal.
    NumberLiteral(NumberLiteral<'input>, Type<'input>),
    /// Any string literal.
    StringLiteral(ZrcString<'input>),
    /// Any char literal
    CharLiteral(StringTok<'input>),
    /// Any identifier.
    Identifier(&'input str),
    /// Any boolean literal.
    BooleanLiteral(bool),
}

impl<'input> TypedExpr<'input> {
    /// Check if this expression is a constant expression
    /// 
    /// Constant expressions are:
    /// - Numeric literals
    /// - Boolean literals
    /// - Character literals
    /// - String literals
    /// - `sizeof` expressions
    /// - Arithmetic, bitwise, logical, equality, and comparison operations on constant expressions
    /// - Unary operations on constant expressions (except address-of and dereference)
    /// - Casts of constant expressions
    /// - Ternary operations where all operands are constant
    #[must_use]
    pub fn is_constant(&self) -> bool {
        match self.kind.value() {
            TypedExprKind::NumberLiteral(_, _)
            | TypedExprKind::BooleanLiteral(_)
            | TypedExprKind::CharLiteral(_)
            | TypedExprKind::StringLiteral(_)
            | TypedExprKind::SizeOf(_) => true,

            TypedExprKind::Arithmetic(_, lhs, rhs)
            | TypedExprKind::BinaryBitwise(_, lhs, rhs)
            | TypedExprKind::Logical(_, lhs, rhs)
            | TypedExprKind::Equality(_, lhs, rhs)
            | TypedExprKind::Comparison(_, lhs, rhs) => lhs.is_constant() && rhs.is_constant(),

            TypedExprKind::UnaryNot(expr)
            | TypedExprKind::UnaryBitwiseNot(expr)
            | TypedExprKind::UnaryMinus(expr) => expr.is_constant(),

            TypedExprKind::Cast(expr, _) => expr.is_constant(),

            TypedExprKind::Ternary(cond, if_true, if_false) => {
                cond.is_constant() && if_true.is_constant() && if_false.is_constant()
            }

            // These are not constant expressions
            TypedExprKind::Comma(_, _)
            | TypedExprKind::Assignment(_, _)
            | TypedExprKind::UnaryAddressOf(_)
            | TypedExprKind::UnaryDereference(_)
            | TypedExprKind::Index(_, _)
            | TypedExprKind::Dot(_, _)
            | TypedExprKind::Call(_, _)
            | TypedExprKind::Identifier(_) => false,
        }
    }
}
