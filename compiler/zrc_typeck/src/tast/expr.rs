//! Expression representation for the Zirco [TAST](super)

use std::fmt::Display;

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

impl Display for Place<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind.value())
    }
}

impl Display for PlaceKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Deref(expr) => write!(f, "(*({expr}))"),
            Self::Variable(name) => write!(f, "{name}"),
            Self::Index(lhs, rhs) => write!(f, "({lhs})[({rhs})]"),
            Self::Dot(place, field) => write!(f, "({place}).{field}"),
        }
    }
}

impl Display for TypedExpr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} as {})", self.kind.value(), self.inferred_type)
    }
}

impl Display for TypedExprKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Comma(lhs, rhs) => write!(f, "({lhs}), ({rhs})"),
            Self::Assignment(place, rhs) => write!(f, "({place}) = ({rhs})"),
            Self::BinaryBitwise(op, lhs, rhs) => write!(f, "({lhs}) {op} ({rhs})"),
            Self::Logical(op, lhs, rhs) => write!(f, "({lhs}) {op} ({rhs})"),
            Self::Equality(op, lhs, rhs) => write!(f, "({lhs}) {op} ({rhs})"),
            Self::Comparison(op, lhs, rhs) => write!(f, "({lhs}) {op} ({rhs})"),
            Self::Arithmetic(op, lhs, rhs) => write!(f, "({lhs}) {op} ({rhs})"),
            Self::UnaryNot(expr) => write!(f, "!({expr})"),
            Self::UnaryBitwiseNot(expr) => write!(f, "~({expr})"),
            Self::UnaryMinus(expr) => write!(f, "-({expr})"),
            Self::UnaryAddressOf(place) => write!(f, "&({place})"),
            Self::UnaryDereference(expr) => write!(f, "*({expr})"),
            Self::Index(lhs, rhs) => write!(f, "({lhs})[({rhs})]"),
            Self::Dot(place, field) => write!(f, "({place}).{field}"),
            Self::Call(place, args) => write!(
                f,
                "({place})({})",
                args.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::Ternary(cond, if_true, if_false) => {
                write!(f, "({cond}) ? ({if_true}) : ({if_false})")
            }
            Self::Cast(expr, ty) => write!(f, "({expr}) as {ty}"),
            Self::SizeOf(ty) => write!(f, "sizeof {ty}"),
            Self::NumberLiteral(num, _ty) => write!(f, "{num}"),
            Self::StringLiteral(string) => write!(f, "\"{string}\""),
            Self::CharLiteral(ch) => write!(f, "'{ch}'"),
            Self::Identifier(name) => write!(f, "{name}"),
            Self::BooleanLiteral(boolean) => write!(f, "{boolean}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use zrc_utils::spanned;

    use super::*;

    #[test]
    fn place_kind_variable_displays_correctly() {
        let place = PlaceKind::Variable("x");
        assert_eq!(place.to_string(), "x");
    }

    #[test]
    fn typed_expr_kind_identifier_displays_correctly() {
        let expr = TypedExprKind::Identifier("foo");
        assert_eq!(expr.to_string(), "foo");
    }

    #[test]
    fn typed_expr_kind_boolean_literal_displays_correctly() {
        let expr_true = TypedExprKind::BooleanLiteral(true);
        let expr_false = TypedExprKind::BooleanLiteral(false);
        assert_eq!(expr_true.to_string(), "true");
        assert_eq!(expr_false.to_string(), "false");
    }

    #[test]
    fn typed_expr_displays_with_type() {
        let expr = TypedExpr {
            inferred_type: Type::I32,
            kind: spanned!(0, TypedExprKind::Identifier("x"), 1),
        };
        assert_eq!(expr.to_string(), "(x as i32)");
    }

    #[test]
    fn place_displays_correctly() {
        let place = Place {
            inferred_type: Type::I32,
            kind: spanned!(0, PlaceKind::Variable("y"), 1),
        };
        assert_eq!(place.to_string(), "y");
    }
}
