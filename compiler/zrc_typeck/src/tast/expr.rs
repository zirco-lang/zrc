//! Expression representation for the Zirco [TAST](super)

use std::fmt::Display;

pub use zrc_parser::{
    ast::expr::{Arithmetic, BinaryBitwise, Comparison, Equality, Logical},
    lexer::{NumberLiteral, StringTok, ZrcString},
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
#[expect(variant_size_differences)]
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
    /// `++x` - prefix increment (increments then returns new value)
    PrefixIncrement(Box<Place<'input>>),
    /// `--x` - prefix decrement (decrements then returns new value)
    PrefixDecrement(Box<Place<'input>>),

    /// `a[b]`
    Index(Box<TypedExpr<'input>>, Box<TypedExpr<'input>>),
    /// `a.b`
    Dot(Box<Place<'input>>, Spanned<&'input str>),
    /// `a(b, c, d, ...)`
    Call(Box<Place<'input>>, Vec<TypedExpr<'input>>),
    /// `x++` - postfix increment (returns old value then increments)
    PostfixIncrement(Box<Place<'input>>),
    /// `x--` - postfix decrement (returns old value then decrements)
    PostfixDecrement(Box<Place<'input>>),

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

    /// `new Type { field1: value1, field2: value2, ... }`
    StructConstruction(indexmap::IndexMap<&'input str, TypedExpr<'input>>),
    /// `[value1, value2, value3, ... ]`
    ArrayLiteral(Vec<TypedExpr<'input>>),

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

/// Precedence level for typed expressions. Higher values bind more tightly.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum Precedence {
    /// Comma operator (lowest precedence)
    Comma = 1,
    /// Assignment operators
    Assignment = 2,
    /// Ternary conditional
    Ternary = 3,
    /// Logical OR
    LogicalOr = 4,
    /// Logical AND
    LogicalAnd = 5,
    /// Bitwise OR
    BitwiseOr = 6,
    /// Bitwise XOR
    BitwiseXor = 7,
    /// Bitwise AND
    BitwiseAnd = 8,
    /// Equality operators
    Equality = 9,
    /// Comparison operators
    Comparison = 10,
    /// Bit shift operators
    BitShift = 11,
    /// Addition and subtraction
    Term = 12,
    /// Multiplication, division, modulo
    Factor = 13,
    /// Cast operator
    Cast = 14,
    /// Unary operators
    Unary = 15,
    /// Postfix operators (highest precedence)
    Postfix = 16,
    /// Primary expressions (literals, identifiers)
    Primary = 17,
}

impl TypedExprKind<'_> {
    /// Get the precedence level of this typed expression kind
    const fn precedence(&self) -> Precedence {
        match self {
            Self::Comma(_, _) => Precedence::Comma,
            Self::Assignment(_, _) => Precedence::Assignment,
            Self::Ternary(_, _, _) => Precedence::Ternary,
            Self::Logical(Logical::Or, _, _) => Precedence::LogicalOr,
            Self::Logical(Logical::And, _, _) => Precedence::LogicalAnd,
            Self::BinaryBitwise(BinaryBitwise::Or, _, _) => Precedence::BitwiseOr,
            Self::BinaryBitwise(BinaryBitwise::Xor, _, _) => Precedence::BitwiseXor,
            Self::BinaryBitwise(BinaryBitwise::And, _, _) => Precedence::BitwiseAnd,
            Self::BinaryBitwise(BinaryBitwise::Shl | BinaryBitwise::Shr, _, _) => {
                Precedence::BitShift
            }
            Self::Equality(_, _, _) => Precedence::Equality,
            Self::Comparison(_, _, _) => Precedence::Comparison,
            Self::Arithmetic(Arithmetic::Addition | Arithmetic::Subtraction, _, _) => {
                Precedence::Term
            }
            Self::Arithmetic(
                Arithmetic::Multiplication | Arithmetic::Division | Arithmetic::Modulo,
                _,
                _,
            ) => Precedence::Factor,
            Self::Cast(_, _) => Precedence::Cast,
            Self::UnaryNot(_)
            | Self::UnaryBitwiseNot(_)
            | Self::UnaryMinus(_)
            | Self::UnaryAddressOf(_)
            | Self::UnaryDereference(_)
            | Self::PrefixIncrement(_)
            | Self::PrefixDecrement(_)
            | Self::SizeOf(_) => Precedence::Unary,
            Self::Index(_, _)
            | Self::Dot(_, _)
            | Self::Call(_, _)
            | Self::PostfixIncrement(_)
            | Self::PostfixDecrement(_) => Precedence::Postfix,
            Self::NumberLiteral(_, _)
            | Self::StringLiteral(_)
            | Self::CharLiteral(_)
            | Self::Identifier(_)
            | Self::BooleanLiteral(_)
            | Self::StructConstruction(_)
            | Self::ArrayLiteral(_) => Precedence::Primary,
        }
    }

    /// Format a child expression with parentheses if needed based on
    /// precedence.
    fn fmt_child(
        f: &mut std::fmt::Formatter<'_>,
        child: &TypedExpr<'_>,
        parent_prec: Precedence,
        is_right: bool,
    ) -> std::fmt::Result {
        let child_prec = child.kind.value().precedence();
        let needs_parens = if is_right {
            // For right children, parenthesize if precedence is lower or equal
            // (left-associative)
            child_prec <= parent_prec
        } else {
            // For left children, parenthesize if precedence is strictly lower
            child_prec < parent_prec
        };

        if needs_parens {
            write!(f, "({child})")
        } else {
            write!(f, "{child}")
        }
    }
}

impl Display for Place<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind.value())
    }
}

impl Display for PlaceKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Deref(expr) => write!(f, "*{expr}"),
            Self::Variable(name) => write!(f, "{name}"),
            Self::Index(lhs, rhs) => write!(f, "{lhs}[{rhs}]"),
            Self::Dot(place, field) => write!(f, "{place}.{field}"),
        }
    }
}

impl Display for TypedExpr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} as {})", self.kind.value(), self.inferred_type)
    }
}

impl Display for TypedExprKind<'_> {
    #[expect(clippy::too_many_lines)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Comma(lhs, rhs) => {
                let prec = self.precedence();
                Self::fmt_child(f, lhs, prec, false)?;
                write!(f, ", ")?;
                Self::fmt_child(f, rhs, prec, true)
            }
            Self::Assignment(place, rhs) => {
                let prec = self.precedence();
                write!(f, "{place} = ")?;
                Self::fmt_child(f, rhs, prec, true)
            }
            Self::BinaryBitwise(op, lhs, rhs) => {
                let prec = self.precedence();
                Self::fmt_child(f, lhs, prec, false)?;
                write!(f, " {op} ")?;
                Self::fmt_child(f, rhs, prec, true)
            }
            Self::Logical(op, lhs, rhs) => {
                let prec = self.precedence();
                Self::fmt_child(f, lhs, prec, false)?;
                write!(f, " {op} ")?;
                Self::fmt_child(f, rhs, prec, true)
            }
            Self::Equality(op, lhs, rhs) => {
                let prec = self.precedence();
                Self::fmt_child(f, lhs, prec, false)?;
                write!(f, " {op} ")?;
                Self::fmt_child(f, rhs, prec, true)
            }
            Self::Comparison(op, lhs, rhs) => {
                let prec = self.precedence();
                Self::fmt_child(f, lhs, prec, false)?;
                write!(f, " {op} ")?;
                Self::fmt_child(f, rhs, prec, true)
            }
            Self::Arithmetic(op, lhs, rhs) => {
                let prec = self.precedence();
                Self::fmt_child(f, lhs, prec, false)?;
                write!(f, " {op} ")?;
                Self::fmt_child(f, rhs, prec, true)
            }
            Self::UnaryNot(expr) => {
                write!(f, "!")?;
                Self::fmt_child(f, expr, Precedence::Unary, true)
            }
            Self::UnaryBitwiseNot(expr) => {
                write!(f, "~")?;
                Self::fmt_child(f, expr, Precedence::Unary, true)
            }
            Self::UnaryMinus(expr) => {
                write!(f, "-")?;
                Self::fmt_child(f, expr, Precedence::Unary, true)
            }
            Self::UnaryAddressOf(place) => write!(f, "&{place}"),
            Self::UnaryDereference(expr) => {
                write!(f, "*")?;
                Self::fmt_child(f, expr, Precedence::Unary, true)
            }
            Self::PrefixIncrement(place) => write!(f, "++{place}"),
            Self::PrefixDecrement(place) => write!(f, "--{place}"),
            Self::Index(lhs, rhs) => {
                Self::fmt_child(f, lhs, Precedence::Postfix, false)?;
                write!(f, "[{rhs}]")
            }
            Self::Dot(place, field) => write!(f, "{place}.{field}"),
            Self::Call(place, args) => write!(
                f,
                "{place}({})",
                args.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::PostfixIncrement(place) => write!(f, "{place}++"),
            Self::PostfixDecrement(place) => write!(f, "{place}--"),
            Self::Ternary(cond, if_true, if_false) => {
                let prec = self.precedence();
                Self::fmt_child(f, cond, prec, false)?;
                write!(f, " ? {if_true} : ")?;
                Self::fmt_child(f, if_false, prec, true)
            }
            Self::Cast(expr, ty) => {
                let prec = self.precedence();
                Self::fmt_child(f, expr, prec, false)?;
                write!(f, " as {ty}")
            }
            Self::SizeOf(ty) => write!(f, "sizeof {ty}"),
            Self::StructConstruction(fields) => {
                write!(
                    f,
                    "{{ {} }}",
                    fields
                        .iter()
                        .map(|(name, expr)| format!("{name}: ({expr})"))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Self::NumberLiteral(num, _ty) => write!(f, "{num}"),
            Self::StringLiteral(string) => write!(f, "\"{string}\""),
            Self::CharLiteral(ch) => write!(f, "'{ch}'"),
            Self::Identifier(name) => write!(f, "{name}"),
            Self::BooleanLiteral(boolean) => write!(f, "{boolean}"),
            Self::ArrayLiteral(items) => {
                write!(
                    f,
                    "[{}]",
                    items
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use zrc_utils::spanned_test;

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
            kind: spanned_test!(0, TypedExprKind::Identifier("x"), 1),
        };
        assert_eq!(expr.to_string(), "(x as i32)");
    }

    #[test]
    fn place_displays_correctly() {
        let place = Place {
            inferred_type: Type::I32,
            kind: spanned_test!(0, PlaceKind::Variable("y"), 1),
        };
        assert_eq!(place.to_string(), "y");
    }
}
