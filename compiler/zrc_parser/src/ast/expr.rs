//! Expression representation for the Zirco AST
//!
//! The main thing within this module you will need is the [`Expr`] struct.

use std::fmt::Display;

use zrc_utils::{
    span::{Span, Spannable, Spanned},
    spanned,
};

use super::ty::Type;
use crate::lexer::{NumberLiteral, StringTok, ZrcString};

/// Arithmetic operators
///
/// For an operator to be considered an arithmetic operator, it must meet the
/// following criteria:
/// - Operates on two integers, or a pointer (lhs) an an integer (rhs)
/// - Both operands must be the same type (except for pointer arithmetic)
/// - The result type is the same as the operand types (or a pointer, for
///   pointer arithmetic)
/// - Performs some mathematical operation
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Arithmetic {
    /// `+`
    Addition,
    /// `-`
    Subtraction,
    /// `*`
    Multiplication,
    /// `/`
    Division,
    /// `%`
    Modulo,
}
impl Display for Arithmetic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Addition => write!(f, "+"),
            Self::Subtraction => write!(f, "-"),
            Self::Multiplication => write!(f, "*"),
            Self::Division => write!(f, "/"),
            Self::Modulo => write!(f, "%"),
        }
    }
}

/// Assignment operators
///
/// All possible forms of assignments with operational variations.
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Assignment {
    /// `=`
    Standard,
    /// Any form of assignment via [`Arithmetic`] operator (e.g. `+=`)
    Arithmetic(Arithmetic),
    /// Any form of assignment via [`BinaryBitwise`] operator (e.g. `&=`)
    BinaryBitwise(BinaryBitwise),
}
impl Display for Assignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Standard => write!(f, "="),
            Self::Arithmetic(op) => write!(f, "{op}="),
            Self::BinaryBitwise(op) => write!(f, "{op}="),
        }
    }
}

/// Binary bitwise operators
///
/// For an operator to be considered a binary bitwise operator, it must meet the
/// following criteria:
/// - Operates on two integers
/// - Both operands must be the same type
/// - The result type is the same as the operand types
/// - Performs some bitwise operation
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum BinaryBitwise {
    /// `&`
    And,
    /// `|`
    Or,
    /// `^`
    Xor,
    /// `<<`
    Shl,
    /// `>>`
    Shr,
}
impl Display for BinaryBitwise {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::And => write!(f, "&"),
            Self::Or => write!(f, "|"),
            Self::Xor => write!(f, "^"),
            Self::Shl => write!(f, "<<"),
            Self::Shr => write!(f, ">>"),
        }
    }
}

/// Logical operators
///
/// For an operand to be considered a logical operator, it must meet the
/// following criteria:
/// - Operates on two booleans
/// - The result type is a boolean
/// - Performs some logical operation
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Logical {
    /// `&&`
    And,
    /// `||`
    Or,
}
impl Display for Logical {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::And => write!(f, "&&"),
            Self::Or => write!(f, "||"),
        }
    }
}

/// Equality checks
///
/// For an operand to be considered an equality check, it must meet the
/// following criteria:
/// - Operates on two values of the same type
/// - The result type is a boolean
/// - Performs some equality or inequality check
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Equality {
    /// `==`
    Eq,
    /// `!=`
    Neq,
}
impl Display for Equality {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eq => write!(f, "=="),
            Self::Neq => write!(f, "!="),
        }
    }
}

/// Comparison checks
///
/// For an operand to be considered a comparison check, it must meet the
/// following criteria:
/// - Operates on two integers
/// - Both operands must be the same type
/// - The result type is a boolean
/// - Performs some comparison or order check
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Comparison {
    /// `>`
    Gt,
    /// `>=`
    Gte,
    /// `<`
    Lt,
    /// `<=`
    Lte,
}
impl Display for Comparison {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Gt => write!(f, ">"),
            Self::Gte => write!(f, ">="),
            Self::Lt => write!(f, "<"),
            Self::Lte => write!(f, "<="),
        }
    }
}

/// A Zirco expression
#[derive(PartialEq, Debug, Clone)]
pub struct Expr<'input>(pub Spanned<ExprKind<'input>>);

impl<'input> Display for Expr<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.value().fmt(f)
    }
}

/// The enum representing the different kinds of expressions in Zirco
///
/// This enum represents all the different kinds of expressions in Zirco. It is
/// used by the parser to represent the AST in the expression position.
#[derive(PartialEq, Debug, Clone)]
#[allow(clippy::module_name_repetitions)]
pub enum ExprKind<'input> {
    /// `a, b`
    Comma(Box<Expr<'input>>, Box<Expr<'input>>),

    /// Assignment operations
    Assignment(Assignment, Box<Expr<'input>>, Box<Expr<'input>>),
    /// Bitwise operations
    BinaryBitwise(BinaryBitwise, Box<Expr<'input>>, Box<Expr<'input>>),
    /// Logical operations
    Logical(Logical, Box<Expr<'input>>, Box<Expr<'input>>),
    /// Equality checks
    Equality(Equality, Box<Expr<'input>>, Box<Expr<'input>>),
    /// Comparisons
    Comparison(Comparison, Box<Expr<'input>>, Box<Expr<'input>>),
    /// Arithmetic operations
    Arithmetic(Arithmetic, Box<Expr<'input>>, Box<Expr<'input>>),

    /// `!x`
    UnaryNot(Box<Expr<'input>>),
    /// `~x`
    UnaryBitwiseNot(Box<Expr<'input>>),
    /// `-x`
    UnaryMinus(Box<Expr<'input>>),
    /// `&x`
    UnaryAddressOf(Box<Expr<'input>>),
    /// `*x`
    UnaryDereference(Box<Expr<'input>>),

    /// `a[b]`
    Index(Box<Expr<'input>>, Box<Expr<'input>>),
    /// `a.b`
    Dot(Box<Expr<'input>>, Spanned<&'input str>),
    /// `a->b`
    Arrow(Box<Expr<'input>>, Spanned<&'input str>),
    /// `a(b, c, d, ...)`
    Call(Box<Expr<'input>>, Spanned<Vec<Expr<'input>>>),

    /// `a ? b : c`
    Ternary(Box<Expr<'input>>, Box<Expr<'input>>, Box<Expr<'input>>),

    /// `x as T`
    Cast(Box<Expr<'input>>, Type<'input>),
    /// `sizeof T`
    SizeOfType(Type<'input>),
    /// `sizeof(expr)`
    SizeOfExpr(Box<Expr<'input>>),

    /// Any numeric literal.
    NumberLiteral(NumberLiteral<'input>),
    /// Any string literal.
    StringLiteral(ZrcString<'input>),
    /// Any char literal
    CharLiteral(StringTok<'input>),
    /// Any identifier.
    Identifier(&'input str),
    /// Any boolean literal.
    BooleanLiteral(bool),
}
impl<'input> Display for ExprKind<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        match self {
            Self::NumberLiteral(n) => write!(f, "{n}"),
            Self::StringLiteral(str) => write!(f, "\"{str}\"",),
            Self::CharLiteral(str) => write!(f, "'{str}'",),
            Self::Identifier(i) => write!(f, "{i}"),
            Self::BooleanLiteral(value) => write!(f, "{value}"),
            Self::Assignment(operator, place, value) => write!(f, "{place} {operator} {value}"),
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
            Self::SizeOfType(ty) => write!(f, "sizeof {ty}"),
            Self::SizeOfExpr(expr) => write!(f, "sizeof({expr})"),
            Self::Dot(expr, key) => write!(f, "{expr}.{key}"),
            Self::Arrow(ptr, key) => write!(f, "{ptr}->{key}"),
            Self::Cast(expr, ty) => write!(f, "{expr} as {ty}"),
            Self::Call(expr, args) => write!(
                f,
                "{expr}({})",
                args.value()
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }?;
        write!(f, ")")
    }
}

// AST builder. We are able to infer the spans of many based on the start of
// their leftmost and the end of their rightmost operands.
#[allow(missing_docs)]
#[allow(clippy::missing_docs_in_private_items)]
#[allow(clippy::should_implement_trait)]
impl<'input> Expr<'input> {
    #[must_use]
    pub fn build_comma(lhs: Self, rhs: Self) -> Self {
        Self(spanned!(
            lhs.0.start(),
            ExprKind::Comma(Box::new(lhs), Box::new(rhs)),
            rhs.0.end()
        ))
    }

    fn build_assignment(assignment: Assignment, lhs: Self, rhs: Self) -> Self {
        Self(spanned!(
            lhs.0.start(),
            ExprKind::Assignment(assignment, Box::new(lhs), Box::new(rhs)),
            rhs.0.end()
        ))
    }

    #[must_use]
    pub fn build_assign(lhs: Self, rhs: Self) -> Self {
        Self::build_assignment(Assignment::Standard, lhs, rhs)
    }

    fn build_arithmetic_assignment(arithmetic: Arithmetic, lhs: Self, rhs: Self) -> Self {
        Self::build_assignment(Assignment::Arithmetic(arithmetic), lhs, rhs)
    }

    #[must_use]
    pub fn build_add_assign(lhs: Self, rhs: Self) -> Self {
        Self::build_arithmetic_assignment(Arithmetic::Addition, lhs, rhs)
    }
    #[must_use]
    pub fn build_sub_assign(lhs: Self, rhs: Self) -> Self {
        Self::build_arithmetic_assignment(Arithmetic::Subtraction, lhs, rhs)
    }
    #[must_use]
    pub fn build_mul_assign(lhs: Self, rhs: Self) -> Self {
        Self::build_arithmetic_assignment(Arithmetic::Multiplication, lhs, rhs)
    }
    #[must_use]
    pub fn build_div_assign(lhs: Self, rhs: Self) -> Self {
        Self::build_arithmetic_assignment(Arithmetic::Division, lhs, rhs)
    }
    #[must_use]
    pub fn build_mod_assign(lhs: Self, rhs: Self) -> Self {
        Self::build_arithmetic_assignment(Arithmetic::Modulo, lhs, rhs)
    }

    fn build_binary_bitwise(op: BinaryBitwise, lhs: Self, rhs: Self) -> Self {
        Self(spanned!(
            lhs.0.start(),
            ExprKind::BinaryBitwise(op, Box::new(lhs), Box::new(rhs)),
            rhs.0.end()
        ))
    }
    #[must_use]
    pub fn build_bit_and(lhs: Self, rhs: Self) -> Self {
        Self::build_binary_bitwise(BinaryBitwise::And, lhs, rhs)
    }
    #[must_use]
    pub fn build_bit_or(lhs: Self, rhs: Self) -> Self {
        Self::build_binary_bitwise(BinaryBitwise::Or, lhs, rhs)
    }
    #[must_use]
    pub fn build_bit_xor(lhs: Self, rhs: Self) -> Self {
        Self::build_binary_bitwise(BinaryBitwise::Xor, lhs, rhs)
    }
    #[must_use]
    pub fn build_shl(lhs: Self, rhs: Self) -> Self {
        Self::build_binary_bitwise(BinaryBitwise::Shl, lhs, rhs)
    }
    #[must_use]
    pub fn build_shr(lhs: Self, rhs: Self) -> Self {
        Self::build_binary_bitwise(BinaryBitwise::Shr, lhs, rhs)
    }

    fn build_logical(op: Logical, lhs: Self, rhs: Self) -> Self {
        Self(spanned!(
            lhs.0.start(),
            ExprKind::Logical(op, Box::new(lhs), Box::new(rhs)),
            rhs.0.end()
        ))
    }
    #[must_use]
    pub fn build_logical_and(lhs: Self, rhs: Self) -> Self {
        Self::build_logical(Logical::And, lhs, rhs)
    }
    #[must_use]
    pub fn build_logical_or(lhs: Self, rhs: Self) -> Self {
        Self::build_logical(Logical::Or, lhs, rhs)
    }

    fn build_equality(op: Equality, lhs: Self, rhs: Self) -> Self {
        Self(spanned!(
            lhs.0.start(),
            ExprKind::Equality(op, Box::new(lhs), Box::new(rhs)),
            rhs.0.end()
        ))
    }
    #[must_use]
    pub fn build_eq(lhs: Self, rhs: Self) -> Self {
        Self::build_equality(Equality::Eq, lhs, rhs)
    }
    #[must_use]
    pub fn build_neq(lhs: Self, rhs: Self) -> Self {
        Self::build_equality(Equality::Neq, lhs, rhs)
    }

    fn build_comparison(op: Comparison, lhs: Self, rhs: Self) -> Self {
        Self(spanned!(
            lhs.0.start(),
            ExprKind::Comparison(op, Box::new(lhs), Box::new(rhs)),
            rhs.0.end()
        ))
    }
    #[must_use]
    pub fn build_gt(lhs: Self, rhs: Self) -> Self {
        Self::build_comparison(Comparison::Gt, lhs, rhs)
    }
    #[must_use]
    pub fn build_gte(lhs: Self, rhs: Self) -> Self {
        Self::build_comparison(Comparison::Gte, lhs, rhs)
    }
    #[must_use]
    pub fn build_lt(lhs: Self, rhs: Self) -> Self {
        Self::build_comparison(Comparison::Lt, lhs, rhs)
    }
    #[must_use]
    pub fn build_lte(lhs: Self, rhs: Self) -> Self {
        Self::build_comparison(Comparison::Lte, lhs, rhs)
    }

    fn build_arithmetic(op: Arithmetic, lhs: Self, rhs: Self) -> Self {
        Self(spanned!(
            lhs.0.start(),
            ExprKind::Arithmetic(op, Box::new(lhs), Box::new(rhs)),
            rhs.0.end()
        ))
    }
    #[must_use]
    pub fn build_add(lhs: Self, rhs: Self) -> Self {
        Self::build_arithmetic(Arithmetic::Addition, lhs, rhs)
    }
    #[must_use]
    pub fn build_sub(lhs: Self, rhs: Self) -> Self {
        Self::build_arithmetic(Arithmetic::Subtraction, lhs, rhs)
    }
    #[must_use]
    pub fn build_mul(lhs: Self, rhs: Self) -> Self {
        Self::build_arithmetic(Arithmetic::Multiplication, lhs, rhs)
    }
    #[must_use]
    pub fn build_div(lhs: Self, rhs: Self) -> Self {
        Self::build_arithmetic(Arithmetic::Division, lhs, rhs)
    }
    #[must_use]
    pub fn build_modulo(lhs: Self, rhs: Self) -> Self {
        Self::build_arithmetic(Arithmetic::Modulo, lhs, rhs)
    }

    // Span needed as the unary op may be in a different position than the expr
    #[must_use]
    pub fn build_not(sp: Span, expr: Self) -> Self {
        Self(ExprKind::UnaryNot(Box::new(expr)).in_span(sp))
    }
    #[must_use]
    pub fn build_bit_not(sp: Span, expr: Self) -> Self {
        Self(ExprKind::UnaryBitwiseNot(Box::new(expr)).in_span(sp))
    }
    #[must_use]
    pub fn build_neg(sp: Span, expr: Self) -> Self {
        Self(ExprKind::UnaryMinus(Box::new(expr)).in_span(sp))
    }
    #[must_use]
    pub fn build_address_of(sp: Span, expr: Self) -> Self {
        Self(ExprKind::UnaryAddressOf(Box::new(expr)).in_span(sp))
    }
    #[must_use]
    pub fn build_deref(sp: Span, expr: Self) -> Self {
        Self(ExprKind::UnaryDereference(Box::new(expr)).in_span(sp))
    }

    /// Span is needed for right brace
    #[must_use]
    pub fn build_index(sp: Span, lhs: Self, rhs: Self) -> Self {
        Self(ExprKind::Index(Box::new(lhs), Box::new(rhs)).in_span(sp))
    }
    #[must_use]
    pub fn build_dot(expr: Self, prop: Spanned<&'input str>) -> Self {
        Self(spanned!(
            expr.0.start(),
            ExprKind::Dot(Box::new(expr), prop),
            prop.end()
        ))
    }
    #[must_use]
    pub fn build_arrow(expr: Self, prop: Spanned<&'input str>) -> Self {
        Self(spanned!(
            expr.0.start(),
            ExprKind::Arrow(Box::new(expr), prop),
            prop.end()
        ))
    }

    #[must_use]
    pub fn build_call(span: Span, f: Self, params: Spanned<Vec<Self>>) -> Self {
        Self(ExprKind::Call(Box::new(f), params).in_span(span))
    }
    #[must_use]
    pub fn build_ternary(cond: Self, if_true: Self, if_false: Self) -> Self {
        Self(spanned!(
            cond.0.start(),
            ExprKind::Ternary(Box::new(cond), Box::new(if_true), Box::new(if_false)),
            if_false.0.end()
        ))
    }
    #[must_use]
    pub fn build_cast(expr: Self, ty: Type<'input>) -> Self {
        Self(spanned!(
            expr.0.start(),
            ExprKind::Cast(Box::new(expr), ty),
            ty.0.end()
        ))
    }
    #[must_use]
    pub fn build_sizeof_type(span: Span, ty: Type<'input>) -> Self {
        Self(ExprKind::SizeOfType(ty).in_span(span))
    }
    #[must_use]
    pub fn build_sizeof_expr(span: Span, expr: Self) -> Self {
        Self(ExprKind::SizeOfExpr(Box::new(expr)).in_span(span))
    }

    // These all need spans because they can't be guessed
    #[must_use]
    pub fn build_number(lit: Spanned<NumberLiteral<'input>>) -> Self {
        let span = lit.span();
        Self(spanned!(
            span.start(),
            ExprKind::NumberLiteral(lit.into_value()),
            span.end()
        ))
    }
    #[must_use]
    pub fn build_number_dec(lit: Spanned<&'input str>) -> Self {
        Self::build_number(lit.map(NumberLiteral::Decimal))
    }
    #[must_use]
    pub fn build_number_hex(lit: Spanned<&'input str>) -> Self {
        Self::build_number(lit.map(NumberLiteral::Hexadecimal))
    }
    #[must_use]
    pub fn build_number_bin(lit: Spanned<&'input str>) -> Self {
        Self::build_number(lit.map(NumberLiteral::Binary))
    }
    #[must_use]
    pub fn build_string(lit: Spanned<ZrcString<'input>>) -> Self {
        let span = lit.span();
        Self(spanned!(
            span.start(),
            ExprKind::StringLiteral(lit.into_value()),
            span.end()
        ))
    }
    #[must_use]
    pub fn build_char(lit: Spanned<StringTok<'input>>) -> Self {
        let span = lit.span();
        Self(spanned!(
            span.start(),
            ExprKind::CharLiteral(lit.into_value()),
            span.end()
        ))
    }
    #[must_use]
    pub fn build_ident(lit: Spanned<&'input str>) -> Self {
        let span = lit.span();
        Self(spanned!(
            span.start(),
            ExprKind::Identifier(lit.into_value()),
            span.end()
        ))
    }
    #[must_use]
    pub fn build_bool(lit: Spanned<bool>) -> Self {
        let span = lit.span();
        Self(spanned!(
            span.start(),
            ExprKind::BooleanLiteral(lit.into_value()),
            span.end()
        ))
    }
}
