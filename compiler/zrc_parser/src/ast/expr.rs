//! Expression representation for the Zirco AST
//!
//! The main thing within this module you will need is the [`Expr`] struct.

use std::fmt::Display;

use zrc_utils::span::Spanned;

/// Arithmetic operators
///
/// For an operator to be considered an arithmetic operator, it must meet the
/// following criteria:
/// - Operates on two integers
/// - Both operands must be the same type
/// - The result type is the same as the operand types
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
pub struct Expr(pub Spanned<ExprKind>);

impl Display for Expr {
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
pub enum ExprKind {
    /// `a, b`
    Comma(Box<Expr>, Box<Expr>),

    /// Assignment operations
    Assignment(Assignment, Box<Expr>, Box<Expr>),
    /// Bitwise operations
    BinaryBitwise(BinaryBitwise, Box<Expr>, Box<Expr>),
    /// Logical operations
    Logical(Logical, Box<Expr>, Box<Expr>),
    /// Equality checks
    Equality(Equality, Box<Expr>, Box<Expr>),
    /// Comparisons
    Comparison(Comparison, Box<Expr>, Box<Expr>),
    /// Arithmetic operations
    Arithmetic(Arithmetic, Box<Expr>, Box<Expr>),

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
    Dot(Box<Expr>, Spanned<String>),
    /// `a->b`
    Arrow(Box<Expr>, Spanned<String>),
    /// `a(b, c, d, ...)`
    Call(Box<Expr>, Spanned<Vec<Expr>>),

    /// `a ? b : c`
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),

    /// `x as T`
    Cast(Box<Expr>, super::ty::Type),

    /// Any numeric literal.
    NumberLiteral(String),
    /// Any string literal.
    StringLiteral(String),
    /// Any identifier.
    Identifier(String),
    /// Any boolean literal.
    BooleanLiteral(bool),
}
impl Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        match self {
            Self::NumberLiteral(n) => write!(f, "{n}"),
            Self::StringLiteral(s) => write!(f, "{s}"),
            Self::Identifier(i) => write!(f, "{i}"),
            Self::BooleanLiteral(b) => write!(f, "{b}"),
            Self::Assignment(op, l, r) => write!(f, "{l} {op} {r}"),
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
            Self::Arrow(l, r) => write!(f, "{l}->{}", r.value()),
            Self::Ternary(l, m, r) => write!(f, "{l} ? {m} : {r}"),
            Self::Index(a, b) => write!(f, "{a}[{b}]"),
            Self::Dot(a, b) => write!(f, "{a}.{}", b.value()),
            Self::Cast(a, t) => write!(f, "{a} as {t}"),
            Self::Call(a, b) => write!(
                f,
                "{}({})",
                a,
                b.value()
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }?;
        write!(f, ")")?;
        Ok(())
    }
}
