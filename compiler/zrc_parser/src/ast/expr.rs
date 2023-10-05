//! Expression representation for the Zirco AST
//!
//! The main thing within this module you will need is the [`Expr`] enum. It
//! contains all the different expression kinds in Zirco.

use std::fmt::Display;

/// Arithmetic operators
///
/// For an operator to fall under this enum, it must operate on two integers
/// of the same type and yield that type, and performs some mathematical
/// operation.
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
/// This enum defines all of the different types of assignments which may use
/// another operation under the hood.
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Assignment {
    /// `=`
    Standard,
    /// `+=` etc.
    Arithmetic(Arithmetic),
    /// `|=` etc.
    BinaryBitwise(BinaryBitwise),
}

impl Display for Assignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Standard => write!(f, "="),
            Self::Arithmetic(Arithmetic::Addition) => write!(f, "+="),
            Self::Arithmetic(Arithmetic::Subtraction) => write!(f, "-="),
            Self::Arithmetic(Arithmetic::Multiplication) => write!(f, "*="),
            Self::Arithmetic(Arithmetic::Division) => write!(f, "/="),
            Self::Arithmetic(Arithmetic::Modulo) => write!(f, "%="),
            Self::BinaryBitwise(BinaryBitwise::And) => write!(f, "&="),
            Self::BinaryBitwise(BinaryBitwise::Or) => write!(f, "|="),
            Self::BinaryBitwise(BinaryBitwise::Xor) => write!(f, "^="),
            Self::BinaryBitwise(BinaryBitwise::Shl) => write!(f, "<<="),
            Self::BinaryBitwise(BinaryBitwise::Shr) => write!(f, ">>="),
        }
    }
}

/// Binary bitwise operators
///
/// For an operator to fall under this enum, it must operate on two integers
/// of the same type and yield that type, and performs some bitwise operation.
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
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Logical {
    /// `&&`
    And,
    /// `||`
    Or,
}

/// Equality checks
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Equality {
    /// `==`
    Eq,
    /// `!=`
    Neq,
}

/// Comparison checks
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

impl Display for Logical {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::And => write!(f, "&&"),
            Self::Or => write!(f, "||"),
        }
    }
}
impl Display for Equality {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eq => write!(f, "=="),
            Self::Neq => write!(f, "!="),
        }
    }
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

/// The enum representing the different kinds of expressions in Zirco
///
/// This enum represents all the different kinds of expressions in Zirco. It is
/// used by the parser to represent the AST in the expression position.
#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    /// `a, b`
    Comma(Box<Expr>, Box<Expr>),

    Assignment(Assignment, Box<Expr>, Box<Expr>),

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

    BinaryBitwise(BinaryBitwise, Box<Expr>, Box<Expr>),

    Logical(Logical, Box<Expr>, Box<Expr>),
    Equality(Equality, Box<Expr>, Box<Expr>),
    Comparison(Comparison, Box<Expr>, Box<Expr>),
    Arithmetic(Arithmetic, Box<Expr>, Box<Expr>),

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
            Self::NumberLiteral(n) => write!(f, "{n}"),
            Self::StringLiteral(s) => write!(f, "{s}"),
            Self::Identifier(i) => write!(f, "{i}"),
            Self::BooleanLiteral(b) => write!(f, "{b}"),
            Self::Error => write!(f, "error"),
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
            Self::Arrow(l, r) => write!(f, "{l}->{r}"),
            Self::Ternary(l, m, r) => write!(f, "{l} ? {m} : {r}"),
            Self::Index(a, b) => write!(f, "{a}[{b}]"),
            Self::Dot(a, b) => write!(f, "{a}.{b}"),
            Self::Call(a, b) => write!(
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
