//! Expression representation for the Zirco AST
//!
//! The main thing within this module you will need is the [`Expr`] struct.

use derive_more::Display;
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
#[derive(PartialEq, Eq, Debug, Clone, Display)]
pub enum Arithmetic {
    /// `+`
    #[display("+")]
    Addition,
    /// `-`
    #[display("-")]
    Subtraction,
    /// `*`
    #[display("*")]
    Multiplication,
    /// `/`
    #[display("/")]
    Division,
    /// `%`
    #[display("%")]
    Modulo,
}

/// Assignment operators
///
/// All possible forms of assignments with operational variations.
#[derive(PartialEq, Eq, Debug, Clone, Display)]
pub enum Assignment {
    /// `=`
    #[display("=")]
    Standard,
    /// Any form of assignment via [`Arithmetic`] operator (e.g. `+=`)
    #[display("{_0}=")]
    Arithmetic(Arithmetic),
    /// Any form of assignment via [`BinaryBitwise`] operator (e.g. `&=`)
    #[display("{_0}=")]
    BinaryBitwise(BinaryBitwise),
}

/// Binary bitwise operators
///
/// For an operator to be considered a binary bitwise operator, it must meet the
/// following criteria:
/// - Operates on two integers
/// - Both operands must be the same type
/// - The result type is the same as the operand types
/// - Performs some bitwise operation
#[derive(PartialEq, Eq, Debug, Clone, Display)]
pub enum BinaryBitwise {
    /// `&`
    #[display("&")]
    And,
    /// `|`
    #[display("|")]
    Or,
    /// `^`
    #[display("^")]
    Xor,
    /// `<<`
    #[display("<<")]
    Shl,
    /// `>>`
    #[display(">>")]
    Shr,
}

/// Logical operators
///
/// For an operand to be considered a logical operator, it must meet the
/// following criteria:
/// - Operates on two booleans
/// - The result type is a boolean
/// - Performs some logical operation
#[derive(PartialEq, Eq, Debug, Clone, Display)]
pub enum Logical {
    /// `&&`
    #[display("&&")]
    And,
    /// `||`
    #[display("||")]
    Or,
}

/// Equality checks
///
/// For an operand to be considered an equality check, it must meet the
/// following criteria:
/// - Operates on two values of the same type
/// - The result type is a boolean
/// - Performs some equality or inequality check
#[derive(PartialEq, Eq, Debug, Clone, Display)]
pub enum Equality {
    /// `==`
    #[display("==")]
    Eq,
    /// `!=`
    #[display("!=")]
    Neq,
}

/// Comparison checks
///
/// For an operand to be considered a comparison check, it must meet the
/// following criteria:
/// - Operates on two integers
/// - Both operands must be the same type
/// - The result type is a boolean
/// - Performs some comparison or order check
#[derive(PartialEq, Eq, Debug, Clone, Display)]
pub enum Comparison {
    /// `>`
    #[display(">")]
    Gt,
    /// `>=`
    #[display(">=")]
    Gte,
    /// `<`
    #[display("<")]
    Lt,
    /// `<=`
    #[display("<=")]
    Lte,
}

/// A Zirco expression
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Expr<'input>(pub Spanned<ExprKind<'input>>);

/// Precedence level for expressions. Higher values bind more tightly.
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
    /// Primary expressions (literals, identifiers, parenthesized)
    Primary = 17,
}

/// The enum representing the different kinds of expressions in Zirco
///
/// This enum represents all the different kinds of expressions in Zirco. It is
/// used by the parser to represent the AST in the expression position.
#[derive(PartialEq, Eq, Debug, Clone)]
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

    /// Struct construction: `new Type { field1: value1, field2: value2 }`
    #[allow(clippy::type_complexity)]
    StructConstruction(
        Type<'input>,
        Spanned<Vec<Spanned<(Spanned<&'input str>, Expr<'input>)>>>,
    ),

    /// Any numeric literal.
    NumberLiteral(NumberLiteral<'input>, Option<Type<'input>>),
    /// Any string literal.
    StringLiteral(ZrcString<'input>),
    /// Any char literal
    CharLiteral(StringTok<'input>),
    /// Any identifier.
    Identifier(&'input str),
    /// Any boolean literal.
    BooleanLiteral(bool),
}

impl ExprKind<'_> {
    /// Get the precedence level of this expression kind
    const fn precedence(&self) -> Precedence {
        match self {
            Self::Comma(_, _) => Precedence::Comma,
            Self::Assignment(_, _, _) => Precedence::Assignment,
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
            | Self::SizeOfType(_)
            | Self::SizeOfExpr(_) => Precedence::Unary,
            Self::Index(_, _) | Self::Dot(_, _) | Self::Arrow(_, _) | Self::Call(_, _) => {
                Precedence::Postfix
            }
            Self::NumberLiteral(_, _)
            | Self::StringLiteral(_)
            | Self::CharLiteral(_)
            | Self::Identifier(_)
            | Self::BooleanLiteral(_)
            | Self::StructConstruction(_, _) => Precedence::Primary,
        }
    }

    /// Format a child expression with parentheses if needed based on
    /// precedence. For left children of left-associative operators, we need
    /// parens if `child_prec` < `parent_prec`. For right children of
    /// left-associative operators, we need parens if `child_prec` <=
    /// `parent_prec`.
    fn fmt_child(
        f: &mut std::fmt::Formatter<'_>,
        child: &Expr<'_>,
        parent_prec: Precedence,
        is_right: bool,
    ) -> std::fmt::Result {
        let child_prec = child.0.value().precedence();
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

impl std::fmt::Display for ExprKind<'_> {
    #[allow(clippy::too_many_lines)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Comma(lhs, rhs) => {
                let prec = self.precedence();
                Self::fmt_child(f, lhs, prec, false)?;
                write!(f, ", ")?;
                Self::fmt_child(f, rhs, prec, true)
            }
            Self::Assignment(op, lhs, rhs) => {
                let prec = self.precedence();
                Self::fmt_child(f, lhs, prec, false)?;
                write!(f, " {op} ")?;
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
                // Unary operators need parens on their operand if it's lower precedence
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
            Self::UnaryAddressOf(expr) => {
                write!(f, "&")?;
                Self::fmt_child(f, expr, Precedence::Unary, true)
            }
            Self::UnaryDereference(expr) => {
                write!(f, "*")?;
                Self::fmt_child(f, expr, Precedence::Unary, true)
            }
            Self::Index(lhs, rhs) => {
                Self::fmt_child(f, lhs, Precedence::Postfix, false)?;
                write!(f, "[")?;
                // Inside brackets, we can use any expression without parens (like in function
                // calls)
                write!(f, "{rhs}")?;
                write!(f, "]")
            }
            Self::Dot(expr, field) => {
                Self::fmt_child(f, expr, Precedence::Postfix, false)?;
                write!(f, ".{}", field.value())
            }
            Self::Arrow(expr, field) => {
                Self::fmt_child(f, expr, Precedence::Postfix, false)?;
                write!(f, "->{}", field.value())
            }
            Self::Call(expr, args) => {
                Self::fmt_child(f, expr, Precedence::Postfix, false)?;
                write!(
                    f,
                    "({})",
                    args.value()
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Self::Ternary(cond, if_true, if_false) => {
                let prec = self.precedence();
                Self::fmt_child(f, cond, prec, false)?;
                write!(f, " ? ")?;
                // Middle expression in ternary can be any expression (like comma)
                write!(f, "{if_true}")?;
                write!(f, " : ")?;
                Self::fmt_child(f, if_false, prec, true)
            }
            Self::Cast(expr, ty) => {
                let prec = self.precedence();
                Self::fmt_child(f, expr, prec, false)?;
                write!(f, " as {ty}")
            }
            Self::SizeOfType(ty) => write!(f, "sizeof {ty}"),
            Self::SizeOfExpr(expr) => write!(f, "sizeof({expr})"),
            Self::StructConstruction(ty, fields) => {
                write!(f, "new {ty} {{ ")?;
                let field_list: Vec<String> = fields
                    .value()
                    .iter()
                    .map(|field| {
                        let (name, expr) = field.value();
                        format!("{}: ({})", name.value(), expr)
                    })
                    .collect();
                write!(f, "{}", field_list.join(", "))?;
                write!(f, " }}")
            }
            Self::NumberLiteral(num, ty) => {
                write!(
                    f,
                    "{num}{}",
                    ty.as_ref().map_or(String::new(), ToString::to_string)
                )
            }
            Self::StringLiteral(string) => write!(f, "\"{string}\""),
            Self::CharLiteral(character) => write!(f, "'{character}'"),
            Self::Identifier(name) => write!(f, "{name}"),
            Self::BooleanLiteral(boolean) => write!(f, "{boolean}"),
        }
    }
}

impl std::fmt::Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.value())
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
    pub fn build_number(
        lit: Spanned<NumberLiteral<'input>>,
        ty: Option<Spanned<Type<'input>>>,
    ) -> Self {
        let start = lit.start();
        let end = ty.as_ref().map_or_else(|| lit.end(), Spanned::end);
        Self(spanned!(
            start,
            ExprKind::NumberLiteral(lit.into_value(), ty.map(Spanned::into_value)),
            end
        ))
    }
    #[must_use]
    pub fn build_number_dec(lit: Spanned<&'input str>, ty: Option<Spanned<Type<'input>>>) -> Self {
        Self::build_number(lit.map(NumberLiteral::Decimal), ty)
    }
    #[must_use]
    pub fn build_number_hex(lit: Spanned<&'input str>, ty: Option<Spanned<Type<'input>>>) -> Self {
        Self::build_number(lit.map(NumberLiteral::Hexadecimal), ty)
    }
    #[must_use]
    pub fn build_number_bin(lit: Spanned<&'input str>, ty: Option<Spanned<Type<'input>>>) -> Self {
        Self::build_number(lit.map(NumberLiteral::Binary), ty)
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
    #[must_use]
    #[allow(clippy::type_complexity)]
    pub fn build_struct_construction(
        ty: Type<'input>,
        fields: Spanned<Vec<Spanned<(Spanned<&'input str>, Self)>>>,
    ) -> Self {
        let start = ty.0.start();
        let end = fields.end();
        Self(spanned!(
            start,
            ExprKind::StructConstruction(ty, fields),
            end
        ))
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn expressions_stringify_to_their_canonical_form() {
        // A list of sample expressions in "canonical form."
        // These are parsed then stringified again, and tested for equality.

        let test_cases = vec![
            "1, 2",
            "a = b",
            "a += b",
            "a -= b",
            "a *= b",
            "a /= b",
            "a %= b",
            "a &= b",
            "a |= b",
            "a ^= b",
            "a <<= b",
            "a >>= b",
            "a << b",
            "a >> b",
            "a && b",
            "a || b",
            "a == b",
            "a != b",
            "a > b",
            "a >= b",
            "a < b",
            "a <= b",
            "a + b",
            "a - b",
            "a * b",
            "a / b",
            "a % b",
            "!a",
            "~a",
            "-a",
            "&a",
            "*a",
            "a[b]",
            "a.b",
            "a->b",
            "a(b, c)",
            "a ? b : c",
            "a as T",
            "sizeof T",
            "sizeof(expr)",
            "1",
            "\"a\"",
            "'a'",
            "a",
            "true",
        ];

        for input in test_cases {
            assert_eq!(
                crate::parser::parse_expr(input)
                    .expect("test cases should have parsed correctly")
                    .to_string(),
                input
            );
        }
    }

    #[test]
    fn parentheses_are_added_when_needed_for_precedence() {
        // Test cases where parentheses ARE needed to preserve precedence
        let test_cases = vec![
            // Multiplication binds tighter than addition
            ("a + b * c", "a + b * c"),
            ("(a + b) * c", "(a + b) * c"),
            ("a * (b + c)", "a * (b + c)"),
            // Comparison binds tighter than logical
            ("a < b && c > d", "a < b && c > d"),
            ("(a < b) && (c > d)", "a < b && c > d"), // Extra parens removed
            // Logical AND binds tighter than OR
            ("a || b && c", "a || b && c"),
            ("(a || b) && c", "(a || b) && c"),
            // Assignment has low precedence
            ("a = b + c", "a = b + c"),
            ("(a = b) + c", "(a = b) + c"),
            // Ternary precedence
            ("a = b ? c : d", "a = b ? c : d"),
            // Multiple operators of same precedence (left associative)
            ("a - b - c", "a - b - c"),
            ("a - (b - c)", "a - (b - c)"),
            // Unary operators
            ("!a && b", "!a && b"),
            ("!(a && b)", "!(a && b)"),
            // Postfix operators have highest precedence
            ("a.b + c", "a.b + c"),
            ("a + b.c", "a + b.c"),
        ];

        for (input, expected) in test_cases {
            let result = crate::parser::parse_expr(input)
                .expect("test cases should have parsed correctly")
                .to_string();
            assert_eq!(
                result, expected,
                "Input: {input} should stringify to: {expected}, but got: {result}"
            );
        }
    }
}
