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
#[derive(PartialEq, Eq, Debug, Clone, Display)]
#[display("{_0}")]
pub struct Expr<'input>(pub Spanned<ExprKind<'input>>);

/// The enum representing the different kinds of expressions in Zirco
///
/// This enum represents all the different kinds of expressions in Zirco. It is
/// used by the parser to represent the AST in the expression position.
#[derive(PartialEq, Eq, Debug, Clone, Display)]
#[display("({_variant})")]
pub enum ExprKind<'input> {
    /// `a, b`
    #[display("{_0}, {_1}")]
    Comma(Box<Expr<'input>>, Box<Expr<'input>>),

    /// Assignment operations
    #[display("{_1} {_0} {_2}")]
    Assignment(Assignment, Box<Expr<'input>>, Box<Expr<'input>>),
    /// Bitwise operations
    #[display("{_1} {_0} {_2}")]
    BinaryBitwise(BinaryBitwise, Box<Expr<'input>>, Box<Expr<'input>>),
    /// Logical operations
    #[display("{_1} {_0} {_2}")]
    Logical(Logical, Box<Expr<'input>>, Box<Expr<'input>>),
    /// Equality checks
    #[display("{_1} {_0} {_2}")]
    Equality(Equality, Box<Expr<'input>>, Box<Expr<'input>>),
    /// Comparisons
    #[display("{_1} {_0} {_2}")]
    Comparison(Comparison, Box<Expr<'input>>, Box<Expr<'input>>),
    /// Arithmetic operations
    #[display("{_1} {_0} {_2}")]
    Arithmetic(Arithmetic, Box<Expr<'input>>, Box<Expr<'input>>),

    /// `!x`
    #[display("!{_0}")]
    UnaryNot(Box<Expr<'input>>),
    /// `~x`
    #[display("~{_0}")]
    UnaryBitwiseNot(Box<Expr<'input>>),
    /// `-x`
    #[display("-{_0}")]
    UnaryMinus(Box<Expr<'input>>),
    /// `&x`
    #[display("&{_0}")]
    UnaryAddressOf(Box<Expr<'input>>),
    /// `*x`
    #[display("*{_0}")]
    UnaryDereference(Box<Expr<'input>>),

    /// `a[b]`
    #[display("{_0}[{_1}]")]
    Index(Box<Expr<'input>>, Box<Expr<'input>>),
    /// `a.b`
    #[display("{_0}.{_1}")]
    Dot(Box<Expr<'input>>, Spanned<&'input str>),
    /// `a->b`
    #[display("{_0}->{_1}")]
    Arrow(Box<Expr<'input>>, Spanned<&'input str>),
    /// `a(b, c, d, ...)`
    #[display(
        "{_0}({})",
        _1.value()
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<String>>()
            .join(", "))]
    Call(Box<Expr<'input>>, Spanned<Vec<Expr<'input>>>),

    /// `a ? b : c`
    #[display("{_0} ? {_1} : {_2}")]
    Ternary(Box<Expr<'input>>, Box<Expr<'input>>, Box<Expr<'input>>),

    /// `x as T`
    #[display("{_0} as {_1}")]
    Cast(Box<Expr<'input>>, Type<'input>),
    /// `sizeof T`
    #[display("sizeof {_0}")]
    SizeOfType(Type<'input>),
    /// `sizeof(expr)`
    #[display("sizeof({_0})")]
    SizeOfExpr(Box<Expr<'input>>),

    /// Any numeric literal.
    #[display("{_0}{}", _1.as_ref().map_or_else(String::new, Type::to_string))]
    NumberLiteral(NumberLiteral<'input>, Option<Type<'input>>),
    /// Any string literal.
    #[display("\"{_0}\"")]
    StringLiteral(ZrcString<'input>),
    /// Any char literal
    #[display("'{_0}'")]
    CharLiteral(StringTok<'input>),
    /// Any identifier.
    #[display("{_0}")]
    Identifier(&'input str),
    /// Any boolean literal.
    #[display("{_0}")]
    BooleanLiteral(bool),
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
        let file_name = expr.0.file_name();
        Self(ExprKind::UnaryNot(Box::new(expr)).in_span(sp, file_name))
    }
    #[must_use]
    pub fn build_bit_not(sp: Span, expr: Self) -> Self {
        let file_name = expr.0.file_name();
        Self(ExprKind::UnaryBitwiseNot(Box::new(expr)).in_span(sp, file_name))
    }
    #[must_use]
    pub fn build_neg(sp: Span, expr: Self) -> Self {
        let file_name = expr.0.file_name();
        Self(ExprKind::UnaryMinus(Box::new(expr)).in_span(sp, file_name))
    }
    #[must_use]
    pub fn build_address_of(sp: Span, expr: Self) -> Self {
        let file_name = expr.0.file_name();
        Self(ExprKind::UnaryAddressOf(Box::new(expr)).in_span(sp, file_name))
    }
    #[must_use]
    pub fn build_deref(sp: Span, expr: Self) -> Self {
        let file_name = expr.0.file_name();
        Self(ExprKind::UnaryDereference(Box::new(expr)).in_span(sp, file_name))
    }

    /// Span is needed for right brace
    #[must_use]
    pub fn build_index(sp: Span, lhs: Self, rhs: Self) -> Self {
        let file_name = lhs.0.file_name();
        Self(ExprKind::Index(Box::new(lhs), Box::new(rhs)).in_span(sp, file_name))
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
        let file_name = f.0.file_name();
        Self(ExprKind::Call(Box::new(f), params).in_span(span, file_name))
    }
    #[must_use]
    pub fn build_ternary(cond: Self, if_true: Self, if_false: Self) -> Self {
        Self(spanned!(
            cond.0.start(),
            ExprKind::Ternary(Box::new(cond), Box::new(if_true), Box::new(if_false)),
            if_false.0.end(),
            cond.0.file_name()
        ))
    }
    #[must_use]
    pub fn build_cast(expr: Self, ty: Type<'input>) -> Self {
        Self(spanned!(
            expr.0.start(),
            ExprKind::Cast(Box::new(expr), ty),
            ty.0.end(),
            expr.0.file_name()
        ))
    }
    #[must_use]
    pub fn build_sizeof_type(span: Span, ty: Type<'input>) -> Self {
        let file_name = ty.0.file_name();
        Self(ExprKind::SizeOfType(ty).in_span(span, file_name))
    }
    #[must_use]
    pub fn build_sizeof_expr(span: Span, expr: Self) -> Self {
        let file_name = expr.0.file_name();
        Self(ExprKind::SizeOfExpr(Box::new(expr)).in_span(span, file_name))
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
        let file_name = lit.file_name();
        Self(spanned!(
            span.start(),
            ExprKind::StringLiteral(lit.into_value()),
            span.end(),
            file_name
        ))
    }
    #[must_use]
    pub fn build_char(lit: Spanned<StringTok<'input>>) -> Self {
        let span = lit.span();
        let file_name = lit.file_name();
        Self(spanned!(
            span.start(),
            ExprKind::CharLiteral(lit.into_value()),
            span.end(),
            file_name
        ))
    }
    #[must_use]
    pub fn build_ident(lit: Spanned<&'input str>) -> Self {
        let span = lit.span();
        let file_name = lit.file_name();
        Self(spanned!(
            span.start(),
            ExprKind::Identifier(lit.into_value()),
            span.end(),
            file_name
        ))
    }
    #[must_use]
    pub fn build_bool(lit: Spanned<bool>) -> Self {
        let span = lit.span();
        let file_name = lit.file_name();
        Self(spanned!(
            span.start(),
            ExprKind::BooleanLiteral(lit.into_value()),
            span.end(),
            file_name
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
            "((1), (2))",
            "((a) = (b))",
            "((a) += (b))",
            "((a) -= (b))",
            "((a) *= (b))",
            "((a) /= (b))",
            "((a) %= (b))",
            "((a) &= (b))",
            "((a) |= (b))",
            "((a) ^= (b))",
            "((a) <<= (b))",
            "((a) >>= (b))",
            "((a) << (b))",
            "((a) >> (b))",
            "((a) && (b))",
            "((a) || (b))",
            "((a) == (b))",
            "((a) != (b))",
            "((a) > (b))",
            "((a) >= (b))",
            "((a) < (b))",
            "((a) <= (b))",
            "((a) + (b))",
            "((a) - (b))",
            "((a) * (b))",
            "((a) / (b))",
            "((a) % (b))",
            "(!(a))",
            "(~(a))",
            "(-(a))",
            "(&(a))",
            "(*(a))",
            "((a)[(b)])",
            "((a).b)",
            "((a)->b)",
            "((a)((b), (c)))",
            "((a) ? (b) : (c))",
            "((a) as T)",
            "(sizeof T)",
            "(sizeof((expr)))",
            "(1)",
            "(\"a\")",
            "('a')",
            "(a)",
            "(true)",
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
}
