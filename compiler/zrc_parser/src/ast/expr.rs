use std::fmt::Display;

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    Comma(Box<Expr>, Box<Expr>),

    Assignment(Box<Expr>, Box<Expr>),
    AdditionAssignment(Box<Expr>, Box<Expr>),
    SubtractionAssignment(Box<Expr>, Box<Expr>),
    MultiplicationAssignment(Box<Expr>, Box<Expr>),
    DivisionAssignment(Box<Expr>, Box<Expr>),
    ModuloAssignment(Box<Expr>, Box<Expr>),
    BitwiseAndAssignment(Box<Expr>, Box<Expr>),
    BitwiseOrAssignment(Box<Expr>, Box<Expr>),
    BitwiseXorAssignment(Box<Expr>, Box<Expr>),
    BitwiseLeftShiftAssignment(Box<Expr>, Box<Expr>),
    BitwiseRightShiftAssignment(Box<Expr>, Box<Expr>),

    UnaryNot(Box<Expr>),
    UnaryBitwiseNot(Box<Expr>),
    UnaryMinus(Box<Expr>),
    UnaryAddressOf(Box<Expr>),
    UnaryDereference(Box<Expr>),

    Index(Box<Expr>, Box<Expr>),
    Dot(Box<Expr>, String),
    Arrow(Box<Expr>, String),
    Call(Box<Expr>, Vec<Expr>),

    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),

    LogicalAnd(Box<Expr>, Box<Expr>),
    LogicalOr(Box<Expr>, Box<Expr>),

    Equals(Box<Expr>, Box<Expr>),
    NotEquals(Box<Expr>, Box<Expr>),

    BitwiseAnd(Box<Expr>, Box<Expr>),
    BitwiseOr(Box<Expr>, Box<Expr>),
    BitwiseXor(Box<Expr>, Box<Expr>),

    GreaterThan(Box<Expr>, Box<Expr>),
    GreaterThanOrEqualTo(Box<Expr>, Box<Expr>),
    LessThan(Box<Expr>, Box<Expr>),
    LessThanOrEqualTo(Box<Expr>, Box<Expr>),

    BitwiseRightShift(Box<Expr>, Box<Expr>),
    BitwiseLeftShift(Box<Expr>, Box<Expr>),

    Addition(Box<Expr>, Box<Expr>),
    Subtraction(Box<Expr>, Box<Expr>),

    Multiplication(Box<Expr>, Box<Expr>),
    Division(Box<Expr>, Box<Expr>),
    Modulo(Box<Expr>, Box<Expr>),

    Cast(Box<Expr>, super::ty::Type),

    NumberLiteral(String),
    StringLiteral(String),
    Identifier(String),
    BooleanLiteral(bool),

    Error,
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        match self {
            Expr::NumberLiteral(n) => write!(f, "{}", n),
            Expr::StringLiteral(s) => write!(f, "{}", s),
            Expr::Identifier(i) => write!(f, "{}", i),
            Expr::BooleanLiteral(b) => write!(f, "{}", b),
            Expr::Error => write!(f, "error"),
            Expr::Assignment(l, r) => write!(f, "{} = {}", l, r),
            Expr::Addition(l, r) => write!(f, "{} + {}", l, r),
            Expr::Subtraction(l, r) => write!(f, "{} - {}", l, r),
            Expr::Multiplication(l, r) => write!(f, "{} * {}", l, r),
            Expr::Division(l, r) => write!(f, "{} / {}", l, r),
            Expr::Modulo(l, r) => write!(f, "{} % {}", l, r),
            Expr::BitwiseAnd(l, r) => write!(f, "{} & {}", l, r),
            Expr::BitwiseOr(l, r) => write!(f, "{} | {}", l, r),
            Expr::BitwiseXor(l, r) => write!(f, "{} ^ {}", l, r),
            Expr::BitwiseLeftShift(l, r) => write!(f, "{} << {}", l, r),
            Expr::BitwiseRightShift(l, r) => write!(f, "{} >> {}", l, r),
            Expr::GreaterThan(l, r) => write!(f, "{} > {}", l, r),
            Expr::GreaterThanOrEqualTo(l, r) => write!(f, "{} >= {}", l, r),
            Expr::LessThan(l, r) => write!(f, "{} < {}", l, r),
            Expr::LessThanOrEqualTo(l, r) => write!(f, "{} <= {}", l, r),
            Expr::Equals(l, r) => write!(f, "{} == {}", l, r),
            Expr::Cast(x, t) => write!(f, "{x} as {t}"),
            Expr::NotEquals(l, r) => write!(f, "{} != {}", l, r),
            Expr::LogicalAnd(l, r) => write!(f, "{} && {}", l, r),
            Expr::LogicalOr(l, r) => write!(f, "{} || {}", l, r),
            Expr::Comma(l, r) => write!(f, "{}, {}", l, r),
            Expr::AdditionAssignment(l, r) => write!(f, "{} += {}", l, r),
            Expr::SubtractionAssignment(l, r) => write!(f, "{} -= {}", l, r),
            Expr::MultiplicationAssignment(l, r) => write!(f, "{} *= {}", l, r),
            Expr::DivisionAssignment(l, r) => write!(f, "{} /= {}", l, r),
            Expr::ModuloAssignment(l, r) => write!(f, "{} %= {}", l, r),
            Expr::BitwiseAndAssignment(l, r) => write!(f, "{} &= {}", l, r),
            Expr::BitwiseOrAssignment(l, r) => write!(f, "{} |= {}", l, r),
            Expr::BitwiseXorAssignment(l, r) => write!(f, "{} ^= {}", l, r),
            Expr::BitwiseLeftShiftAssignment(l, r) => write!(f, "{} <<= {}", l, r),
            Expr::BitwiseRightShiftAssignment(l, r) => write!(f, "{} >>= {}", l, r),
            Expr::UnaryNot(e) => write!(f, "!{}", e),
            Expr::UnaryBitwiseNot(e) => write!(f, "~{}", e),
            Expr::UnaryMinus(e) => write!(f, "-{}", e),
            Expr::UnaryAddressOf(e) => write!(f, "&{}", e),
            Expr::UnaryDereference(e) => write!(f, "*{}", e),
            Expr::Arrow(l, r) => write!(f, "{}->{}", l, r),
            Expr::Ternary(l, m, r) => write!(f, "{} ? {} : {}", l, m, r),
            Expr::Index(a, b) => write!(f, "{}[{}]", a, b),
            Expr::Dot(a, b) => write!(f, "{}.{}", a, b),
            Expr::Call(a, b) => write!(
                f,
                "{}({})",
                a,
                b.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }?;
        write!(f, ")")?;
        Ok(())
    }
}
