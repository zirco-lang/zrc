use std::fmt::Display;

#[derive(PartialEq, Debug, Clone)]
pub struct TypedExpr(pub super::ty::Type, pub TypedExprKind);

#[derive(PartialEq, Debug, Clone)]
pub enum TypedExprKind {
    Comma(Box<TypedExpr>, Box<TypedExpr>),

    Assignment(Box<TypedExpr>, Box<TypedExpr>),
    AdditionAssignment(Box<TypedExpr>, Box<TypedExpr>),
    SubtractionAssignment(Box<TypedExpr>, Box<TypedExpr>),
    MultiplicationAssignment(Box<TypedExpr>, Box<TypedExpr>),
    DivisionAssignment(Box<TypedExpr>, Box<TypedExpr>),
    ModuloAssignment(Box<TypedExpr>, Box<TypedExpr>),
    BitwiseAndAssignment(Box<TypedExpr>, Box<TypedExpr>),
    BitwiseOrAssignment(Box<TypedExpr>, Box<TypedExpr>),
    BitwiseXorAssignment(Box<TypedExpr>, Box<TypedExpr>),
    BitwiseLeftShiftAssignment(Box<TypedExpr>, Box<TypedExpr>),
    BitwiseRightShiftAssignment(Box<TypedExpr>, Box<TypedExpr>),

    UnaryNot(Box<TypedExpr>),
    UnaryBitwiseNot(Box<TypedExpr>),
    UnaryMinus(Box<TypedExpr>),
    UnaryAddressOf(Box<TypedExpr>),
    UnaryDereference(Box<TypedExpr>),

    Index(Box<TypedExpr>, Box<TypedExpr>),
    Dot(Box<TypedExpr>, String),
    Arrow(Box<TypedExpr>, String),
    Call(Box<TypedExpr>, Vec<TypedExpr>),

    Ternary(Box<TypedExpr>, Box<TypedExpr>, Box<TypedExpr>),

    LogicalAnd(Box<TypedExpr>, Box<TypedExpr>),
    LogicalOr(Box<TypedExpr>, Box<TypedExpr>),

    Equals(Box<TypedExpr>, Box<TypedExpr>),
    NotEquals(Box<TypedExpr>, Box<TypedExpr>),

    BitwiseAnd(Box<TypedExpr>, Box<TypedExpr>),
    BitwiseOr(Box<TypedExpr>, Box<TypedExpr>),
    BitwiseXor(Box<TypedExpr>, Box<TypedExpr>),

    GreaterThan(Box<TypedExpr>, Box<TypedExpr>),
    GreaterThanOrEqualTo(Box<TypedExpr>, Box<TypedExpr>),
    LessThan(Box<TypedExpr>, Box<TypedExpr>),
    LessThanOrEqualTo(Box<TypedExpr>, Box<TypedExpr>),

    BitwiseRightShift(Box<TypedExpr>, Box<TypedExpr>),
    BitwiseLeftShift(Box<TypedExpr>, Box<TypedExpr>),

    Addition(Box<TypedExpr>, Box<TypedExpr>),
    Subtraction(Box<TypedExpr>, Box<TypedExpr>),

    Multiplication(Box<TypedExpr>, Box<TypedExpr>),
    Division(Box<TypedExpr>, Box<TypedExpr>),
    Modulo(Box<TypedExpr>, Box<TypedExpr>),

    Cast(Box<TypedExpr>, super::ty::Type),

    NumberLiteral(String),
    StringLiteral(String),
    Identifier(String),
    BooleanLiteral(bool),
}

impl Display for TypedExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}) (", self.0)?;
        match self.1.clone() {
            TypedExprKind::NumberLiteral(n) => write!(f, "{}", n),
            TypedExprKind::StringLiteral(s) => write!(f, "{}", s),
            TypedExprKind::Identifier(i) => write!(f, "{}", i),
            TypedExprKind::BooleanLiteral(b) => write!(f, "{}", b),
            TypedExprKind::Assignment(l, r) => write!(f, "{} = {}", l, r),
            TypedExprKind::Addition(l, r) => write!(f, "{} + {}", l, r),
            TypedExprKind::Subtraction(l, r) => write!(f, "{} - {}", l, r),
            TypedExprKind::Multiplication(l, r) => write!(f, "{} * {}", l, r),
            TypedExprKind::Division(l, r) => write!(f, "{} / {}", l, r),
            TypedExprKind::Modulo(l, r) => write!(f, "{} % {}", l, r),
            TypedExprKind::BitwiseAnd(l, r) => write!(f, "{} & {}", l, r),
            TypedExprKind::BitwiseOr(l, r) => write!(f, "{} | {}", l, r),
            TypedExprKind::BitwiseXor(l, r) => write!(f, "{} ^ {}", l, r),
            TypedExprKind::BitwiseLeftShift(l, r) => write!(f, "{} << {}", l, r),
            TypedExprKind::BitwiseRightShift(l, r) => write!(f, "{} >> {}", l, r),
            TypedExprKind::GreaterThan(l, r) => write!(f, "{} > {}", l, r),
            TypedExprKind::GreaterThanOrEqualTo(l, r) => write!(f, "{} >= {}", l, r),
            TypedExprKind::LessThan(l, r) => write!(f, "{} < {}", l, r),
            TypedExprKind::LessThanOrEqualTo(l, r) => write!(f, "{} <= {}", l, r),
            TypedExprKind::Equals(l, r) => write!(f, "{} == {}", l, r),
            TypedExprKind::NotEquals(l, r) => write!(f, "{} != {}", l, r),
            TypedExprKind::LogicalAnd(l, r) => write!(f, "{} && {}", l, r),
            TypedExprKind::LogicalOr(l, r) => write!(f, "{} || {}", l, r),
            TypedExprKind::Comma(l, r) => write!(f, "{}, {}", l, r),
            TypedExprKind::AdditionAssignment(l, r) => write!(f, "{} += {}", l, r),
            TypedExprKind::SubtractionAssignment(l, r) => write!(f, "{} -= {}", l, r),
            TypedExprKind::MultiplicationAssignment(l, r) => write!(f, "{} *= {}", l, r),
            TypedExprKind::DivisionAssignment(l, r) => write!(f, "{} /= {}", l, r),
            TypedExprKind::ModuloAssignment(l, r) => write!(f, "{} %= {}", l, r),
            TypedExprKind::BitwiseAndAssignment(l, r) => write!(f, "{} &= {}", l, r),
            TypedExprKind::BitwiseOrAssignment(l, r) => write!(f, "{} |= {}", l, r),
            TypedExprKind::BitwiseXorAssignment(l, r) => write!(f, "{} ^= {}", l, r),
            TypedExprKind::BitwiseLeftShiftAssignment(l, r) => write!(f, "{} <<= {}", l, r),
            TypedExprKind::BitwiseRightShiftAssignment(l, r) => write!(f, "{} >>= {}", l, r),
            TypedExprKind::UnaryNot(e) => write!(f, "!{}", e),
            TypedExprKind::Cast(x, t) => write!(f, "{x} as {t}"),
            TypedExprKind::UnaryBitwiseNot(e) => write!(f, "~{}", e),
            TypedExprKind::UnaryMinus(e) => write!(f, "-{}", e),
            TypedExprKind::UnaryAddressOf(e) => write!(f, "&{}", e),
            TypedExprKind::UnaryDereference(e) => write!(f, "*{}", e),
            TypedExprKind::Arrow(l, r) => write!(f, "{}->{}", l, r),
            TypedExprKind::Ternary(l, m, r) => write!(f, "{} ? {} : {}", l, m, r),
            TypedExprKind::Index(a, b) => write!(f, "{}[{}]", a, b),
            TypedExprKind::Dot(a, b) => write!(f, "{}.{}", a, b),
            TypedExprKind::Call(a, b) => write!(
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
