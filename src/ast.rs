#[derive(PartialEq, Debug, Clone, new)]
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
    UnaryMinus(Box<Expr>),
    PreIncrement(Box<Expr>),
    PreDecrement(Box<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Dot(Box<Expr>, String),
    NamespaceAccess(Box<Expr>, String),
    PostIncrement(Box<Expr>),
    PostDecrement(Box<Expr>),
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

    NumberLiteral(String),
    StringLiteral(String),
    Identifier(String),
    BooleanLiteral(bool),

    Parenthesized(Box<Expr>),

    Error,
}
