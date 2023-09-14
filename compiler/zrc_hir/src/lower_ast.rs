pub fn lower_expr(expr: zrc_parser::ast::expr::Expr) -> Result<super::hir::expr::Expr, String> {
    use super::hir::expr::Expr as HirExpr;
    use zrc_parser::ast::expr::Expr as AstExpr;
    match expr {
        AstExpr::Error => Err("Parse error encountered. Cannot lower to HIR.".to_string()), // TODO: Allow this to lower, that way we can try to type check even with errors present

        AstExpr::Comma(a, b) => Ok(HirExpr::Comma(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::Assignment(a, b) => Ok(HirExpr::Assignment(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::AdditionAssignment(a, b) => Ok(HirExpr::AdditionAssignment(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::SubtractionAssignment(a, b) => Ok(HirExpr::SubtractionAssignment(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::MultiplicationAssignment(a, b) => Ok(HirExpr::MultiplicationAssignment(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::DivisionAssignment(a, b) => Ok(HirExpr::DivisionAssignment(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::ModuloAssignment(a, b) => Ok(HirExpr::ModuloAssignment(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::BitwiseAndAssignment(a, b) => Ok(HirExpr::BitwiseAndAssignment(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::BitwiseOrAssignment(a, b) => Ok(HirExpr::BitwiseOrAssignment(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::BitwiseXorAssignment(a, b) => Ok(HirExpr::BitwiseXorAssignment(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::BitwiseLeftShiftAssignment(a, b) => Ok(HirExpr::BitwiseLeftShiftAssignment(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::BitwiseRightShiftAssignment(a, b) => Ok(HirExpr::BitwiseRightShiftAssignment(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::UnaryNot(a) => Ok(HirExpr::UnaryNot(Box::new(lower_expr(*a)?))),
        AstExpr::UnaryBitwiseNot(a) => Ok(HirExpr::UnaryBitwiseNot(Box::new(lower_expr(*a)?))),
        AstExpr::UnaryMinus(a) => Ok(HirExpr::UnaryMinus(Box::new(lower_expr(*a)?))),
        AstExpr::UnaryAddressOf(a) => Ok(HirExpr::UnaryAddressOf(Box::new(lower_expr(*a)?))),
        AstExpr::UnaryDereference(a) => Ok(HirExpr::UnaryDereference(Box::new(lower_expr(*a)?))),
        AstExpr::Index(a, b) => Ok(HirExpr::Index(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::Dot(a, b) => Ok(HirExpr::Dot(Box::new(lower_expr(*a)?), b)),
        AstExpr::Arrow(a, b) => Ok(HirExpr::Arrow(Box::new(lower_expr(*a)?), b)),
        AstExpr::Call(a, b) => Ok(HirExpr::Call(
            Box::new(lower_expr(*a)?),
            b.into_iter()
                .map(|e| lower_expr(e))
                .collect::<Result<Vec<_>, _>>()?,
        )),
        AstExpr::Ternary(a, b, c) => Ok(HirExpr::Ternary(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
            Box::new(lower_expr(*c)?),
        )),
        AstExpr::LogicalAnd(a, b) => Ok(HirExpr::LogicalAnd(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::LogicalOr(a, b) => Ok(HirExpr::LogicalOr(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::Equals(a, b) => Ok(HirExpr::Equals(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::NotEquals(a, b) => Ok(HirExpr::NotEquals(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::BitwiseAnd(a, b) => Ok(HirExpr::BitwiseAnd(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::BitwiseOr(a, b) => Ok(HirExpr::BitwiseOr(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::BitwiseXor(a, b) => Ok(HirExpr::BitwiseXor(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::GreaterThan(a, b) => Ok(HirExpr::GreaterThan(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::GreaterThanOrEqualTo(a, b) => Ok(HirExpr::GreaterThanOrEqualTo(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::LessThan(a, b) => Ok(HirExpr::LessThan(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::LessThanOrEqualTo(a, b) => Ok(HirExpr::LessThanOrEqualTo(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::BitwiseRightShift(a, b) => Ok(HirExpr::BitwiseRightShift(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::BitwiseLeftShift(a, b) => Ok(HirExpr::BitwiseLeftShift(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::Addition(a, b) => Ok(HirExpr::Addition(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::Subtraction(a, b) => Ok(HirExpr::Subtraction(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::Multiplication(a, b) => Ok(HirExpr::Multiplication(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::Division(a, b) => Ok(HirExpr::Division(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::Modulo(a, b) => Ok(HirExpr::Modulo(
            Box::new(lower_expr(*a)?),
            Box::new(lower_expr(*b)?),
        )),
        AstExpr::NumberLiteral(n) => Ok(HirExpr::NumberLiteral(n)),
        AstExpr::StringLiteral(s) => Ok(HirExpr::StringLiteral(s)),
        AstExpr::Identifier(i) => Ok(HirExpr::Identifier(i)),
        AstExpr::BooleanLiteral(b) => Ok(HirExpr::BooleanLiteral(b)),
    }
}

pub fn lower_ty(ty: zrc_parser::ast::ty::Type) -> super::hir::ty::Type {
    use super::hir::ty::Type as HirType;
    use zrc_parser::ast::ty::Type as AstType;

    match ty {
        AstType::Identifier(x) => HirType::Identifier(x),
    }
}

pub fn lower_program(
    program: Vec<zrc_parser::ast::stmt::Declaration>,
) -> Result<Vec<super::hir::stmt::Declaration>, String> {
    use super::hir::stmt::ArgumentDeclaration as HirArgumentDeclaration;
    use super::hir::stmt::Declaration as HirDeclaration;
    use super::hir::stmt::LetDeclaration as HirLetDeclaration;
    use zrc_parser::ast::stmt::Declaration as AstDeclaration;

    program
        .into_iter()
        .map(|declaration| -> Result<HirDeclaration, String> {
            Ok(match declaration {
                AstDeclaration::DeclarationList(declarations) => HirDeclaration::DeclarationList(
                    declarations
                        .into_iter()
                        .map(|declaration| -> Result<HirLetDeclaration, String> {
                            Ok(HirLetDeclaration {
                                name: declaration.name,
                                ty: declaration.ty.map(lower_ty),
                                value: declaration.value.map(lower_expr).transpose()?,
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?,
                ),

                AstDeclaration::FunctionDefinition {
                    name,
                    parameters,
                    return_type,
                    body,
                } => HirDeclaration::FunctionDefinition {
                    name,
                    parameters: parameters
                        .into_iter()
                        .map(|argument| HirArgumentDeclaration {
                            name: argument.name,
                            ty: argument.ty.map(lower_ty),
                        })
                        .collect(),
                    return_type: return_type.map(lower_ty),
                    body: lower_block(body)?,
                },
            })
        })
        .collect::<Result<Vec<_>, _>>()
}

pub fn lower_block(
    block: Vec<zrc_parser::ast::stmt::Stmt>,
) -> Result<Vec<super::hir::stmt::Stmt>, String> {
    use super::hir::expr::Expr as HirExpr;
    use super::hir::stmt::Stmt as HirStmt;
    use zrc_parser::ast::expr::Expr as AstExpr;
    use zrc_parser::ast::stmt::Stmt as AstStmt;

    block
        .into_iter()
        .map(|stmt| -> Result<HirStmt, String> {
            Ok(match stmt {
                AstStmt::IfStmt(cond, then) => HirStmt::IfStmt {
                    cond: lower_expr(cond)?,
                    then: lower_block(vec![*then])?, // implicitly wrap in a block. if (x) y; becomes if (x) {y;}. Technically if (x) {y;} becomes if (x) {{y;}} but that's mainly unimportant.
                    then_else: None,
                },

                AstStmt::IfElseStmt(cond, then, then_else) => HirStmt::IfStmt {
                    cond: lower_expr(cond)?,
                    then: lower_block(vec![*then])?,
                    then_else: Some(lower_block(vec![*then_else])?),
                },

                // Infinite loops are desugared to just that -- infinite loops.
                AstStmt::WhileStmt(AstExpr::BooleanLiteral(true), stmt) => {
                    HirStmt::InfiniteLoop(lower_block(vec![*stmt])?) // same block magic
                }

                // While loops with a condition desugar to an infinite loop with a break condition.
                AstStmt::WhileStmt(cond, stmt) => HirStmt::InfiniteLoop(vec![HirStmt::IfStmt {
                    cond: HirExpr::UnaryNot(Box::new(lower_expr(cond)?)),
                    then: vec![HirStmt::BreakStmt],
                    then_else: Some(lower_block(vec![*stmt])?),
                }]),

                // And for loops are desugared as well.
                // for (init; cond; post) { body; }
                // becomes:
                // { // for scoping so init doesn't leak into parent scope
                //     init;
                //     while (cond) {
                //         { body; } // again ot prevent scope leak
                //         post;
                //     }
                // }
                //
                // Be aware this recurses. Don't use ForStmt anywhere below or you may end up in infinite recursion.
                AstStmt::ForStmt {
                    init,
                    cond,
                    post,
                    body,
                } => HirStmt::BlockStmt(lower_block(vec![
                    AstStmt::Declaration(*init),
                    AstStmt::WhileStmt(
                        cond,
                        Box::new(AstStmt::BlockStmt(vec![
                            AstStmt::BlockStmt(vec![*body]),
                            AstStmt::ExprStmt(post),
                        ])),
                    ),
                ])?),

                AstStmt::BlockStmt(stmts) => HirStmt::BlockStmt(lower_block(stmts)?),

                AstStmt::ExprStmt(expr) => HirStmt::ExprStmt(lower_expr(expr)?),

                AstStmt::BreakStmt => HirStmt::BreakStmt,
                AstStmt::ContinueStmt => HirStmt::ContinueStmt,
                AstStmt::EmptyStmt => HirStmt::EmptyStmt,
                AstStmt::ReturnStmt(x) => HirStmt::ReturnStmt(x.map(lower_expr).transpose()?),
                AstStmt::Declaration(declaration) => {
                    HirStmt::Declaration(lower_program(vec![declaration])?[0].clone())
                }
            })
        })
        .collect::<Result<Vec<_>, _>>()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn desugaring_works_as_expected() {
        use super::super::hir::{
            expr::Expr,
            stmt::{Declaration, LetDeclaration, Stmt},
            ty::Type,
        };

        println!(
            "{}",
            lower_program(
                zrc_parser::parser::parse_program(concat!(
                    "fn main() -> i32 {\n",
                    "  if (x) y;\n",
                    "  while (true) y;\n",
                    "  while (x) y;\n",
                    "  for (let i = 0; cond; post) body;\n",
                    "}"
                ))
                .unwrap()
            )
            .unwrap()
            .into_iter()
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .join("\n")
        );

        assert_eq!(
            lower_program(
                zrc_parser::parser::parse_program(concat!(
                    "fn main() -> i32 {\n",
                    "  if (x) y;\n",
                    "  while (true) y;\n",
                    "  while (x) y;\n",
                    "  for (let i = 0; cond; post) body;\n",
                    "}"
                ))
                .unwrap()
            ),
            Ok(vec![Declaration::FunctionDefinition {
                name: "main".to_string(),
                parameters: vec![],
                return_type: Some(Type::Identifier("i32".to_string())),
                body: vec![
                    Stmt::IfStmt {
                        cond: Expr::Identifier("x".to_string()),
                        // implicitly wrapped in block
                        then: vec![Stmt::ExprStmt(Expr::Identifier("y".to_string()))],
                        then_else: None,
                    },
                    // also wrapped in implicit box
                    Stmt::InfiniteLoop(vec![Stmt::ExprStmt(Expr::Identifier("y".to_string()))]),
                    // desugared to infinite loop
                    Stmt::InfiniteLoop(vec![Stmt::IfStmt {
                        cond: Expr::UnaryNot(Box::new(Expr::Identifier("x".to_string()))),
                        then: vec![Stmt::BreakStmt],
                        then_else: Some(vec![Stmt::ExprStmt(Expr::Identifier("y".to_string()))])
                    }]),
                    // desugared to infinite loop as well
                    Stmt::BlockStmt(vec![
                        Stmt::Declaration(Declaration::DeclarationList(vec![LetDeclaration {
                            name: "i".to_string(),
                            ty: None,
                            value: Some(Expr::NumberLiteral("0".to_string()))
                        },])),
                        Stmt::InfiniteLoop(vec![Stmt::IfStmt {
                            cond: Expr::UnaryNot(Box::new(Expr::Identifier("cond".to_string()))),
                            then: vec![Stmt::BreakStmt],
                            then_else: Some(vec![Stmt::BlockStmt(vec![
                                Stmt::BlockStmt(vec![Stmt::ExprStmt(Expr::Identifier(
                                    "body".to_string()
                                ))]),
                                Stmt::ExprStmt(Expr::Identifier("post".to_string()))
                            ])])
                        }])
                    ])
                ]
            }])
        )
    }
}
