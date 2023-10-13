//! for blocks

use std::collections::HashMap;

use anyhow::bail;
use zrc_diagnostics::{Diagnostic, DiagnosticKind, Severity, Spanned as DiagnosticSpan};
use zrc_parser::ast::{
    stmt::{Declaration as AstDeclaration, LetDeclaration as AstLetDeclaration, Stmt, StmtKind},
    Spanned,
};

use super::{resolve_type, type_expr, Scope};
use crate::tast::{
    expr::TypedExpr,
    stmt::{
        ArgumentDeclaration as TastArgumentDeclaration, LetDeclaration as TastLetDeclaration,
        TypedDeclaration, TypedStmt,
    },
    ty::Type as TastType,
};

/// Describes whether a block returns void or a type.
#[derive(Debug, Clone, PartialEq)]
#[allow(clippy::module_name_repetitions)]
pub enum BlockReturnType {
    /// The function/block returns void (`fn()`)
    Void,
    /// The function/block returns T (`fn() -> T`)
    Return(TastType),
}

impl BlockReturnType {
    /// Converts this [`BlockReturnType`] into None for void and Some(t) for
    /// Return(t).
    #[must_use]
    #[allow(clippy::missing_const_for_fn)] // I think clippy's high.
    pub fn into_option(self) -> Option<TastType> {
        match self {
            Self::Void => None,
            Self::Return(t) => Some(t),
        }
    }

    /// Converts this [`BlockReturnType`] to a [`TastType`]
    #[must_use]
    #[allow(clippy::missing_const_for_fn)] // I think clippy's high.
    pub fn into_tast_type(self) -> TastType {
        match self {
            Self::Void => TastType::Void,
            Self::Return(t) => t,
        }
    }
}

/// Describes if a block MAY, MUST, or MUST NOT return.
#[derive(Debug, Clone, PartialEq)]
#[allow(clippy::module_name_repetitions)]
pub enum BlockReturnAbility {
    /// The block MUST NOT return at any point.
    MustNotReturn,

    /// The block MAY return, but it is not required.
    ///
    /// Any sub-blocks of this block MAY return.
    MayReturn(BlockReturnType),

    /// The block MUST return.
    ///
    /// Any sub-blocks of this block MAY return. At least one MUST return.
    MustReturn(BlockReturnType),
}

/// Describes if a block labeled [MAY return](BlockReturnAbility::MayReturn)
/// actually returns.
///
/// This is necessary for determining the fulfillment of a [MUST
/// return](BlockReturnAbility::MustReturn) when a block contains a nested block
/// (because the outer block must have at least *one* path which is guaranteed
/// to return)
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::module_name_repetitions)]
pub enum BlockReturnActuality {
    /// The block is guaranteed to never return on any path.
    DoesNotReturn,

    /// The block will return on some paths but not all. In some cases, this may
    /// be selected even if the block will always return, as it is sometimes
    /// unknown.
    MightReturn,

    /// The block is guaranteed to return on any path.
    WillReturn,
}

/// Convert a single [AST statement](Stmt) like `x;` to a block statement `{ x;
/// }` without converting `{ x; }` to `{ { x; } }`. This is preferred instead of
/// `vec![x]` as it prevents extra nesting layers.
fn coerce_stmt_into_block(stmt: Stmt) -> super::Spanned<Vec<Stmt>> {
    match stmt.0 .1 {
        StmtKind::BlockStmt(stmts) => Spanned(stmt.0 .0, stmts, stmt.0 .2),
        _ => Spanned(stmt.0 .0, vec![stmt.clone()], stmt.0 .2),
    }
}

/// Process a vector of [AST let declarations](AstLetDeclaration) and insert it
/// into the scope, returning a vector of [TAST let
/// declarations](TastLetDeclaration).
///
/// # Errors
/// Errors with type checker errors.
pub fn process_let_declaration(
    scope: &mut Scope,
    declarations: Vec<Spanned<AstLetDeclaration>>,
) -> Result<Vec<TastLetDeclaration>, zrc_diagnostics::Diagnostic> {
    declarations
        .into_iter()
        .map(
            |let_declaration| -> Result<TastLetDeclaration, Diagnostic> {
                if scope.get_value(&let_declaration.1.name.1).is_some() {
                    // TODO: In the future we may allow shadowing but currently no
                    return Err(Diagnostic(
                        Severity::Error,
                        DiagnosticSpan(
                            let_declaration.1.name.0,
                            DiagnosticKind::IdentifierAlreadyInUse(let_declaration.1.name.1),
                            let_declaration.1.name.2,
                        ),
                    ));
                }

                let typed_expr = let_declaration
                    .1
                    .value
                    .clone()
                    .map(|expr| type_expr(scope, expr))
                    .transpose()?;
                let resolved_ty = let_declaration
                    .1
                    .ty
                    .map(|ty| resolve_type(scope, ty))
                    .transpose()?;

                let result_decl = match (typed_expr, resolved_ty) {
                    (None, None) => {
                        return Err(Diagnostic(
                            Severity::Error,
                            DiagnosticSpan(
                                let_declaration.0,
                                DiagnosticKind::NoTypeNoValue,
                                let_declaration.2,
                            ),
                        ));
                    }

                    // Explicitly typed with no value
                    (None, Some(ty)) => TastLetDeclaration {
                        name: let_declaration.1.name.1,
                        ty,
                        value: None,
                    },

                    // Infer type from value
                    (Some(TypedExpr(ty, ex)), None) => TastLetDeclaration {
                        name: let_declaration.1.name.1,
                        ty: ty.clone(),
                        value: Some(TypedExpr(ty, ex)),
                    },

                    // Both explicitly typed and inferable
                    (Some(TypedExpr(ty, ex)), Some(resolved_ty)) => {
                        if ty == resolved_ty {
                            TastLetDeclaration {
                                name: let_declaration.1.name.1,
                                ty: ty.clone(),
                                value: Some(TypedExpr(ty, ex)),
                            }
                        } else {
                            return Err(Diagnostic(
                                Severity::Error,
                                DiagnosticSpan(
                                    let_declaration.1.value.clone().unwrap().0 .0,
                                    DiagnosticKind::InvalidAssignmentRightHandSideType {
                                        expected: resolved_ty.to_string(),
                                        got: ty.to_string(),
                                    },
                                    let_declaration.1.value.clone().unwrap().0 .2,
                                ),
                            ));
                        }
                    }
                };
                scope.set_value(result_decl.name.clone(), result_decl.ty.clone());
                Ok(result_decl)
            },
        )
        .collect::<Result<Vec<_>, Diagnostic>>()
}

/// Process a top-level [AST declaration](AstDeclaration), insert it into the
/// scope, and return a [TAST declaration](TypedDeclaration).
///
/// This should only be used in the global scope.
///
/// # Errors
/// Errors if a type checker error is encountered.
#[allow(clippy::too_many_lines)]
pub fn process_declaration(
    global_scope: &mut Scope,
    declaration: AstDeclaration,
) -> Result<TypedDeclaration, zrc_diagnostics::Diagnostic> {
    Ok(match declaration {
        AstDeclaration::FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
        } => {
            if global_scope.get_value(&name.1).is_some() {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        name.0,
                        DiagnosticKind::IdentifierAlreadyInUse(name.1),
                        name.2,
                    ),
                ));
            }

            let resolved_return_type = return_type
                .map(|ty| resolve_type(global_scope, ty))
                .transpose()?
                .map_or(BlockReturnType::Void, BlockReturnType::Return);

            let resolved_parameters = parameters
                .1
                .into_iter()
                .map(|parameter| -> Result<TastArgumentDeclaration, Diagnostic> {
                    Ok(TastArgumentDeclaration {
                        name: parameter.1.name.1,
                        ty: resolve_type(global_scope, parameter.1.ty)?,
                    })
                })
                .collect::<Result<Vec<_>, Diagnostic>>()?;

            global_scope.set_value(
                name.clone().1,
                TastType::Fn(
                    resolved_parameters
                        .iter()
                        .map(|x| x.ty.clone())
                        .collect::<Vec<_>>(),
                    Box::new(resolved_return_type.clone()),
                ),
            );

            TypedDeclaration::FunctionDeclaration {
                name: name.1,
                parameters: resolved_parameters.clone(),
                return_type: resolved_return_type.clone().into_option(),
                body: if let Some(body) = body {
                    let mut function_scope = global_scope.clone();
                    for param in resolved_parameters {
                        function_scope.set_value(param.name, param.ty);
                    }

                    // discard return actuality as it's guaranteed
                    Some(
                        type_block(
                            &function_scope,
                            body,
                            false,
                            BlockReturnAbility::MustReturn(resolved_return_type),
                        )?
                        .0,
                    )
                } else {
                    None
                },
            }
        }
        AstDeclaration::StructDeclaration { name, fields } => {
            if global_scope.get_type(&name.1).is_some() {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        name.0,
                        DiagnosticKind::IdentifierAlreadyInUse(name.1),
                        name.2,
                    ),
                ));
            }

            let resolved_pairs = fields
                .1
                .into_iter()
                .map(|(name, ty)| -> Result<(String, TastType), Diagnostic> {
                    Ok((name, resolve_type(global_scope, ty.1 .1)?))
                })
                .collect::<Result<HashMap<_, _>, Diagnostic>>()?;

            global_scope.set_type(name.1.clone(), TastType::Struct(resolved_pairs.clone()));

            TypedDeclaration::StructDeclaration {
                name: name.1,
                fields: resolved_pairs,
            }
        }
    })
}

/// Type check a block of [AST statement](Stmt)s and return a block of [TAST
/// statement](TypedStmt)s.
///
/// It performs a small desugaring where all statements become implicit blocks.
///
/// This function must be provided a block of statements, and a few bits of
/// information about the parent scope in form of booleans that toggle certain
/// statements like `break` and a [`BlockReturnAbility`].
///
/// # Behavior of block returns
/// In many cases, a block [MUST return](BlockReturnAbility::MustReturn). For
/// example, this is done in the main block of a function. When a function
/// contains sub-blocks, those blocks [*may*
/// return](BlockReturnAbility::MayReturn) but are not required to. However, at
/// least one of the blocks within must be guaranteed to return in order to
/// fulfill a MUST return, otherwise the function is not guaranteed to return.
/// So, if you pass this function a **may** return order, it will return a
/// [`BlockReturnActuality`] which can be used to determine if a MUST return is
/// fulfilled.
///
/// ```rs
/// { // This block must return.
///     { // This block MAY return.
///         if (x) return; // This MAY return.
///     } // This block WILL SOMETIMES return.
///     // Because the above block is not GUARANTEED to return, the "must
///     // return" is not yet satisfied.
/// }
/// ```
///
/// # Errors
/// Errors if a type checker error is encountered.
// TODO: Maybe the TAST should attach the BlockReturnActuality in each BlockStmt itself and preserve
// it on sub-blocks in the TAST (this may be helpful in control flow analysis)
#[allow(clippy::too_many_lines)]
#[allow(clippy::module_name_repetitions)]
pub fn type_block(
    parent_scope: &Scope,
    input_block: super::Spanned<Vec<Stmt>>,
    can_use_break_continue: bool,
    return_ability: BlockReturnAbility,
) -> Result<(Vec<TypedStmt>, BlockReturnActuality), zrc_diagnostics::Diagnostic> {
    let mut scope = parent_scope.clone();

    // At first, the block does not return.
    let (tast_block, return_actualities): (Vec<_>, Vec<_>) = input_block
        .1
        .into_iter()
        .map(
            |stmt| -> Result<(TypedStmt, BlockReturnActuality), Diagnostic> {
                match stmt.0 .1 {
                    StmtKind::EmptyStmt => {
                        Ok((TypedStmt::EmptyStmt, BlockReturnActuality::DoesNotReturn))
                    }
                    StmtKind::BreakStmt if can_use_break_continue => {
                        Ok((TypedStmt::BreakStmt, BlockReturnActuality::DoesNotReturn))
                    }
                    StmtKind::BreakStmt => {
                        return Err(Diagnostic(
                            Severity::Error,
                            DiagnosticSpan(
                                stmt.0 .0,
                                DiagnosticKind::CannotUseBreakOutsideOfLoop,
                                stmt.0 .2,
                            ),
                        ))
                    }

                    StmtKind::ContinueStmt if can_use_break_continue => {
                        Ok((TypedStmt::BreakStmt, BlockReturnActuality::DoesNotReturn))
                    }
                    StmtKind::ContinueStmt => {
                        return Err(Diagnostic(
                            Severity::Error,
                            DiagnosticSpan(
                                stmt.0 .0,
                                DiagnosticKind::CannotUseContinueOutsideOfLoop,
                                stmt.0 .2,
                            ),
                        ))
                    }

                    StmtKind::DeclarationList(declarations) => Ok((
                        TypedStmt::DeclarationList(process_let_declaration(
                            &mut scope,
                            declarations.1,
                        )?),
                        BlockReturnActuality::DoesNotReturn, /* because expressions can't return */
                    )),

                    StmtKind::IfStmt(cond, then, then_else) => {
                        // TODO: if `cond` is always true at compile-time, we can prove the if
                        // branch is always taken (hence if it's WillReturn we can be WillReturn
                        // instead of MayReturn)

                        let typed_cond = type_expr(&scope, cond.clone())?;

                        if typed_cond.0 != TastType::Bool {
                            return Err(Diagnostic(
                                Severity::Error,
                                DiagnosticSpan(
                                    cond.0 .0,
                                    DiagnosticKind::ExpectedGot {
                                        expected: "bool".to_string(),
                                        got: typed_cond.0.to_string(),
                                    },
                                    cond.0 .2,
                                ),
                            ));
                        }

                        let (typed_then, then_return_actuality) = type_block(
                            &scope,
                            coerce_stmt_into_block(*then),
                            can_use_break_continue,
                            // return ability of a sub-block is determined by this match:
                            match return_ability.clone() {
                                BlockReturnAbility::MustNotReturn => {
                                    BlockReturnAbility::MustNotReturn
                                }
                                BlockReturnAbility::MustReturn(x)
                                | BlockReturnAbility::MayReturn(x) => {
                                    BlockReturnAbility::MayReturn(x)
                                }
                            },
                        )?;

                        let (typed_then_else, then_else_return_actuality) = then_else
                            .map(|then_else| {
                                type_block(
                                    &scope,
                                    coerce_stmt_into_block(*then_else),
                                    can_use_break_continue,
                                    // return ability of a sub-block is determined by this match:
                                    match return_ability.clone() {
                                        BlockReturnAbility::MustNotReturn => {
                                            BlockReturnAbility::MustNotReturn
                                        }
                                        BlockReturnAbility::MustReturn(x)
                                        | BlockReturnAbility::MayReturn(x) => {
                                            BlockReturnAbility::MayReturn(x)
                                        }
                                    },
                                )
                            })
                            .transpose()?
                            .unzip();

                        Ok((
                            TypedStmt::IfStmt(typed_cond, typed_then, typed_then_else),
                            match (
                                then_return_actuality,
                                then_else_return_actuality
                                    .unwrap_or(BlockReturnActuality::DoesNotReturn),
                            ) {
                                (
                                    BlockReturnActuality::DoesNotReturn,
                                    BlockReturnActuality::DoesNotReturn,
                                ) => BlockReturnActuality::DoesNotReturn,
                                (
                                    BlockReturnActuality::DoesNotReturn,
                                    BlockReturnActuality::WillReturn,
                                )
                                | (
                                    BlockReturnActuality::WillReturn,
                                    BlockReturnActuality::DoesNotReturn,
                                )
                                | (BlockReturnActuality::MightReturn, _)
                                | (_, BlockReturnActuality::MightReturn) => {
                                    BlockReturnActuality::MightReturn
                                }
                                (
                                    BlockReturnActuality::WillReturn,
                                    BlockReturnActuality::WillReturn,
                                ) => BlockReturnActuality::WillReturn,
                            },
                        ))
                    }
                    StmtKind::WhileStmt(cond, body) => {
                        // TODO: we might be able to prove that the body runs at least once or an
                        // infinite loop making this won't/will return statically

                        let typed_cond = type_expr(&scope, cond.clone())?;

                        if typed_cond.0 != TastType::Bool {
                            return Err(Diagnostic(
                                Severity::Error,
                                DiagnosticSpan(
                                    cond.0 .0,
                                    DiagnosticKind::ExpectedGot {
                                        expected: "bool".to_string(),
                                        got: typed_cond.0.to_string(),
                                    },
                                    cond.0 .2,
                                ),
                            ));
                        }

                        let (typed_body, body_return_actuality) = type_block(
                            &scope,
                            coerce_stmt_into_block(*body),
                            true,
                            // return ability of a sub-block is determined by this match:
                            match return_ability.clone() {
                                BlockReturnAbility::MustNotReturn => {
                                    BlockReturnAbility::MustNotReturn
                                }
                                BlockReturnAbility::MustReturn(x)
                                | BlockReturnAbility::MayReturn(x) => {
                                    BlockReturnAbility::MayReturn(x)
                                }
                            },
                        )?;

                        Ok((
                            TypedStmt::WhileStmt(typed_cond, typed_body),
                            match body_return_actuality {
                                BlockReturnActuality::DoesNotReturn => {
                                    BlockReturnActuality::DoesNotReturn
                                }

                                // in case the loop does not run at all or runs infinitely,
                                // WillReturn counts too
                                BlockReturnActuality::MightReturn
                                | BlockReturnActuality::WillReturn => {
                                    BlockReturnActuality::MightReturn
                                }
                            },
                        ))
                    }
                    StmtKind::ForStmt {
                        init,
                        cond,
                        post,
                        body,
                    } => {
                        // TODO: same logic as the TODO comment on the while loop applies here.

                        // the declaration made in the for loop's init is scoped to *only* the loop
                        // so we need to make a subscope for it
                        let mut loop_scope = scope.clone();

                        // if present, evaluate the declaration
                        let typed_init = init
                            .map(|decl| process_let_declaration(&mut loop_scope, decl.1))
                            .transpose()?;

                        let typed_cond = cond
                            .clone()
                            .map(|cond| type_expr(&loop_scope, cond))
                            .transpose()?;

                        if let Some(inner_t_cond) = typed_cond.clone() {
                            if inner_t_cond.0 != TastType::Bool {
                                return Err(Diagnostic(
                                    Severity::Error,
                                    DiagnosticSpan(
                                        cond.clone().unwrap().0 .0,
                                        DiagnosticKind::ExpectedGot {
                                            expected: "bool".to_string(),
                                            got: inner_t_cond.0.to_string(),
                                        },
                                        cond.clone().unwrap().0 .2,
                                    ),
                                ));
                            }
                        }

                        let typed_post =
                            post.map(|post| type_expr(&loop_scope, post)).transpose()?;

                        let (typed_body, body_return_actuality) = type_block(
                            &loop_scope,
                            coerce_stmt_into_block(*body),
                            true,
                            // return ability of a sub-block is determined by this match:
                            match return_ability.clone() {
                                BlockReturnAbility::MustNotReturn => {
                                    BlockReturnAbility::MustNotReturn
                                }
                                BlockReturnAbility::MustReturn(x)
                                | BlockReturnAbility::MayReturn(x) => {
                                    BlockReturnAbility::MayReturn(x)
                                }
                            },
                        )?;

                        Ok((
                            TypedStmt::ForStmt {
                                init: typed_init.map(Box::new),
                                cond: typed_cond,
                                post: typed_post,
                                body: typed_body,
                            },
                            match body_return_actuality {
                                BlockReturnActuality::DoesNotReturn => {
                                    BlockReturnActuality::DoesNotReturn
                                }

                                // in case the loop does not run at all or runs infinitely,
                                // WillReturn counts too
                                BlockReturnActuality::MightReturn
                                | BlockReturnActuality::WillReturn => {
                                    BlockReturnActuality::MightReturn
                                }
                            },
                        ))
                    }

                    StmtKind::BlockStmt(body) => {
                        let (typed_body, return_actuality) = type_block(
                            &scope,
                            Spanned(stmt.0 .0, body, stmt.0 .2),
                            can_use_break_continue,
                            // return ability of a sub-block is determined by this match:
                            match return_ability.clone() {
                                BlockReturnAbility::MustNotReturn => {
                                    BlockReturnAbility::MustNotReturn
                                }
                                BlockReturnAbility::MustReturn(x)
                                | BlockReturnAbility::MayReturn(x) => {
                                    BlockReturnAbility::MayReturn(x)
                                }
                            },
                        )?;
                        Ok((TypedStmt::BlockStmt(typed_body), return_actuality))
                    }

                    StmtKind::ExprStmt(expr) => Ok((
                        TypedStmt::ExprStmt(type_expr(&scope, expr)?),
                        BlockReturnActuality::DoesNotReturn,
                    )),
                    StmtKind::ReturnStmt(value) => {
                        let resolved_value = value
                            .clone()
                            .map(|expr| type_expr(&scope, expr))
                            .transpose()?;
                        match (resolved_value, return_ability.clone()) {
                            // expects no return
                            (_, BlockReturnAbility::MustNotReturn) => {
                                return Err(Diagnostic(
                                    Severity::Error,
                                    DiagnosticSpan(
                                        stmt.0 .0,
                                        DiagnosticKind::CannotReturnHere,
                                        stmt.0 .2,
                                    ),
                                ));
                            }

                            // return; in void fn
                            (
                                None,
                                BlockReturnAbility::MayReturn(BlockReturnType::Void)
                                | BlockReturnAbility::MustReturn(BlockReturnType::Void),
                            ) => Ok((
                                TypedStmt::ReturnStmt(None),
                                BlockReturnActuality::WillReturn,
                            )),

                            // return; in fn with required return type
                            (
                                None,
                                BlockReturnAbility::MayReturn(BlockReturnType::Return(t))
                                | BlockReturnAbility::MustReturn(BlockReturnType::Return(t)),
                            ) => {
                                return Err(Diagnostic(
                                    Severity::Error,
                                    DiagnosticSpan(
                                        stmt.0 .0,
                                        DiagnosticKind::ExpectedGot {
                                            expected: t.to_string(),
                                            got: "void".to_string(),
                                        },
                                        stmt.0 .2,
                                    ),
                                ));
                            }

                            // return x; in fn expecting to return void
                            (
                                Some(TypedExpr(ty, _)),
                                BlockReturnAbility::MustReturn(BlockReturnType::Void)
                                | BlockReturnAbility::MayReturn(BlockReturnType::Void),
                            ) => {
                                return Err(Diagnostic(
                                    Severity::Error,
                                    DiagnosticSpan(
                                        stmt.0 .0,
                                        DiagnosticKind::ExpectedGot {
                                            expected: "void".to_string(),
                                            got: ty.to_string(),
                                        },
                                        stmt.0 .2,
                                    ),
                                ));
                            }

                            // return x; in fn expecting to return x
                            (
                                Some(TypedExpr(ty, ex)),
                                BlockReturnAbility::MustReturn(BlockReturnType::Return(t))
                                | BlockReturnAbility::MayReturn(BlockReturnType::Return(t)),
                            ) => {
                                if ty == t {
                                    Ok((
                                        TypedStmt::ReturnStmt(Some(TypedExpr(ty, ex))),
                                        BlockReturnActuality::WillReturn,
                                    ))
                                } else {
                                    return Err(Diagnostic(
                                        Severity::Error,
                                        DiagnosticSpan(
                                            value.clone().unwrap().0 .0,
                                            DiagnosticKind::ExpectedGot {
                                                expected: t.to_string(),
                                                got: ty.to_string(),
                                            },
                                            value.unwrap().0 .2,
                                        ),
                                    ));
                                }
                            }
                        }
                    }
                }
            },
        )
        .collect::<Result<Vec<_>, Diagnostic>>()?
        .into_iter()
        .unzip();

    let might_return = return_actualities.iter().any(|x| {
        matches!(
            x,
            BlockReturnActuality::MightReturn | BlockReturnActuality::WillReturn
        )
    });
    let will_return = return_actualities
        .iter()
        .any(|x| matches!(x, BlockReturnActuality::WillReturn));

    let return_actuality = match (might_return, will_return) {
        (_, true) => BlockReturnActuality::WillReturn,
        (true, false) => BlockReturnActuality::MightReturn,
        (false, false) => BlockReturnActuality::DoesNotReturn,
    };

    #[allow(clippy::match_same_arms)] // for clarity
    match (return_ability, return_actuality) {
        (BlockReturnAbility::MustNotReturn, BlockReturnActuality::DoesNotReturn) => {
            Ok((tast_block, BlockReturnActuality::DoesNotReturn))
        }
        (BlockReturnAbility::MustReturn(_), BlockReturnActuality::WillReturn) => {
            Ok((tast_block, BlockReturnActuality::WillReturn))
        }
        (BlockReturnAbility::MayReturn(_), BlockReturnActuality::WillReturn) => {
            Ok((tast_block, BlockReturnActuality::WillReturn))
        }
        (BlockReturnAbility::MayReturn(_), BlockReturnActuality::MightReturn) => {
            Ok((tast_block, BlockReturnActuality::MightReturn))
        }
        (BlockReturnAbility::MayReturn(_), BlockReturnActuality::DoesNotReturn) => {
            Ok((tast_block, BlockReturnActuality::DoesNotReturn))
        }
        (BlockReturnAbility::MustReturn(_), BlockReturnActuality::MightReturn) => {
            return Err(Diagnostic(
                Severity::Error,
                DiagnosticSpan(
                    input_block.0,
                    DiagnosticKind::ExpectedABlockToReturn,
                    input_block.2,
                ),
            ));
        }
        (BlockReturnAbility::MustReturn(_), BlockReturnActuality::DoesNotReturn) => {
            return Err(Diagnostic(
                Severity::Error,
                DiagnosticSpan(
                    input_block.0,
                    DiagnosticKind::ExpectedABlockToReturn,
                    input_block.2,
                ),
            ));
        }
        (BlockReturnAbility::MustNotReturn, BlockReturnActuality::MightReturn) => {
            panic!("block must not return, but a sub-block may return -- this should have been caught when checking that block");
        }
        (BlockReturnAbility::MustNotReturn, BlockReturnActuality::WillReturn) => {
            panic!("block must not return, but a sub-block will return -- this should have been caught when checking that block");
        }
    }
}
