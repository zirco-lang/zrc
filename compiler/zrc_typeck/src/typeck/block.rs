//! for blocks

use zrc_diagnostics::{Diagnostic, DiagnosticKind, Severity};
use zrc_parser::ast::stmt::{
    ArgumentDeclarationList, Declaration as AstDeclaration, LetDeclaration as AstLetDeclaration,
    Stmt, StmtKind,
};
use zrc_utils::span::{Spannable, Spanned};

use super::{resolve_type, type_expr, Scope};
use crate::tast::{
    self,
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
pub enum BlockReturnType<'input> {
    /// The function/block returns void (`fn()`)
    Void,
    /// The function/block returns T (`fn() -> T`)
    Return(TastType<'input>),
}

impl<'input> BlockReturnType<'input> {
    /// Converts this [`BlockReturnType`] into None for void and Some(t) for
    /// Return(t).
    #[must_use]
    #[allow(clippy::missing_const_for_fn)] // I think clippy's high.
    pub fn into_option(self) -> Option<TastType<'input>> {
        match self {
            Self::Void => None,
            Self::Return(return_type) => Some(return_type),
        }
    }

    /// Converts this [`BlockReturnType`] to a [`TastType`]
    #[must_use]
    #[allow(clippy::missing_const_for_fn)] // I think clippy's high.
    pub fn into_tast_type(self) -> TastType<'input> {
        match self {
            Self::Void => TastType::Void,
            Self::Return(return_type) => return_type,
        }
    }
}

/// Describes if a block MAY, MUST, or MUST NOT return.
#[derive(Debug, Clone, PartialEq)]
#[allow(clippy::module_name_repetitions)]
pub enum BlockReturnAbility<'input> {
    /// The block MUST NOT return at any point.
    MustNotReturn,

    /// The block MAY return, but it is not required.
    ///
    /// Any sub-blocks of this block MAY return.
    MayReturn(BlockReturnType<'input>),

    /// The block MUST return.
    ///
    /// Any sub-blocks of this block MAY return. At least one MUST return.
    MustReturn(BlockReturnType<'input>),
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
fn coerce_stmt_into_block(stmt: Stmt<'_>) -> Spanned<Vec<Stmt<'_>>> {
    #[allow(clippy::wildcard_enum_match_arm)]
    stmt.0.clone().map(|value| match value {
        StmtKind::BlockStmt(stmts) => stmts,
        _ => vec![stmt],
    })
}

/// Process a vector of [AST let declarations](AstLetDeclaration) and insert it
/// into the scope, returning a vector of [TAST let
/// declarations](TastLetDeclaration).
///
/// # Errors
/// Errors with type checker errors.
pub fn process_let_declaration<'input>(
    scope: &mut Scope<'input>,
    declarations: Vec<Spanned<AstLetDeclaration<'input>>>,
) -> Result<Vec<TastLetDeclaration<'input>>, zrc_diagnostics::Diagnostic> {
    declarations
        .into_iter()
        .map(
            |let_declaration| -> Result<TastLetDeclaration, Diagnostic> {
                if scope
                    .get_value(let_declaration.value().name.value())
                    .is_some()
                {
                    // TODO: In the future we may allow shadowing but currently no
                    return Err(Diagnostic(
                        Severity::Error,
                        let_declaration
                            .value()
                            .name
                            .map(|x| DiagnosticKind::IdentifierAlreadyInUse(x.to_string())),
                    ));
                }

                let typed_expr = let_declaration
                    .value()
                    .value
                    .clone()
                    .map(|expr| type_expr(scope, expr))
                    .transpose()?;
                let resolved_ty = let_declaration
                    .value()
                    .ty
                    .clone()
                    .map(|ty| resolve_type(scope, ty))
                    .transpose()?;

                let result_decl = match (typed_expr, resolved_ty) {
                    (None, None) => {
                        return Err(Diagnostic(
                            Severity::Error,
                            let_declaration
                                .span()
                                .containing(DiagnosticKind::NoTypeNoValue),
                        ));
                    }

                    // Explicitly typed with no value
                    (None, Some(ty)) => TastLetDeclaration {
                        name: let_declaration.value().name.into_value(),
                        ty,
                        value: None,
                    },

                    // Infer type from value
                    (Some(TypedExpr(ty, ex)), None) => TastLetDeclaration {
                        name: let_declaration.value().name.into_value(),
                        ty: ty.clone(),
                        value: Some(TypedExpr(ty, ex)),
                    },

                    // Both explicitly typed and inferable
                    (Some(TypedExpr(ty, ex)), Some(resolved_ty)) => {
                        if ty == resolved_ty {
                            TastLetDeclaration {
                                name: let_declaration.value().name.into_value(),
                                ty: ty.clone(),
                                value: Some(TypedExpr(ty, ex)),
                            }
                        } else {
                            return Err(Diagnostic(
                                Severity::Error,
                                let_declaration
                                    .value()
                                    .value
                                    .clone()
                                    .unwrap()
                                    .0
                                    .span()
                                    .containing(
                                        DiagnosticKind::InvalidAssignmentRightHandSideType {
                                            expected: resolved_ty.to_string(),
                                            got: ty.to_string(),
                                        },
                                    ),
                            ));
                        }
                    }
                };
                scope.set_value(result_decl.name, result_decl.ty.clone());
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
pub fn process_declaration<'input>(
    global_scope: &mut Scope<'input>,
    declaration: AstDeclaration<'input>,
) -> Result<TypedDeclaration<'input>, zrc_diagnostics::Diagnostic> {
    Ok(match declaration {
        AstDeclaration::FunctionDeclaration {
            parameters,
            body: Some(_),
            ..
        } if matches!(parameters.value(), ArgumentDeclarationList::Variadic(_)) => {
            return Err(Diagnostic(
                Severity::Error,
                parameters.map(|_| DiagnosticKind::VariadicFunctionMustBeExternal),
            ));
        }

        AstDeclaration::FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
        } => {
            if global_scope.get_value(name.value()).is_some() {
                return Err(Diagnostic(
                    Severity::Error,
                    name.map(|x| DiagnosticKind::IdentifierAlreadyInUse(x.to_string())),
                ));
            }

            let resolved_return_type = return_type
                .map(|ty| resolve_type(global_scope, ty))
                .transpose()?
                .map_or(BlockReturnType::Void, BlockReturnType::Return);

            let (ArgumentDeclarationList::NonVariadic(inner_params)
            | ArgumentDeclarationList::Variadic(inner_params)) = parameters.value();

            let resolved_parameters = inner_params
                .iter()
                .map(|parameter| -> Result<TastArgumentDeclaration, Diagnostic> {
                    Ok(TastArgumentDeclaration {
                        name: parameter.value().name.into_value(),
                        ty: resolve_type(global_scope, parameter.value().ty.clone())?,
                    })
                })
                .collect::<Result<Vec<_>, Diagnostic>>()?;

            global_scope.set_value(
                name.into_value(),
                TastType::Fn(
                    match parameters.value() {
                        ArgumentDeclarationList::NonVariadic(_) => {
                            tast::stmt::ArgumentDeclarationList::NonVariadic(
                                resolved_parameters.clone(),
                            )
                        }
                        ArgumentDeclarationList::Variadic(_) => {
                            tast::stmt::ArgumentDeclarationList::Variadic(
                                resolved_parameters.clone(),
                            )
                        }
                    },
                    Box::new(resolved_return_type.clone()),
                ),
            );

            TypedDeclaration::FunctionDeclaration {
                name: name.into_value(),
                parameters: match parameters.value() {
                    ArgumentDeclarationList::NonVariadic(_) => {
                        tast::stmt::ArgumentDeclarationList::NonVariadic(
                            resolved_parameters.clone(),
                        )
                    }
                    ArgumentDeclarationList::Variadic(_) => {
                        tast::stmt::ArgumentDeclarationList::Variadic(resolved_parameters.clone())
                    }
                },
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
            if global_scope.get_type(name.value()).is_some() {
                return Err(Diagnostic(
                    Severity::Error,
                    name.map(|x| DiagnosticKind::IdentifierAlreadyInUse(x.to_string())),
                ));
            }

            let fields = super::ty::resolve_struct_keys(
                global_scope,
                fields.map(|x| x.into_iter().collect::<Vec<_>>()),
            )?;

            global_scope.set_type(name.value(), TastType::Struct(fields.clone()));

            TypedDeclaration::StructDeclaration {
                name: name.into_value(),
                fields,
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
///
/// # Panics
/// Panics in some internal state failures.
// TODO: Maybe the TAST should attach the BlockReturnActuality in each BlockStmt itself and preserve
// it on sub-blocks in the TAST (this may be helpful in control flow analysis)
#[allow(clippy::too_many_lines)]
#[allow(clippy::module_name_repetitions)]
pub fn type_block<'input>(
    parent_scope: &Scope<'input>,
    input_block: Spanned<Vec<Stmt<'input>>>,
    can_use_break_continue: bool,
    return_ability: BlockReturnAbility,
) -> Result<(Vec<TypedStmt<'input>>, BlockReturnActuality), zrc_diagnostics::Diagnostic> {
    let mut scope: Scope<'input> = parent_scope.clone();

    let input_block_span = input_block.span();

    // At first, the block does not return.
    let (tast_block, return_actualities): (Vec<_>, Vec<_>) =
        input_block
            .into_value()
            .into_iter()
            .map(
                |stmt| -> Result<(TypedStmt<'input>, BlockReturnActuality), Diagnostic> {
                    let stmt_span = stmt.0.span();
                    match stmt.0.into_value() {
                        StmtKind::EmptyStmt => {
                            Ok((TypedStmt::EmptyStmt, BlockReturnActuality::DoesNotReturn))
                        }
                        StmtKind::BreakStmt if can_use_break_continue => {
                            Ok((TypedStmt::BreakStmt, BlockReturnActuality::DoesNotReturn))
                        }
                        StmtKind::BreakStmt => Err(Diagnostic(
                            Severity::Error,
                            stmt_span.containing(DiagnosticKind::CannotUseBreakOutsideOfLoop),
                        )),

                        StmtKind::ContinueStmt if can_use_break_continue => {
                            Ok((TypedStmt::BreakStmt, BlockReturnActuality::DoesNotReturn))
                        }
                        StmtKind::ContinueStmt => Err(Diagnostic(
                            Severity::Error,
                            stmt_span.containing(DiagnosticKind::CannotUseContinueOutsideOfLoop),
                        )),

                        StmtKind::DeclarationList(declarations) => Ok((
                            TypedStmt::DeclarationList(process_let_declaration(
                                &mut scope,
                                declarations.clone().into_value(),
                            )?),
                            BlockReturnActuality::DoesNotReturn, /* because expressions can't
                                                                  * return */
                        )),

                        StmtKind::IfStmt(cond, then, then_else) => {
                            // TODO: if `cond` is always true at compile-time, we can prove the if
                            // branch is always taken (hence if it's WillReturn we can be WillReturn
                            // instead of MayReturn)

                            let typed_cond = type_expr(&scope, cond.clone())?;

                            if typed_cond.0 != TastType::Bool {
                                return Err(Diagnostic(
                                    Severity::Error,
                                    cond.0.span().containing(DiagnosticKind::ExpectedGot {
                                        expected: "bool".to_string(),
                                        got: typed_cond.0.to_string(),
                                    }),
                                ));
                            }

                            let (typed_then, then_return_actuality) = type_block(
                                &scope,
                                coerce_stmt_into_block(*then.clone()),
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
                                .clone()
                                .map(|then_else| {
                                    type_block(
                                        &scope,
                                        coerce_stmt_into_block(*then_else),
                                        can_use_break_continue,
                                        // return ability of a sub-block is determined by this
                                        // match:
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
                            // TODO: we might be able to prove that the body runs at least once or
                            // an infinite loop making this won't/will
                            // return statically

                            let typed_cond = type_expr(&scope, cond.clone())?;

                            if typed_cond.0 != TastType::Bool {
                                return Err(Diagnostic(
                                    Severity::Error,
                                    cond.0.span().containing(DiagnosticKind::ExpectedGot {
                                        expected: "bool".to_string(),
                                        got: typed_cond.0.to_string(),
                                    }),
                                ));
                            }

                            let (typed_body, body_return_actuality) = type_block(
                                &scope,
                                coerce_stmt_into_block(*body.clone()),
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

                            // the declaration made in the for loop's init is scoped to *only* the
                            // loop so we need to make a subscope for it
                            let mut loop_scope = scope.clone();

                            // if present, evaluate the declaration
                            let typed_init = init
                                .clone()
                                .map(|decl| {
                                    process_let_declaration(&mut loop_scope, (*decl).into_value())
                                })
                                .transpose()?;

                            let typed_cond = cond
                                .clone()
                                .map(|cond| type_expr(&loop_scope, cond))
                                .transpose()?;

                            if let Some(inner_t_cond) = typed_cond.clone() {
                                if inner_t_cond.0 != TastType::Bool {
                                    return Err(Diagnostic(
                                        Severity::Error,
                                        cond.clone().unwrap().0.span().containing(
                                            DiagnosticKind::ExpectedGot {
                                                expected: "bool".to_string(),
                                                got: inner_t_cond.0.to_string(),
                                            },
                                        ),
                                    ));
                                }
                            }

                            let typed_post = post
                                .clone()
                                .map(|post| type_expr(&loop_scope, post))
                                .transpose()?;

                            let (typed_body, body_return_actuality) = type_block(
                                &loop_scope,
                                coerce_stmt_into_block(*body.clone()),
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
                                body.clone().in_span(stmt_span),
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
                                (_, BlockReturnAbility::MustNotReturn) => Err(Diagnostic(
                                    Severity::Error,
                                    stmt_span.containing(DiagnosticKind::CannotReturnHere),
                                )),

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
                                    BlockReturnAbility::MayReturn(BlockReturnType::Return(
                                        return_ty,
                                    ))
                                    | BlockReturnAbility::MustReturn(BlockReturnType::Return(
                                        return_ty,
                                    )),
                                ) => Err(Diagnostic(
                                    Severity::Error,
                                    stmt_span.containing(DiagnosticKind::ExpectedGot {
                                        expected: return_ty.to_string(),
                                        got: "void".to_string(),
                                    }),
                                )),

                                // return x; in fn expecting to return void
                                (
                                    Some(TypedExpr(ty, _)),
                                    BlockReturnAbility::MustReturn(BlockReturnType::Void)
                                    | BlockReturnAbility::MayReturn(BlockReturnType::Void),
                                ) => Err(Diagnostic(
                                    Severity::Error,
                                    stmt_span.containing(DiagnosticKind::ExpectedGot {
                                        expected: "void".to_string(),
                                        got: ty.to_string(),
                                    }),
                                )),

                                // return x; in fn expecting to return x
                                (
                                    Some(TypedExpr(ty, ex)),
                                    BlockReturnAbility::MustReturn(BlockReturnType::Return(
                                        return_ty,
                                    ))
                                    | BlockReturnAbility::MayReturn(BlockReturnType::Return(
                                        return_ty,
                                    )),
                                ) => {
                                    if ty == return_ty {
                                        Ok((
                                            TypedStmt::ReturnStmt(Some(TypedExpr(ty, ex))),
                                            BlockReturnActuality::WillReturn,
                                        ))
                                    } else {
                                        Err(Diagnostic(
                                            Severity::Error,
                                            value.clone().unwrap().0.span().containing(
                                                DiagnosticKind::ExpectedGot {
                                                    expected: return_ty.to_string(),
                                                    got: ty.to_string(),
                                                },
                                            ),
                                        ))
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
        (BlockReturnAbility::MustReturn(_), BlockReturnActuality::MightReturn) => Err(Diagnostic(
            Severity::Error,
            input_block_span.containing(DiagnosticKind::ExpectedABlockToReturn),
        )),
        (BlockReturnAbility::MustReturn(_), BlockReturnActuality::DoesNotReturn) => {
            Err(Diagnostic(
                Severity::Error,
                input_block_span.containing(DiagnosticKind::ExpectedABlockToReturn),
            ))
        }
        (BlockReturnAbility::MustNotReturn, BlockReturnActuality::MightReturn) => {
            panic!(concat!(
                "block must not return, but a sub-block may return",
                " -- this should have been caught when checking that block"
            ));
        }
        (BlockReturnAbility::MustNotReturn, BlockReturnActuality::WillReturn) => {
            panic!(concat!(
                "block must not return, but a sub-block may return",
                " -- this should have been caught when checking that block"
            ));
        }
    }
}
