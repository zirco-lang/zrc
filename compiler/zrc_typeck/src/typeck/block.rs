//! for blocks

use zrc_diagnostics::{Diagnostic, DiagnosticKind, Severity};
use zrc_parser::ast::stmt::{
    ArgumentDeclarationList, Declaration as AstDeclaration, LetDeclaration as AstLetDeclaration,
    Stmt, StmtKind,
};
use zrc_utils::span::{Span, Spannable, Spanned};

use super::{resolve_type, type_expr, GlobalScope, Scope};
use crate::tast::{
    self,
    expr::TypedExpr,
    stmt::{
        ArgumentDeclaration as TastArgumentDeclaration, LetDeclaration as TastLetDeclaration,
        TypedDeclaration, TypedStmt, TypedStmtKind,
    },
    ty::{Fn, Type as TastType},
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
    let span = stmt.0.span();

    #[allow(clippy::wildcard_enum_match_arm)]
    stmt.0.map(|value| match value {
        StmtKind::BlockStmt(stmts) => stmts,
        stmt_kind => vec![Stmt(stmt_kind.in_span(span))],
    })
}

/// Process a vector of [AST let declarations](AstLetDeclaration) and insert it
/// into the scope, returning a vector of [TAST let
/// declarations](TastLetDeclaration).
///
/// # Errors
/// Errors with type checker errors.
fn process_let_declaration<'input>(
    scope: &mut Scope<'input, '_>,
    declarations: Vec<Spanned<AstLetDeclaration<'input>>>,
) -> Result<Vec<Spanned<TastLetDeclaration<'input>>>, Diagnostic> {
    declarations
        .into_iter()
        .map(
            |let_declaration| -> Result<Spanned<TastLetDeclaration>, Diagnostic> {
                let let_decl_span = let_declaration.span();
                let let_declaration = let_declaration.into_value();

                if scope.values.has(let_declaration.name.value()) {
                    // TODO: In the future we may allow shadowing but currently no
                    return Err(Diagnostic(
                        Severity::Error,
                        let_declaration
                            .name
                            .map(|x| DiagnosticKind::IdentifierAlreadyInUse(x.to_string())),
                    ));
                }

                let typed_expr = let_declaration
                    .value
                    .map(|expr| type_expr(scope, expr))
                    .transpose()?;

                let resolved_ty = let_declaration
                    .ty
                    .map(|ty| resolve_type(scope.types, ty))
                    .transpose()?;

                let result_decl = match (typed_expr, resolved_ty) {
                    (None, None) => {
                        return Err(Diagnostic(
                            Severity::Error,
                            let_decl_span.containing(DiagnosticKind::NoTypeNoValue),
                        ));
                    }

                    // Explicitly typed with no value
                    (None, Some(ty)) => TastLetDeclaration {
                        name: let_declaration.name,
                        ty,
                        value: None,
                    },

                    // Infer type from value
                    (
                        Some(TypedExpr {
                            inferred_type,
                            kind,
                        }),
                        None,
                    ) => TastLetDeclaration {
                        name: let_declaration.name,
                        ty: inferred_type.clone(),
                        value: Some(TypedExpr {
                            inferred_type,
                            kind,
                        }),
                    },

                    // Both explicitly typed and inferable
                    (
                        Some(TypedExpr {
                            inferred_type,
                            kind,
                        }),
                        Some(resolved_ty),
                    ) => {
                        if inferred_type == resolved_ty {
                            TastLetDeclaration {
                                name: let_declaration.name,
                                ty: inferred_type.clone(),
                                value: Some(TypedExpr {
                                    inferred_type,
                                    kind,
                                }),
                            }
                        } else {
                            return Err(Diagnostic(
                                Severity::Error,
                                let_decl_span.containing(
                                    DiagnosticKind::InvalidAssignmentRightHandSideType {
                                        expected: resolved_ty.to_string(),
                                        got: inferred_type.to_string(),
                                    },
                                ),
                            ));
                        }
                    }
                };

                if result_decl.ty == TastType::Void {
                    return Err(Diagnostic(
                        Severity::Error,
                        let_decl_span.containing(DiagnosticKind::CannotDeclareVoid),
                    ));
                }

                scope
                    .values
                    .insert(result_decl.name.value(), result_decl.ty.clone());
                Ok(result_decl.in_span(let_decl_span))
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
#[allow(clippy::too_many_lines, clippy::missing_panics_doc)]
pub fn process_declaration<'input>(
    global_scope: &mut GlobalScope<'input>,
    declaration: AstDeclaration<'input>,
) -> Result<Option<TypedDeclaration<'input>>, Diagnostic> {
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
            let resolved_return_type = return_type
                .clone()
                .map(|ty| resolve_type(&global_scope.types, ty))
                .transpose()?
                .map_or(BlockReturnType::Void, BlockReturnType::Return);

            let (ArgumentDeclarationList::NonVariadic(inner_params)
            | ArgumentDeclarationList::Variadic(inner_params)) = parameters.value();

            let resolved_parameters = inner_params
                .iter()
                .map(|parameter| -> Result<TastArgumentDeclaration, Diagnostic> {
                    Ok(TastArgumentDeclaration {
                        name: parameter.value().name,
                        ty: resolve_type(&global_scope.types, parameter.value().ty.clone())?
                            .in_span(parameter.span()),
                    })
                })
                .collect::<Result<Vec<_>, Diagnostic>>()?;

            let fn_type = Fn {
                arguments: match parameters.value() {
                    ArgumentDeclarationList::NonVariadic(_) => {
                        tast::stmt::ArgumentDeclarationList::NonVariadic(
                            resolved_parameters.clone(),
                        )
                    }
                    ArgumentDeclarationList::Variadic(_) => {
                        tast::stmt::ArgumentDeclarationList::Variadic(resolved_parameters.clone())
                    }
                },
                returns: Box::new(resolved_return_type.clone()),
            };

            let has_existing_implementation = if let Some(ty) =
                global_scope.global_values.resolve(name.value())
            {
                if let TastType::Fn(_) = ty {
                    // if a function has already been declared with this name...

                    let canonical = global_scope.declarations.get(name.value()).expect(
                        "global_scope.declarations was not populated with function properly",
                    );

                    // TODO: store and reference previous declaration's span in the error
                    if canonical.fn_type != fn_type {
                        return Err(Diagnostic(
                            Severity::Error,
                            name.map(|_| {
                                DiagnosticKind::ConflictingFunctionDeclarations(
                                    canonical.fn_type.to_string(),
                                    fn_type.to_string(),
                                )
                            }),
                        ));
                    }

                    // TODO: store and reference previous declaration's span in the error
                    if body.is_some() && canonical.has_implementation {
                        return Err(Diagnostic(
                            Severity::Error,
                            name.map(|x| DiagnosticKind::ConflictingImplementations(x.to_string())),
                        ));
                    }

                    canonical.has_implementation
                } else {
                    return Err(Diagnostic(
                        Severity::Error,
                        name.map(|x| DiagnosticKind::IdentifierAlreadyInUse(x.to_string())),
                    ));
                }
            } else {
                false
            };

            global_scope
                .global_values
                .insert(name.into_value(), TastType::Fn(fn_type.clone()));

            global_scope.declarations.insert(
                name.into_value(),
                super::FunctionDeclarationGlobalMetadata {
                    fn_type,
                    has_implementation: body.is_some() || has_existing_implementation,
                },
            );

            Some(TypedDeclaration::FunctionDeclaration {
                name,
                parameters: match parameters.value() {
                    ArgumentDeclarationList::NonVariadic(_) => {
                        tast::stmt::ArgumentDeclarationList::NonVariadic(
                            resolved_parameters.clone(),
                        )
                    }
                    ArgumentDeclarationList::Variadic(_) => {
                        tast::stmt::ArgumentDeclarationList::Variadic(resolved_parameters.clone())
                    }
                }
                .in_span(parameters.span()),
                return_type: resolved_return_type
                    .clone()
                    .into_option()
                    .map(|existing_type| {
                        existing_type
                            .in_span(return_type.expect("already unwrapped before").0.span())
                    }),
                body: if let Some(body) = body {
                    let mut function_scope = global_scope.create_subscope();
                    for param in resolved_parameters {
                        function_scope
                            .values
                            .insert(param.name.value(), param.ty.into_value());
                    }

                    // discard return actuality as it's guaranteed
                    Some(
                        body.span().containing(
                            type_block(
                                &function_scope,
                                body,
                                false,
                                BlockReturnAbility::MustReturn(resolved_return_type),
                            )?
                            .0,
                        ),
                    )
                } else {
                    None
                },
            })
        }
        AstDeclaration::TypeAliasDeclaration { name, ty } => {
            if global_scope.types.has(name.value()) {
                return Err(Diagnostic(
                    Severity::Error,
                    name.map(|x| DiagnosticKind::IdentifierAlreadyInUse(x.to_string())),
                ));
            }

            let resolved_ty = resolve_type(&global_scope.types, ty)?;

            global_scope.types.insert(name.value(), resolved_ty.clone());

            None
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
pub fn type_block<'input, 'gs>(
    parent_scope: &Scope<'input, 'gs>,
    input_block: Spanned<Vec<Stmt<'input>>>,
    can_use_break_continue: bool,
    return_ability: BlockReturnAbility,
) -> Result<(Vec<TypedStmt<'input>>, BlockReturnActuality), Diagnostic> {
    let mut scope: Scope<'input, 'gs> = parent_scope.clone();

    let input_block_span = input_block.span();

    // At first, the block does not return.
    let (mut tast_block, return_actualities): (Vec<_>, Vec<_>) = input_block
        .into_value()
        .into_iter()
        .filter_map(
            |stmt| -> Option<Result<(TypedStmt<'input>, BlockReturnActuality), Diagnostic>> {
                let stmt_span = stmt.0.span();
                let inner_closure =
                    || -> Result<Option<(TypedStmt<'_>, BlockReturnActuality)>, Diagnostic> {
                        match stmt.0.into_value() {
                            StmtKind::EmptyStmt => Ok(None),
                            StmtKind::BreakStmt if can_use_break_continue => Ok(Some((
                                TypedStmt(TypedStmtKind::BreakStmt.in_span(stmt_span)),
                                BlockReturnActuality::DoesNotReturn,
                            ))),
                            StmtKind::BreakStmt => Err(Diagnostic(
                                Severity::Error,
                                stmt_span.containing(DiagnosticKind::CannotUseBreakOutsideOfLoop),
                            )),

                            StmtKind::ContinueStmt if can_use_break_continue => Ok(Some((
                                TypedStmt(TypedStmtKind::ContinueStmt.in_span(stmt_span)),
                                BlockReturnActuality::DoesNotReturn,
                            ))),
                            StmtKind::ContinueStmt => Err(Diagnostic(
                                Severity::Error,
                                stmt_span
                                    .containing(DiagnosticKind::CannotUseContinueOutsideOfLoop),
                            )),

                            StmtKind::DeclarationList(declarations) => Ok(Some((
                                TypedStmt(
                                    TypedStmtKind::DeclarationList(process_let_declaration(
                                        &mut scope,
                                        declarations.clone().into_value(),
                                    )?)
                                    .in_span(stmt_span),
                                ),
                                BlockReturnActuality::DoesNotReturn, /* because expressions
                                                                      * can't
                                                                      * return */
                            ))),

                            StmtKind::IfStmt(cond, then, then_else) => {
                                // TODO: if `cond` is always true at compile-time, we can prove the
                                // if branch is always taken (hence
                                // if it's WillReturn we can be WillReturn
                                // instead of MayReturn)

                                let cond_span = cond.0.span();
                                let then_span = then.0.span();
                                let te_span = then_else.as_ref().map(|x| x.0.span());

                                let typed_cond = type_expr(&scope, cond)?;

                                if typed_cond.inferred_type != TastType::Bool {
                                    return Err(Diagnostic(
                                        Severity::Error,
                                        cond_span.containing(DiagnosticKind::ExpectedGot {
                                            expected: "bool".to_string(),
                                            got: typed_cond.inferred_type.to_string(),
                                        }),
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

                                Ok(Some((
                                    TypedStmt(
                                        TypedStmtKind::IfStmt(
                                            typed_cond,
                                            typed_then.in_span(then_span),
                                            typed_then_else.map(|x| {
                                                x.in_span(
                                                    te_span.expect(
                                                        "should have been unwrapped already",
                                                    ),
                                                )
                                            }),
                                        )
                                        .in_span(stmt_span),
                                    ),
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
                                )))
                            }
                            StmtKind::WhileStmt(cond, body) => {
                                // TODO: we might be able to prove that the body runs at least once
                                // or an infinite loop making this
                                // won't/will return statically

                                let cond_span = cond.0.span();
                                let body_span = body.0.span();
                                let typed_cond = type_expr(&scope, cond)?;

                                if typed_cond.inferred_type != TastType::Bool {
                                    return Err(Diagnostic(
                                        Severity::Error,
                                        cond_span.containing(DiagnosticKind::ExpectedGot {
                                            expected: "bool".to_string(),
                                            got: typed_cond.inferred_type.to_string(),
                                        }),
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

                                Ok(Some((
                                    TypedStmt(
                                        TypedStmtKind::WhileStmt(
                                            typed_cond,
                                            typed_body.in_span(body_span),
                                        )
                                        .in_span(stmt_span),
                                    ),
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
                                )))
                            }
                            StmtKind::DoWhileStmt(body, cond) => {
                                let cond_span = cond.0.span();
                                let body_span = body.0.span();
                                let typed_cond = type_expr(&scope, cond)?;

                                if typed_cond.inferred_type != TastType::Bool {
                                    return Err(Diagnostic(
                                        Severity::Error,
                                        cond_span.containing(DiagnosticKind::ExpectedGot {
                                            expected: "bool".to_string(),
                                            got: typed_cond.inferred_type.to_string(),
                                        }),
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

                                Ok(Some((
                                    TypedStmt(
                                        TypedStmtKind::DoWhileStmt(
                                            typed_body.in_span(body_span),
                                            typed_cond,
                                        )
                                        .in_span(stmt_span),
                                    ),
                                    match body_return_actuality {
                                        BlockReturnActuality::DoesNotReturn => {
                                            BlockReturnActuality::DoesNotReturn
                                        }

                                        // In a `do..while` loop, there is a GUARENTEE that the
                                        // body will run at least once. For this reason,
                                        // we map WillReturn to WillReturn unlike `for` and `while`.
                                        BlockReturnActuality::MightReturn => {
                                            BlockReturnActuality::MightReturn
                                        }
                                        BlockReturnActuality::WillReturn => {
                                            BlockReturnActuality::WillReturn
                                        }
                                    },
                                )))
                            }
                            StmtKind::ForStmt {
                                init,
                                cond,
                                post,
                                body,
                            } => {
                                // TODO: same logic as the TODO comment on the while loop applies
                                // here.

                                // the declaration made in the for loop's init is scoped to *only*
                                // the loop so we need to make a
                                // subscope for it
                                let mut loop_scope = scope.clone();

                                // if present, evaluate the declaration
                                let typed_init = init
                                    .map(|decl| {
                                        process_let_declaration(
                                            &mut loop_scope,
                                            (*decl).into_value(),
                                        )
                                    })
                                    .transpose()?;

                                let cond_span = cond.as_ref().map(|inner| inner.0.span());
                                let typed_cond =
                                    cond.map(|cond| type_expr(&loop_scope, cond)).transpose()?;

                                if let Some(inner_t_cond) = typed_cond.clone() {
                                    if inner_t_cond.inferred_type != TastType::Bool {
                                        return Err(Diagnostic(
                                            Severity::Error,
                                            cond_span
                                                .expect("span should exist if we unwrapped it")
                                                .containing(DiagnosticKind::ExpectedGot {
                                                    expected: "bool".to_string(),
                                                    got: inner_t_cond.inferred_type.to_string(),
                                                }),
                                        ));
                                    }
                                }

                                let typed_post =
                                    post.map(|post| type_expr(&loop_scope, post)).transpose()?;

                                let body_as_block = coerce_stmt_into_block(*body);
                                let body_as_block_span = body_as_block.span();
                                let (typed_body, body_return_actuality) = type_block(
                                    &loop_scope,
                                    body_as_block,
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

                                Ok(Some((
                                    TypedStmt(
                                        TypedStmtKind::ForStmt {
                                            init: typed_init.map(Box::new),
                                            cond: typed_cond,
                                            post: typed_post,
                                            body: typed_body.in_span(body_as_block_span),
                                        }
                                        .in_span(stmt_span),
                                    ),
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
                                )))
                            }

                            StmtKind::BlockStmt(body) => {
                                let (typed_body, return_actuality) = type_block(
                                    &scope,
                                    body.in_span(stmt_span),
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
                                Ok(Some((
                                    TypedStmt(
                                        TypedStmtKind::BlockStmt(typed_body).in_span(stmt_span),
                                    ),
                                    return_actuality,
                                )))
                            }

                            StmtKind::ExprStmt(expr) => Ok(Some((
                                TypedStmt(
                                    TypedStmtKind::ExprStmt(type_expr(&scope, expr)?)
                                        .in_span(stmt_span),
                                ),
                                BlockReturnActuality::DoesNotReturn,
                            ))),
                            StmtKind::ReturnStmt(value) => {
                                let value_span = value.as_ref().map(|inner| inner.0.span());
                                let resolved_value =
                                    value.map(|expr| type_expr(&scope, expr)).transpose()?;
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
                                    ) => Ok(Some((
                                        TypedStmt(
                                            TypedStmtKind::ReturnStmt(None).in_span(stmt_span),
                                        ),
                                        BlockReturnActuality::WillReturn,
                                    ))),

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
                                        Some(TypedExpr { inferred_type, .. }),
                                        BlockReturnAbility::MustReturn(BlockReturnType::Void)
                                        | BlockReturnAbility::MayReturn(BlockReturnType::Void),
                                    ) => Err(Diagnostic(
                                        Severity::Error,
                                        stmt_span.containing(DiagnosticKind::ExpectedGot {
                                            expected: "void".to_string(),
                                            got: inferred_type.to_string(),
                                        }),
                                    )),

                                    // return x; in fn expecting to return x
                                    (
                                        Some(TypedExpr {
                                            inferred_type,
                                            kind,
                                        }),
                                        BlockReturnAbility::MustReturn(BlockReturnType::Return(
                                            return_ty,
                                        ))
                                        | BlockReturnAbility::MayReturn(BlockReturnType::Return(
                                            return_ty,
                                        )),
                                    ) => {
                                        if inferred_type == return_ty {
                                            Ok(Some((
                                                TypedStmt(
                                                    TypedStmtKind::ReturnStmt(Some(TypedExpr {
                                                        inferred_type,
                                                        kind,
                                                    }))
                                                    .in_span(stmt_span),
                                                ),
                                                BlockReturnActuality::WillReturn,
                                            )))
                                        } else {
                                            Err(Diagnostic(
                                                Severity::Error,
                                                value_span
                                                    .expect("value should exist if we unwrapped it")
                                                    .containing(DiagnosticKind::ExpectedGot {
                                                        expected: return_ty.to_string(),
                                                        got: inferred_type.to_string(),
                                                    }),
                                            ))
                                        }
                                    }
                                }
                            }
                        }
                    };

                inner_closure().transpose()
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
        (
            BlockReturnAbility::MustReturn(BlockReturnType::Void),
            BlockReturnActuality::MightReturn | BlockReturnActuality::DoesNotReturn,
        ) => {
            tast_block.push(TypedStmt(TypedStmtKind::ReturnStmt(None).in_span(
                Span::from_positions(input_block_span.end() - 1, input_block_span.end()),
            )));

            Ok((tast_block, BlockReturnActuality::WillReturn))
        }
        (
            BlockReturnAbility::MustReturn(_),
            BlockReturnActuality::MightReturn | BlockReturnActuality::DoesNotReturn,
        ) => Err(Diagnostic(
            Severity::Error,
            input_block_span.containing(DiagnosticKind::ExpectedABlockToReturn),
        )),
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

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use tast::stmt::ArgumentDeclarationList as TastArgumentDeclarationList;
    use zrc_parser::ast::{
        expr::{Expr, ExprKind},
        stmt::ArgumentDeclarationList as AstArgumentDeclarationList,
        ty::{Type, TypeKind},
    };
    use zrc_utils::spanned;

    use super::*;
    use crate::typeck::{FunctionDeclarationGlobalMetadata, TypeScope, ValueScope};

    #[test]
    fn re_declaration_works_as_expected() {
        assert!(process_declaration(
            &mut GlobalScope {
                global_values: ValueScope::from([(
                    "get_true",
                    TastType::Fn(Fn {
                        arguments: TastArgumentDeclarationList::NonVariadic(vec![]),
                        returns: Box::new(BlockReturnType::Return(TastType::Bool))
                    })
                )]),
                types: TypeScope::from([("bool", TastType::Bool)]),
                declarations: HashMap::from([(
                    "get_true",
                    FunctionDeclarationGlobalMetadata {
                        fn_type: Fn {
                            arguments: TastArgumentDeclarationList::NonVariadic(vec![]),
                            returns: Box::new(BlockReturnType::Return(TastType::Bool))
                        },
                        has_implementation: false
                    }
                )])
            },
            AstDeclaration::FunctionDeclaration {
                name: spanned!(0, "get_true", 0),
                parameters: spanned!(0, AstArgumentDeclarationList::NonVariadic(vec![]), 0),
                return_type: Some(Type(spanned!(0, TypeKind::Identifier("bool"), 0))),
                body: Some(spanned!(
                    0,
                    vec![Stmt(spanned!(
                        0,
                        StmtKind::ReturnStmt(Some(Expr(spanned!(
                            0,
                            ExprKind::BooleanLiteral(true),
                            0
                        )))),
                        0
                    ))],
                    0
                ))
            }
        )
        .is_ok());
    }
}
