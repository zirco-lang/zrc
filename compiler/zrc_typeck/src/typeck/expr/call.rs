//! type checking for call expressions

use zrc_diagnostics::{Diagnostic, DiagnosticKind, SpanExt};
use zrc_parser::ast::expr::Expr;
use zrc_utils::span::{Span, Spannable, Spanned};

use super::{
    super::scope::Scope,
    helpers::{expr_to_place, try_coerce_to},
    type_expr,
};
use crate::tast::{
    expr::{TypedExpr, TypedExprKind},
    stmt::ArgumentDeclarationList,
    ty::{Fn, Type as TastType},
};

/// Typeck a call expr
#[allow(clippy::needless_pass_by_value, clippy::too_many_lines)]
pub fn type_expr_call<'input>(
    scope: &Scope<'input, '_>,
    expr_span: Span,
    f: Expr<'input>,
    args: Spanned<Vec<Expr<'input>>>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let f_span = f.0.span();
    let ft = type_expr(scope, f)?;
    let args_t = args
        .value()
        .iter()
        .map(|x| type_expr(scope, x.clone()))
        .collect::<Result<Vec<TypedExpr>, Diagnostic>>()?;

    #[allow(clippy::wildcard_enum_match_arm)]
    match ft.inferred_type.clone() {
        TastType::Fn(Fn {
            arguments: ArgumentDeclarationList::NonVariadic(arg_types),
            returns: ret_type,
        }) => {
            if arg_types.len() != args_t.len() {
                return Err(DiagnosticKind::FunctionArgumentCountMismatch {
                    expected: arg_types.len().to_string(),
                    got: args_t.len().to_string(),
                }
                .error_in(expr_span));
            }

            for (i, (arg_type, arg_t)) in arg_types.iter().zip(args_t.iter()).enumerate() {
                let expected = arg_type.ty.value();
                let got = &arg_t.inferred_type;
                if *expected != *got && !got.can_implicitly_cast_to(expected) {
                    return Err(args.value()[i].0.span().error(
                        DiagnosticKind::FunctionArgumentTypeMismatch {
                            n: i,
                            expected: arg_type.ty.to_string(),
                            got: arg_t.inferred_type.to_string(),
                        },
                    ));
                }
            }

            // Insert implicit casts where needed
            let args_with_casts = arg_types
                .iter()
                .zip(args_t)
                .map(|(arg_type, arg_t)| {
                    if arg_t.inferred_type != *arg_type.ty.value()
                        && arg_t
                            .inferred_type
                            .can_implicitly_cast_to(arg_type.ty.value())
                    {
                        // Try to coerce the argument to the parameter type
                        try_coerce_to(arg_t, arg_type.ty.value())
                    } else {
                        arg_t
                    }
                })
                .collect();

            Ok(TypedExpr {
                inferred_type: *ret_type,
                kind: TypedExprKind::Call(Box::new(expr_to_place(f_span, ft)?), args_with_casts)
                    .in_span(expr_span),
            })
        }
        TastType::Fn(Fn {
            arguments: ArgumentDeclarationList::Variadic(beginning_arg_types),
            returns: ret_type,
        }) => {
            if beginning_arg_types.len() > args_t.len() {
                return Err(DiagnosticKind::FunctionArgumentCountMismatch {
                    expected: format!("at least {}", beginning_arg_types.len()),
                    got: args_t.len().to_string(),
                }
                .error_in(expr_span));
            }

            for (i, (arg_type, arg_t)) in beginning_arg_types.iter().zip(args_t.iter()).enumerate()
            {
                let expected = arg_type.ty.value();
                let got = &arg_t.inferred_type;
                if *expected != *got && !got.can_implicitly_cast_to(expected) {
                    return Err(args.value()[i].0.span().error(
                        DiagnosticKind::FunctionArgumentTypeMismatch {
                            n: i,
                            expected: arg_type.ty.to_string(),
                            got: arg_t.inferred_type.to_string(),
                        },
                    ));
                }
            }

            // Insert implicit casts where needed for non-variadic arguments
            let mut args_with_casts = Vec::new();
            for (arg_type, arg_t) in beginning_arg_types.iter().zip(args_t.iter()) {
                if arg_t.inferred_type != *arg_type.ty.value()
                    && arg_t
                        .inferred_type
                        .can_implicitly_cast_to(arg_type.ty.value())
                {
                    // Try to coerce the argument to the parameter type
                    args_with_casts.push(try_coerce_to(arg_t.clone(), arg_type.ty.value()));
                } else {
                    args_with_casts.push(arg_t.clone());
                }
            }
            // Add the variadic arguments as-is
            args_with_casts.extend(args_t.into_iter().skip(beginning_arg_types.len()));

            // the rest may be any, so we don't need to check them
            Ok(TypedExpr {
                inferred_type: *ret_type,
                kind: TypedExprKind::Call(Box::new(expr_to_place(f_span, ft)?), args_with_casts)
                    .in_span(expr_span),
            })
        }
        _ => Err(
            DiagnosticKind::CannotCallNonFunction(ft.inferred_type.to_string()).error_in(expr_span),
        ),
    }
}
