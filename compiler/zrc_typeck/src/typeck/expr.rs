//! for expressions

use zrc_diagnostics::{Diagnostic, DiagnosticKind, Severity, Spanned as DiagnosticSpan};
use zrc_parser::ast::{
    expr::{Assignment, Expr, ExprKind},
    Spanned,
};

use super::Scope;
use crate::{
    tast::{
        expr::{Place, PlaceKind, TypedExpr, TypedExprKind},
        ty::Type as TastType,
    },
    typeck::resolve_type,
};

/// Desugar an assignment like `x += y` to `x = x + y`.
fn desugar_assignment(mode: Assignment, lhs: Expr, rhs: Expr) -> (Expr, Expr) {
    match mode {
        Assignment::Standard => (lhs, rhs),
        Assignment::Arithmetic(op) => (
            lhs.clone(),
            Expr(Spanned(
                rhs.0 .0,
                ExprKind::Arithmetic(op, Box::new(lhs), Box::new(rhs.clone())),
                rhs.0 .2,
            )),
        ),
        Assignment::BinaryBitwise(op) => (
            lhs.clone(),
            Expr(Spanned(
                rhs.0 .0,
                ExprKind::BinaryBitwise(op, Box::new(lhs), Box::new(rhs.clone())),
                rhs.0 .2,
            )),
        ),
    }
}

/// Validate an expr into a place
fn expr_to_place(
    span: (usize, usize),
    expr: TypedExpr,
) -> Result<Place, zrc_diagnostics::Diagnostic> {
    Ok(match expr.1 {
        TypedExprKind::UnaryDereference(x) => Place(expr.0, PlaceKind::Deref(x)),
        TypedExprKind::Identifier(x) => Place(expr.0, PlaceKind::Variable(x)),
        TypedExprKind::Index(x, y) => Place(expr.0, PlaceKind::Index(x, y)),
        TypedExprKind::Dot(x, y) => Place(
            expr.0,
            PlaceKind::Dot(Box::new(expr_to_place(span, *x)?), y),
        ),
        TypedExprKind::Arrow(x, y) => Place(
            expr.0,
            PlaceKind::Arrow(Box::new(expr_to_place(span, *x)?), y),
        ),
        _ => {
            return Err(zrc_diagnostics::Diagnostic(
                zrc_diagnostics::Severity::Error,
                zrc_diagnostics::Spanned(
                    span.0,
                    zrc_diagnostics::DiagnosticKind::AssignmentToNonPlace(expr.to_string()),
                    span.1,
                ),
            ))
        }
    })
}

// FIXME: this NEEDS to be rewritten to use references almost everywhere and be
// no-clone. We stack overflow for deep expressions which is VERY VERY BAD.
/// Type check and infer an [AST expression](Expr) to a [TAST
/// expression](TypedExpr).
///
/// # Errors
/// Errors if a type checker error is encountered.
#[allow(clippy::too_many_lines)] // FIXME: make this fn shorter
#[allow(clippy::module_name_repetitions)]
pub fn type_expr(scope: &Scope, expr: Expr) -> Result<TypedExpr, zrc_diagnostics::Diagnostic> {
    Ok(match expr.0 .1 {
        ExprKind::Comma(a, b) => {
            let at = type_expr(scope, *a)?;
            let bt = type_expr(scope, *b)?;
            TypedExpr(
                bt.clone().0,
                TypedExprKind::Comma(Box::new(at), Box::new(bt)),
            )
        }
        ExprKind::Assignment(mode, place, value) => {
            // Desugar `x += y` to `x = x + y`.
            let (place, value) = desugar_assignment(mode, *place, *value);

            let place_t = expr_to_place((expr.0 .0, expr.0 .2), type_expr(scope, place)?)?;
            let value_t = type_expr(scope, value)?;

            if place_t.0 != value_t.0 {
                return Err(zrc_diagnostics::Diagnostic(
                    zrc_diagnostics::Severity::Error,
                    zrc_diagnostics::Spanned(
                        expr.0 .0,
                        zrc_diagnostics::DiagnosticKind::InvalidAssignmentRightHandSideType {
                            expected: place_t.0.to_string(),
                            got: value_t.0.to_string(),
                        },
                        expr.0 .2,
                    ),
                ));
            }

            TypedExpr(
                place_t.clone().0,
                TypedExprKind::Assignment(Box::new(place_t), Box::new(value_t)),
            )
        }

        ExprKind::UnaryNot(x) => {
            let t = type_expr(scope, *x)?;
            if t.0 != TastType::Bool {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        expr.0 .0,
                        DiagnosticKind::UnaryNotExpectedBoolean(t.0.to_string()),
                        expr.0 .2,
                    ),
                ));
            }
            TypedExpr(t.0.clone(), TypedExprKind::UnaryNot(Box::new(t)))
        }
        ExprKind::UnaryBitwiseNot(x) => {
            let t = type_expr(scope, *x)?;
            if !t.0.is_integer() {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        expr.0 .0,
                        DiagnosticKind::UnaryBitwiseNotExpectedInteger(t.0.to_string()),
                        expr.0 .2,
                    ),
                ));
            }
            TypedExpr(t.0.clone(), TypedExprKind::UnaryBitwiseNot(Box::new(t)))
        }
        ExprKind::UnaryMinus(x) => {
            let t = type_expr(scope, *x)?;
            if !t.0.is_signed_integer() {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        expr.0 .0,
                        DiagnosticKind::UnaryMinusExpectedSignedInteger(t.0.to_string()),
                        expr.0 .2,
                    ),
                ));
            }
            TypedExpr(t.0.clone(), TypedExprKind::UnaryMinus(Box::new(t)))
        }
        ExprKind::UnaryAddressOf(x) => {
            let t = type_expr(scope, *x)?;
            TypedExpr(
                TastType::Ptr(Box::new(t.0.clone())),
                TypedExprKind::UnaryAddressOf(Box::new(t)),
            )
        }
        ExprKind::UnaryDereference(x) => {
            let t = type_expr(scope, *x)?;
            if let TastType::Ptr(tt) = t.clone().0 {
                TypedExpr(*tt, TypedExprKind::UnaryDereference(Box::new(t)))
            } else {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        expr.0 .0,
                        DiagnosticKind::CannotDereferenceNonPointer(t.0.to_string()),
                        expr.0 .2,
                    ),
                ));
            }
        }

        ExprKind::Index(ptr, offset) => {
            let ptr_t = type_expr(scope, *ptr)?;
            let offset_t = type_expr(scope, *offset)?;

            if !offset_t.0.is_integer() {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        expr.0 .0,
                        DiagnosticKind::IndexOffsetMustBeInteger(offset_t.0.to_string()),
                        expr.0 .2,
                    ),
                ));
            }

            if let TastType::Ptr(t) = ptr_t.0.clone() {
                TypedExpr(
                    *t,
                    TypedExprKind::Index(Box::new(ptr_t), Box::new(offset_t)),
                )
            } else {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        expr.0 .0,
                        DiagnosticKind::CannotIndexIntoNonPointer(ptr_t.0.to_string()),
                        expr.0 .2,
                    ),
                ));
            }
        }

        ExprKind::Dot(obj, key) => {
            let obj_t = type_expr(scope, *obj)?;

            if let TastType::Struct(fields) = obj_t.0.clone() {
                if let Some(t) = fields.get(&key.1) {
                    TypedExpr(
                        t.clone(),
                        TypedExprKind::Dot(Box::new(obj_t), key.1.clone()),
                    )
                } else {
                    return Err(Diagnostic(
                        Severity::Error,
                        DiagnosticSpan(
                            expr.0 .0,
                            DiagnosticKind::StructDoesNotHaveMember(obj_t.to_string(), key.1),
                            expr.0 .2,
                        ),
                    ));
                }
            } else {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        expr.0 .0,
                        DiagnosticKind::StructMemberAccessOnNonStruct(obj_t.to_string()),
                        expr.0 .2,
                    ),
                ));
            }
        }
        ExprKind::Arrow(obj, key) => {
            let obj_t = type_expr(scope, *obj.clone())?;

            if let TastType::Ptr(_) = obj_t.0 {
                type_expr(
                    scope,
                    Expr(Spanned(
                        obj.0 .0,
                        ExprKind::Dot(
                            Box::new(Expr(Spanned(
                                (*obj).0 .0,
                                ExprKind::UnaryDereference(obj.clone()),
                                (*obj).0 .2,
                            ))),
                            key.clone(),
                        ),
                        // because 'expr' is already moved
                        key.2,
                    )),
                )?
            } else {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        expr.0 .0,
                        DiagnosticKind::CannotDereferenceNonPointer(obj_t.to_string()),
                        expr.0 .2,
                    ),
                ));
            }
        }
        ExprKind::Call(f, args) => {
            let ft = type_expr(scope, *f)?;
            let args_t = args
                .1
                .iter()
                .map(|x| type_expr(scope, x.clone()))
                .collect::<Result<Vec<TypedExpr>, Diagnostic>>()?;

            if let TastType::Fn(arg_types, ret_type) = ft.0.clone() {
                if arg_types.len() != args_t.len() {
                    return Err(Diagnostic(
                        Severity::Error,
                        DiagnosticSpan(
                            expr.0 .0,
                            DiagnosticKind::FunctionArgumentCountMismatch {
                                expected: arg_types.len(),
                                got: args_t.len(),
                            },
                            expr.0 .2,
                        ),
                    ));
                }

                for (i, (arg_type, arg_t)) in arg_types.iter().zip(args_t.iter()).enumerate() {
                    if arg_type != &arg_t.0 {
                        return Err(Diagnostic(
                            Severity::Error,
                            DiagnosticSpan(
                                args.1[i].0 .0,
                                DiagnosticKind::FunctionArgumentTypeMismatch {
                                    n: i,
                                    expected: arg_type.to_string(),
                                    got: arg_t.0.to_string(),
                                },
                                args.1[i].0 .2,
                            ),
                        ));
                    }
                }

                TypedExpr(
                    ret_type.into_option().unwrap_or(TastType::Void),
                    TypedExprKind::Call(Box::new(ft), args_t),
                )
            } else {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        expr.0 .0,
                        DiagnosticKind::CannotCallNonFunction(ft.to_string()),
                        expr.0 .2,
                    ),
                ));
            }
        }

        ExprKind::Ternary(cond, if_true, if_false) => {
            let cond_t = type_expr(scope, *cond.clone())?;
            let if_true_t = type_expr(scope, *if_true)?;
            let if_false_t = type_expr(scope, *if_false)?;

            if cond_t.0 != TastType::Bool {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        cond.0 .0,
                        DiagnosticKind::TernaryConditionMustBeBoolean(cond_t.to_string()),
                        cond.0 .2,
                    ),
                ));
            }

            if if_true_t.0 != if_false_t.0 {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        expr.0 .0,
                        DiagnosticKind::TernaryArmsMustHaveSameType(
                            if_true_t.to_string(),
                            if_false_t.to_string(),
                        ),
                        expr.0 .2,
                    ),
                ));
            }

            TypedExpr(
                if_true_t.0.clone(),
                TypedExprKind::Ternary(Box::new(cond_t), Box::new(if_true_t), Box::new(if_false_t)),
            )
        }

        ExprKind::Logical(op, a, b) => {
            let at = type_expr(scope, *a.clone())?;
            let bt = type_expr(scope, *b.clone())?;

            if at.0 != TastType::Bool {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        a.0 .0,
                        DiagnosticKind::ExpectedGot {
                            expected: "bool".to_string(),
                            got: at.0.to_string(),
                        },
                        a.0 .2,
                    ),
                ));
            }

            if bt.0 != TastType::Bool {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        b.0 .0,
                        DiagnosticKind::ExpectedGot {
                            expected: "bool".to_string(),
                            got: bt.0.to_string(),
                        },
                        b.0 .2,
                    ),
                ));
            }

            TypedExpr(
                TastType::Bool,
                TypedExprKind::Logical(op, Box::new(at), Box::new(bt)),
            )
        }
        ExprKind::Equality(op, a, b) => {
            let at = type_expr(scope, *a)?;
            let bt = type_expr(scope, *b)?;

            if at.0.is_integer() && bt.0.is_integer() && at.0 == bt.0 {
                // int == int is valid
            } else if let (TastType::Ptr(_), TastType::Ptr(_)) = (at.0.clone(), bt.0.clone()) {
                // *T == *U is valid
            } else {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        expr.0 .0,
                        DiagnosticKind::EqualityOperators(at.0.to_string(), bt.0.to_string()),
                        expr.0 .2,
                    ),
                ));
            }

            TypedExpr(
                TastType::Bool,
                TypedExprKind::Equality(op, Box::new(at), Box::new(bt)),
            )
        }
        ExprKind::BinaryBitwise(op, a, b) => {
            let at = type_expr(scope, *a.clone())?;
            let bt = type_expr(scope, *b.clone())?;

            if !at.0.is_integer() {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        a.0 .0,
                        DiagnosticKind::ExpectedGot {
                            expected: "integer".to_string(),
                            got: at.0.to_string(),
                        },
                        a.0 .2,
                    ),
                ));
            }

            if !bt.0.is_integer() {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        b.0 .0,
                        DiagnosticKind::ExpectedGot {
                            expected: "integer".to_string(),
                            got: bt.0.to_string(),
                        },
                        b.0 .2,
                    ),
                ));
            }

            if at.0 != bt.0 {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        expr.0 .0,
                        DiagnosticKind::ExpectedSameType(at.0.to_string(), bt.0.to_string()),
                        expr.0 .2,
                    ),
                ));
            }

            TypedExpr(
                at.0.clone(),
                TypedExprKind::BinaryBitwise(op, Box::new(at), Box::new(bt)),
            )
        }
        ExprKind::Comparison(op, a, b) => {
            let at = type_expr(scope, *a.clone())?;
            let bt = type_expr(scope, *b.clone())?;

            if !at.0.is_integer() {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        a.0 .0,
                        DiagnosticKind::ExpectedGot {
                            expected: "integer".to_string(),
                            got: at.0.to_string(),
                        },
                        a.0 .2,
                    ),
                ));
            }

            if !bt.0.is_integer() {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        b.0 .0,
                        DiagnosticKind::ExpectedGot {
                            expected: "integer".to_string(),
                            got: bt.0.to_string(),
                        },
                        b.0 .2,
                    ),
                ));
            }

            if at.0 != bt.0 {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        expr.0 .0,
                        DiagnosticKind::ExpectedSameType(at.0.to_string(), bt.0.to_string()),
                        expr.0 .2,
                    ),
                ));
            }

            TypedExpr(
                TastType::Bool,
                TypedExprKind::Comparison(op, Box::new(at), Box::new(bt)),
            )
        }
        ExprKind::Arithmetic(op, a, b) => {
            let at = type_expr(scope, *a.clone())?;
            let bt = type_expr(scope, *b.clone())?;

            if !at.0.is_integer() {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        a.0 .0,
                        DiagnosticKind::ExpectedGot {
                            expected: "integer".to_string(),
                            got: at.0.to_string(),
                        },
                        a.0 .2,
                    ),
                ));
            }

            if !bt.0.is_integer() {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        b.0 .0,
                        DiagnosticKind::ExpectedGot {
                            expected: "integer".to_string(),
                            got: bt.0.to_string(),
                        },
                        b.0 .2,
                    ),
                ));
            }

            if at.0 != bt.0 {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        expr.0 .0,
                        DiagnosticKind::ExpectedSameType(at.0.to_string(), bt.0.to_string()),
                        expr.0 .2,
                    ),
                ));
            }

            TypedExpr(
                at.0.clone(),
                TypedExprKind::Arithmetic(op, Box::new(at), Box::new(bt)),
            )
        }

        ExprKind::Cast(x, t) => {
            let xt = type_expr(scope, *x)?;
            let tt = resolve_type(scope, t)?;

            if xt.0.is_integer() && tt.is_integer() {
                // int -> int cast is valid
                TypedExpr(tt.clone(), TypedExprKind::Cast(Box::new(xt), tt))
            } else if let (TastType::Ptr(_), TastType::Ptr(_)) = (xt.0.clone(), tt.clone()) {
                // *T -> *U cast is valid
                TypedExpr(tt.clone(), TypedExprKind::Cast(Box::new(xt), tt))
            } else if let (TastType::Ptr(_), _) | (_, TastType::Ptr(_)) = (xt.0.clone(), tt.clone())
            {
                // ensure one is an int
                if xt.0.is_integer() || tt.is_integer() {
                    // *T -> int or int -> *T cast is valid
                    TypedExpr(tt.clone(), TypedExprKind::Cast(Box::new(xt), tt))
                } else {
                    return Err(Diagnostic(
                        Severity::Error,
                        DiagnosticSpan(
                            expr.0 .0,
                            DiagnosticKind::InvalidCast(xt.0.to_string(), tt.to_string()),
                            expr.0 .2,
                        ),
                    ));
                }
            } else {
                return Err(Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        expr.0 .0,
                        DiagnosticKind::InvalidCast(xt.0.to_string(), tt.to_string()),
                        expr.0 .2,
                    ),
                ));
            }
        }

        ExprKind::NumberLiteral(n) => TypedExpr(TastType::I32, TypedExprKind::NumberLiteral(n)),
        ExprKind::StringLiteral(s) => TypedExpr(
            TastType::Ptr(Box::new(TastType::U8)),
            TypedExprKind::StringLiteral(s),
        ),
        ExprKind::Identifier(i) => {
            let t = scope.get_value(&i).ok_or_else(|| {
                Diagnostic(
                    Severity::Error,
                    DiagnosticSpan(
                        expr.0 .0,
                        DiagnosticKind::UnableToResolveIdentifier(i.clone()),
                        expr.0 .2,
                    ),
                )
            })?;
            TypedExpr(t.clone(), TypedExprKind::Identifier(i))
        }
        ExprKind::BooleanLiteral(b) => TypedExpr(TastType::Bool, TypedExprKind::BooleanLiteral(b)),
    })
}
