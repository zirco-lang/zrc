//! for expressions

use anyhow::{bail, Context as _};
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
fn expr_to_place(expr: TypedExpr) -> anyhow::Result<Place> {
    Ok(match expr.1 {
        TypedExprKind::UnaryDereference(x) => Place(expr.0, PlaceKind::Deref(x)),
        TypedExprKind::Identifier(x) => Place(expr.0, PlaceKind::Variable(x)),
        TypedExprKind::Index(x, y) => Place(expr.0, PlaceKind::Index(x, y)),
        TypedExprKind::Dot(x, y) => Place(expr.0, PlaceKind::Dot(Box::new(expr_to_place(*x)?), y)),
        TypedExprKind::Arrow(x, y) => {
            Place(expr.0, PlaceKind::Arrow(Box::new(expr_to_place(*x)?), y))
        }
        _ => bail!("Cannot assign to non-place expression {}", expr.0),
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
pub fn type_expr(scope: &Scope, expr: Expr) -> anyhow::Result<TypedExpr> {
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
            // TODO: Check place is a valid lvalue so that `7 = 4;` is invalid

            // Desugar `x += y` to `x = x + y`.
            let (place, value) = desugar_assignment(mode, *place, *value);

            let place_t = expr_to_place(type_expr(scope, place)?)?;
            let value_t = type_expr(scope, value)?;

            if place_t.0 != value_t.0 {
                bail!("Type mismatch: {} != {}", place_t.0, value_t.0);
            }

            TypedExpr(
                place_t.clone().0,
                TypedExprKind::Assignment(Box::new(place_t), Box::new(value_t)),
            )
        }

        ExprKind::UnaryNot(x) => {
            let t = type_expr(scope, *x)?;
            if t.0 != TastType::Bool {
                bail!("Cannot not {}", t.0);
            }
            TypedExpr(t.0.clone(), TypedExprKind::UnaryNot(Box::new(t)))
        }
        ExprKind::UnaryBitwiseNot(x) => {
            let t = type_expr(scope, *x)?;
            if !t.0.is_integer() {
                bail!("Cannot bitwise not {}", t.0);
            }
            TypedExpr(t.0.clone(), TypedExprKind::UnaryBitwiseNot(Box::new(t)))
        }
        ExprKind::UnaryMinus(x) => {
            let t = type_expr(scope, *x)?;
            if !t.0.is_integer() {
                bail!("Cannot unary minus {}", t.0);
            }
            if !t.0.is_signed_integer() {
                bail!("Cannot unary minus unsigned integer {}", t.0);
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
                bail!("Cannot dereference {}", t.0);
            }
        }

        ExprKind::Index(ptr, offset) => {
            let ptr_t = type_expr(scope, *ptr)?;
            let offset_t = type_expr(scope, *offset)?;

            if !offset_t.0.is_integer() {
                bail!("Index offset must be integer, not {}", offset_t.0);
            }

            if let TastType::Ptr(t) = ptr_t.0.clone() {
                TypedExpr(
                    *t,
                    TypedExprKind::Index(Box::new(ptr_t), Box::new(offset_t)),
                )
            } else {
                bail!("Cannot index {}", ptr_t.0);
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
                    bail!("Struct {} does not have field {}", obj_t.0, key.1);
                }
            } else {
                bail!("Cannot dot into non-struct type {}", obj_t.0);
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
                bail!("Cannot deref to access into non-pointer type {}", obj_t.0);
            }
        }
        ExprKind::Call(f, args) => {
            let ft = type_expr(scope, *f)?;
            let args_t = args
                .1
                .iter()
                .map(|x| type_expr(scope, x.clone()))
                .collect::<anyhow::Result<Vec<TypedExpr>>>()?;

            if let TastType::Fn(arg_types, ret_type) = ft.0.clone() {
                if arg_types.len() != args_t.len() {
                    bail!(
                        "Function takes {} arguments, not {}",
                        arg_types.len(),
                        args_t.len()
                    );
                }

                for (i, (arg_type, arg_t)) in arg_types.iter().zip(args_t.iter()).enumerate() {
                    if arg_type != &arg_t.0 {
                        bail!("Argument {} must be {}, not {}", i, arg_type, arg_t.0);
                    }
                }

                TypedExpr(
                    ret_type.into_option().unwrap_or(TastType::Void),
                    TypedExprKind::Call(Box::new(ft), args_t),
                )
            } else {
                bail!("Cannot call non-function type {}", ft.0);
            }
        }

        ExprKind::Ternary(cond, if_true, if_false) => {
            let cond_t = type_expr(scope, *cond)?;
            let if_true_t = type_expr(scope, *if_true)?;
            let if_false_t = type_expr(scope, *if_false)?;

            if cond_t.0 != TastType::Bool {
                bail!("Ternary condition must be bool, not {}", cond_t.0);
            }

            if if_true_t.0 != if_false_t.0 {
                bail!(
                    "Ternary branches must have same type, not {} and {}",
                    if_true_t.0,
                    if_false_t.0
                );
            }

            TypedExpr(
                if_true_t.0.clone(),
                TypedExprKind::Ternary(Box::new(cond_t), Box::new(if_true_t), Box::new(if_false_t)),
            )
        }

        ExprKind::Logical(op, a, b) => {
            let at = type_expr(scope, *a)?;
            let bt = type_expr(scope, *b)?;

            if at.0 != TastType::Bool {
                bail!("Operator `{op}` lhs must be bool, not {}", at.0);
            }

            if bt.0 != TastType::Bool {
                bail!("Operator `{op}` rhs must be bool, not {}", bt.0);
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
                bail!(
                    "Operator `{}` lhs and rhs must be same-type integer or pointer, not {} and {}",
                    op,
                    at.0,
                    bt.0
                );
            }

            TypedExpr(
                TastType::Bool,
                TypedExprKind::Equality(op, Box::new(at), Box::new(bt)),
            )
        }
        ExprKind::BinaryBitwise(op, a, b) => {
            let at = type_expr(scope, *a)?;
            let bt = type_expr(scope, *b)?;

            if !at.0.is_integer() {
                bail!("Operator `{op}` lhs must be integer, not {}", at.0);
            }

            if !bt.0.is_integer() {
                bail!("Operator `{op}` rhs must be integer, not {}", bt.0);
            }

            if at.0 != bt.0 {
                bail!(
                    "Operator `{op}` lhs and rhs must have same type, not {} and {}",
                    at.0,
                    bt.0
                );
            }

            TypedExpr(
                at.0.clone(),
                TypedExprKind::BinaryBitwise(op, Box::new(at), Box::new(bt)),
            )
        }
        ExprKind::Comparison(op, a, b) => {
            let at = type_expr(scope, *a)?;
            let bt = type_expr(scope, *b)?;

            if !at.0.is_integer() {
                bail!("Operator `{op}` lhs must be integer, not {}", at.0);
            }

            if !bt.0.is_integer() {
                bail!("Operator `{op}` rhs must be integer, not {}", bt.0);
            }

            if at.0 != bt.0 {
                bail!(
                    "Operator `{op}` lhs and rhs must have same type, not {} and {}",
                    at.0,
                    bt.0
                );
            }

            TypedExpr(
                TastType::Bool,
                TypedExprKind::Comparison(op, Box::new(at), Box::new(bt)),
            )
        }
        ExprKind::Arithmetic(op, a, b) => {
            let at = type_expr(scope, *a)?;
            let bt = type_expr(scope, *b)?;

            if !at.0.is_integer() {
                bail!("Operator `{op}` lhs must be integer, not {}", at.0);
            }

            if !bt.0.is_integer() {
                bail!("Operator `{op}` rhs must be integer, not {}", bt.0);
            }

            if at.0 != bt.0 {
                bail!(
                    "Operator `{op}` lhs and rhs must have same type, not {} and {}",
                    at.0,
                    bt.0
                );
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
                    bail!("Cannot cast {} to {}", xt.0, tt);
                }
            } else {
                bail!("Cannot cast {} to {}", xt.0, tt);
            }
        }

        ExprKind::NumberLiteral(n) => TypedExpr(TastType::I32, TypedExprKind::NumberLiteral(n)),
        ExprKind::StringLiteral(s) => TypedExpr(
            TastType::Ptr(Box::new(TastType::U8)),
            TypedExprKind::StringLiteral(s),
        ),
        ExprKind::Identifier(i) => {
            let t = scope
                .get_value(&i)
                .context(format!("Unknown identifier {i}"))?;
            TypedExpr(t.clone(), TypedExprKind::Identifier(i))
        }
        ExprKind::BooleanLiteral(b) => TypedExpr(TastType::Bool, TypedExprKind::BooleanLiteral(b)),

        ExprKind::Error => bail!("Parse error encountered"),
    })
}
