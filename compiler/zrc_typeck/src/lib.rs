#![warn(
    clippy::cargo,
    clippy::nursery,
    clippy::pedantic,
    clippy::missing_docs_in_private_items,
    missing_docs
)]
#![allow(clippy::multiple_crate_versions)]

use std::collections::HashMap;

pub mod tast;

use tast::expr::{TypedExpr, TypedExprKind};
use tast::ty::Type as TastType;
use zrc_parser::ast::{expr::Expr, ty::Type as ParserType};

fn resolve_type(
    type_scope: &HashMap<String, TastType>,
    ty: ParserType,
) -> Result<TastType, String> {
    Ok(match ty {
        ParserType::Identifier(x) => match x.as_str() {
            "i8" => TastType::I8,
            "u8" => TastType::U8,
            "i16" => TastType::I16,
            "u16" => TastType::U16,
            "i32" => TastType::I32,
            "u32" => TastType::U32,
            "i64" => TastType::I64,
            "u64" => TastType::U64,
            "isize" => TastType::Isize,
            "usize" => TastType::Usize,
            "bool" => TastType::Bool,
            _ => {
                if let Some(t) = type_scope.get(&x) {
                    t.clone()
                } else {
                    return Err(format!("Unknown type {}", x));
                }
            }
        },
        ParserType::Ptr(t) => TastType::Ptr(Box::new(resolve_type(type_scope, *t)?)),
        ParserType::Struct(members) => TastType::Struct(
            members
                .iter()
                .map(|(k, v)| Ok((k.clone(), resolve_type(type_scope, v.clone())?)))
                .collect::<Result<HashMap<String, TastType>, String>>()?,
        ),
    })
}

// FIXME: this NEEDS to be rewritten to use references almost everywhere and be no-clone. We stack overflow which is BAD.
fn type_expr(
    value_scope: &HashMap<String, TastType>, // X:i32 represents "let X: i32;"
    type_scope: &HashMap<String, TastType>,  // X:i32 represents "typedef X = i32;"
    expr: Expr,
) -> Result<TypedExpr, String> {
    Ok(match expr {
        Expr::Comma(a, b) => {
            let at = type_expr(value_scope, type_scope, *a)?;
            let bt = type_expr(value_scope, type_scope, *b)?;
            TypedExpr(
                bt.clone().0,
                TypedExprKind::Comma(Box::new(at), Box::new(bt)),
            )
        }
        Expr::Assignment(place, value) => {
            let place_t = type_expr(value_scope, type_scope, *place)?;
            let value_t = type_expr(value_scope, type_scope, *value)?;

            if place_t.0 != value_t.0 {
                return Err(format!(
                    "Type mismatch: {:?} != {:?}",
                    place_t.clone().0,
                    value_t.clone().0
                ));
            }

            TypedExpr(
                place_t.clone().0,
                TypedExprKind::Assignment(Box::new(place_t), Box::new(value_t)),
            )
        }

        Expr::AdditionAssignment(place, value) => type_expr(
            value_scope,
            type_scope,
            Expr::Assignment(place.clone(), Box::new(Expr::Addition(place, value))),
        )?,
        Expr::SubtractionAssignment(place, value) => type_expr(
            value_scope,
            type_scope,
            Expr::Assignment(place.clone(), Box::new(Expr::Subtraction(place, value))),
        )?,
        Expr::MultiplicationAssignment(place, value) => type_expr(
            value_scope,
            type_scope,
            Expr::Assignment(place.clone(), Box::new(Expr::Multiplication(place, value))),
        )?,
        Expr::DivisionAssignment(place, value) => type_expr(
            value_scope,
            type_scope,
            Expr::Assignment(place.clone(), Box::new(Expr::Division(place, value))),
        )?,
        Expr::ModuloAssignment(place, value) => type_expr(
            value_scope,
            type_scope,
            Expr::Assignment(place.clone(), Box::new(Expr::Modulo(place, value))),
        )?,
        Expr::BitwiseAndAssignment(place, value) => type_expr(
            value_scope,
            type_scope,
            Expr::Assignment(place.clone(), Box::new(Expr::BitwiseAnd(place, value))),
        )?,
        Expr::BitwiseOrAssignment(place, value) => type_expr(
            value_scope,
            type_scope,
            Expr::Assignment(place.clone(), Box::new(Expr::BitwiseOr(place, value))),
        )?,
        Expr::BitwiseXorAssignment(place, value) => type_expr(
            value_scope,
            type_scope,
            Expr::Assignment(place.clone(), Box::new(Expr::BitwiseXor(place, value))),
        )?,
        Expr::BitwiseLeftShiftAssignment(place, value) => type_expr(
            value_scope,
            type_scope,
            Expr::Assignment(
                place.clone(),
                Box::new(Expr::BitwiseLeftShift(place, value)),
            ),
        )?,
        Expr::BitwiseRightShiftAssignment(place, value) => type_expr(
            value_scope,
            type_scope,
            Expr::Assignment(
                place.clone(),
                Box::new(Expr::BitwiseRightShift(place, value)),
            ),
        )?,

        Expr::UnaryNot(x) => TypedExpr(
            TastType::Bool,
            TypedExprKind::UnaryNot(Box::new(type_expr(value_scope, type_scope, *x)?)),
        ),
        Expr::UnaryBitwiseNot(x) => {
            let t = type_expr(value_scope, type_scope, *x)?;
            if !t.0.is_integer() {
                return Err(format!("Cannot bitwise not {:?}", t.0));
            }
            TypedExpr(t.0.clone(), TypedExprKind::UnaryBitwiseNot(Box::new(t)))
        }
        Expr::UnaryMinus(x) => {
            let t = type_expr(value_scope, type_scope, *x)?;
            if !t.0.is_integer() {
                return Err(format!("Cannot unary minus {:?}", t.0));
            }
            if !t.0.is_signed_integer() {
                return Err(format!("Cannot unary minus unsigned integer {:?}", t.0));
            }
            TypedExpr(t.0.clone(), TypedExprKind::UnaryMinus(Box::new(t)))
        }
        Expr::UnaryAddressOf(x) => {
            let t = type_expr(value_scope, type_scope, *x)?;
            TypedExpr(
                TastType::Ptr(Box::new(t.0.clone())),
                TypedExprKind::UnaryAddressOf(Box::new(t)),
            )
        }
        Expr::UnaryDereference(x) => {
            let t = type_expr(value_scope, type_scope, *x)?;
            if let TastType::Ptr(tt) = t.clone().0 {
                TypedExpr(*tt, TypedExprKind::UnaryDereference(Box::new(t)))
            } else {
                return Err(format!("Cannot dereference {:?}", t.0));
            }
        }

        Expr::Index(ptr, offset) => {
            let ptr_t = type_expr(value_scope, type_scope, *ptr)?;
            let offset_t = type_expr(value_scope, type_scope, *offset)?;

            if offset_t.0 != TastType::Usize {
                return Err(format!("Index offset must be usize, not {:?}", offset_t.0));
            }

            if let TastType::Ptr(t) = ptr_t.0.clone() {
                TypedExpr(
                    *t,
                    TypedExprKind::Index(Box::new(ptr_t), Box::new(offset_t)),
                )
            } else {
                return Err(format!("Cannot index {:?}", ptr_t.0));
            }
        }

        Expr::Dot(obj, key) => {
            let obj_t = type_expr(value_scope, type_scope, *obj.clone())?;

            if let TastType::Struct(fields) = obj_t.0.clone() {
                if let Some(t) = fields.get(&key) {
                    TypedExpr(t.clone(), TypedExprKind::Dot(Box::new(obj_t), key.clone()))
                } else {
                    return Err(format!(
                        "Struct {:?} does not have field {:?}",
                        obj_t.0, key
                    ));
                }
            } else {
                return Err(format!("Cannot dot into non-struct type {:?}", obj_t.0));
            }
        }
        Expr::Arrow(obj, key) => {
            let obj_t = type_expr(value_scope, type_scope, *obj.clone())?;

            if let TastType::Ptr(_) = obj_t.0.clone() {
                type_expr(
                    value_scope,
                    type_scope,
                    Expr::Dot(Box::new(Expr::UnaryDereference(obj)), key),
                )?
            } else {
                return Err(format!(
                    "Cannot deref to access into non-pointer type {:?}",
                    obj_t.0
                ));
            }
        }
        Expr::Call(f, args) => {
            let ft = type_expr(value_scope, type_scope, *f)?;
            let args_t = args
                .iter()
                .map(|x| type_expr(value_scope, type_scope, x.clone()))
                .collect::<Result<Vec<TypedExpr>, String>>()?;

            if let TastType::Fn(arg_types, ret_type) = ft.0.clone() {
                if arg_types.len() != args_t.len() {
                    return Err(format!(
                        "Function takes {} arguments, not {}",
                        arg_types.len(),
                        args_t.len()
                    ));
                }

                for (i, (arg_type, arg_t)) in arg_types.iter().zip(args_t.iter()).enumerate() {
                    if arg_type != &arg_t.0 {
                        return Err(format!(
                            "Argument {} must be {:?}, not {:?}",
                            i, arg_type, arg_t.0
                        ));
                    }
                }

                TypedExpr(
                    *ret_type,
                    TypedExprKind::Call(Box::new(ft), args_t.iter().cloned().collect()),
                )
            } else {
                return Err(format!("Cannot call non-function type {:?}", ft.0));
            }
        }

        Expr::Ternary(cond, if_true, if_false) => {
            let cond_t = type_expr(value_scope, type_scope, *cond)?;
            let if_true_t = type_expr(value_scope, type_scope, *if_true)?;
            let if_false_t = type_expr(value_scope, type_scope, *if_false)?;

            if cond_t.0 != TastType::Bool {
                return Err(format!(
                    "Ternary condition must be bool, not {:?}",
                    cond_t.0
                ));
            }

            if if_true_t.0 != if_false_t.0 {
                return Err(format!(
                    "Ternary branches must have same type, not {:?} and {:?}",
                    if_true_t.0, if_false_t.0
                ));
            }

            TypedExpr(
                if_true_t.0.clone(),
                TypedExprKind::Ternary(Box::new(cond_t), Box::new(if_true_t), Box::new(if_false_t)),
            )
        }

        Expr::LogicalAnd(a, b) => {
            let at = type_expr(value_scope, type_scope, *a)?;
            let bt = type_expr(value_scope, type_scope, *b)?;

            if at.0 != TastType::Bool {
                return Err(format!("Logical AND lhs must be bool, not {:?}", at.0));
            }

            if bt.0 != TastType::Bool {
                return Err(format!("Logical AND rhs must be bool, not {:?}", bt.0));
            }

            TypedExpr(
                TastType::Bool,
                TypedExprKind::LogicalAnd(Box::new(at), Box::new(bt)),
            )
        }

        Expr::LogicalOr(a, b) => {
            let at = type_expr(value_scope, type_scope, *a)?;
            let bt = type_expr(value_scope, type_scope, *b)?;

            if at.0 != TastType::Bool {
                return Err(format!("Logical OR lhs must be bool, not {:?}", at.0));
            }

            if bt.0 != TastType::Bool {
                return Err(format!("Logical OR rhs must be bool, not {:?}", bt.0));
            }

            TypedExpr(
                TastType::Bool,
                TypedExprKind::LogicalOr(Box::new(at), Box::new(bt)),
            )
        }

        Expr::Equals(a, b) => {
            let at = type_expr(value_scope, type_scope, *a)?;
            let bt = type_expr(value_scope, type_scope, *b)?;

            if at.0 != bt.0 {
                return Err(format!(
                    "Equals lhs and rhs must have same type, not {:?} and {:?}",
                    at.0, bt.0
                ));
            }

            TypedExpr(
                TastType::Bool,
                TypedExprKind::Equals(Box::new(at), Box::new(bt)),
            )
        }

        Expr::NotEquals(a, b) => {
            let at = type_expr(value_scope, type_scope, *a)?;
            let bt = type_expr(value_scope, type_scope, *b)?;

            if at.0 != bt.0 {
                return Err(format!(
                    "NotEquals lhs and rhs must have same type, not {:?} and {:?}",
                    at.0, bt.0
                ));
            }

            TypedExpr(
                TastType::Bool,
                TypedExprKind::NotEquals(Box::new(at), Box::new(bt)),
            )
        }

        Expr::BitwiseAnd(a, b) => {
            let at = type_expr(value_scope, type_scope, *a)?;
            let bt = type_expr(value_scope, type_scope, *b)?;

            if !at.0.is_integer() {
                return Err(format!("BitwiseAnd lhs must be integer, not {:?}", at.0));
            }

            if !bt.0.is_integer() {
                return Err(format!("BitwiseAnd rhs must be integer, not {:?}", bt.0));
            }

            if at.0 != bt.0 {
                return Err(format!(
                    "BitwiseAnd lhs and rhs must have same type, not {:?} and {:?}",
                    at.0, bt.0
                ));
            }

            TypedExpr(
                at.0.clone(),
                TypedExprKind::BitwiseAnd(Box::new(at), Box::new(bt)),
            )
        }
        Expr::BitwiseOr(a, b) => {
            let at = type_expr(value_scope, type_scope, *a)?;
            let bt = type_expr(value_scope, type_scope, *b)?;

            if !at.0.is_integer() {
                return Err(format!("BitwiseOr lhs must be integer, not {:?}", at.0));
            }

            if !bt.0.is_integer() {
                return Err(format!("BitwiseOr rhs must be integer, not {:?}", bt.0));
            }

            if at.0 != bt.0 {
                return Err(format!(
                    "BitwiseOr lhs and rhs must have same type, not {:?} and {:?}",
                    at.0, bt.0
                ));
            }

            TypedExpr(
                at.0.clone(),
                TypedExprKind::BitwiseOr(Box::new(at), Box::new(bt)),
            )
        }
        Expr::BitwiseXor(a, b) => {
            let at = type_expr(value_scope, type_scope, *a)?;
            let bt = type_expr(value_scope, type_scope, *b)?;

            if !at.0.is_integer() {
                return Err(format!("BitwiseXor lhs must be integer, not {:?}", at.0));
            }

            if !bt.0.is_integer() {
                return Err(format!("BitwiseXor rhs must be integer, not {:?}", bt.0));
            }

            if at.0 != bt.0 {
                return Err(format!(
                    "BitwiseXor lhs and rhs must have same type, not {:?} and {:?}",
                    at.0, bt.0
                ));
            }

            TypedExpr(
                at.0.clone(),
                TypedExprKind::BitwiseXor(Box::new(at), Box::new(bt)),
            )
        }

        Expr::GreaterThan(a, b) => {
            let at = type_expr(value_scope, type_scope, *a)?;
            let bt = type_expr(value_scope, type_scope, *b)?;

            if !at.0.is_integer() {
                return Err(format!("GreaterThan lhs must be integer, not {:?}", at.0));
            }

            if !bt.0.is_integer() {
                return Err(format!("GreaterThan rhs must be integer, not {:?}", bt.0));
            }

            if at.0 != bt.0 {
                return Err(format!(
                    "GreaterThan lhs and rhs must have same type, not {:?} and {:?}",
                    at.0, bt.0
                ));
            }

            TypedExpr(
                TastType::Bool,
                TypedExprKind::GreaterThan(Box::new(at), Box::new(bt)),
            )
        }

        Expr::GreaterThanOrEqualTo(a, b) => {
            let at = type_expr(value_scope, type_scope, *a)?;
            let bt = type_expr(value_scope, type_scope, *b)?;

            if !at.0.is_integer() {
                return Err(format!(
                    "GreaterThanOrEqualTo lhs must be integer, not {:?}",
                    at.0
                ));
            }

            if !bt.0.is_integer() {
                return Err(format!(
                    "GreaterThanOrEqualTo rhs must be integer, not {:?}",
                    bt.0
                ));
            }

            if at.0 != bt.0 {
                return Err(format!(
                    "GreaterThanOrEqualTo lhs and rhs must have same type, not {:?} and {:?}",
                    at.0, bt.0
                ));
            }

            TypedExpr(
                TastType::Bool,
                TypedExprKind::GreaterThanOrEqualTo(Box::new(at), Box::new(bt)),
            )
        }

        Expr::LessThan(a, b) => {
            let at = type_expr(value_scope, type_scope, *a)?;
            let bt = type_expr(value_scope, type_scope, *b)?;

            if !at.0.is_integer() {
                return Err(format!("LessThan lhs must be integer, not {:?}", at.0));
            }

            if !bt.0.is_integer() {
                return Err(format!("LessThan rhs must be integer, not {:?}", bt.0));
            }

            if at.0 != bt.0 {
                return Err(format!(
                    "LessThan lhs and rhs must have same type, not {:?} and {:?}",
                    at.0, bt.0
                ));
            }

            TypedExpr(
                TastType::Bool,
                TypedExprKind::LessThan(Box::new(at), Box::new(bt)),
            )
        }

        Expr::LessThanOrEqualTo(a, b) => {
            let at = type_expr(value_scope, type_scope, *a)?;
            let bt = type_expr(value_scope, type_scope, *b)?;

            if !at.0.is_integer() {
                return Err(format!(
                    "LessThanOrEqualTo lhs must be integer, not {:?}",
                    at.0
                ));
            }

            if !bt.0.is_integer() {
                return Err(format!(
                    "LessThanOrEqualTo rhs must be integer, not {:?}",
                    bt.0
                ));
            }

            if at.0 != bt.0 {
                return Err(format!(
                    "LessThanOrEqualTo lhs and rhs must have same type, not {:?} and {:?}",
                    at.0, bt.0
                ));
            }

            TypedExpr(
                TastType::Bool,
                TypedExprKind::LessThanOrEqualTo(Box::new(at), Box::new(bt)),
            )
        }

        Expr::BitwiseRightShift(a, b) => {
            let at = type_expr(value_scope, type_scope, *a)?;
            let bt = type_expr(value_scope, type_scope, *b)?;

            if !at.0.is_integer() {
                return Err(format!(
                    "BitwiseRightShift lhs must be integer, not {:?}",
                    at.0
                ));
            }

            if !bt.0.is_integer() {
                return Err(format!(
                    "BitwiseRightShift rhs must be integer, not {:?}",
                    bt.0
                ));
            }

            if at.0 != bt.0 {
                return Err(format!(
                    "BitwiseRightShift lhs and rhs must have same type, not {:?} and {:?}",
                    at.0, bt.0
                ));
            }

            TypedExpr(
                at.0.clone(),
                TypedExprKind::BitwiseRightShift(Box::new(at), Box::new(bt)),
            )
        }

        Expr::BitwiseLeftShift(a, b) => {
            let at = type_expr(value_scope, type_scope, *a)?;
            let bt = type_expr(value_scope, type_scope, *b)?;

            if !at.0.is_integer() {
                return Err(format!(
                    "BitwiseLeftShift lhs must be integer, not {:?}",
                    at.0
                ));
            }

            if !bt.0.is_integer() {
                return Err(format!(
                    "BitwiseLeftShift rhs must be integer, not {:?}",
                    bt.0
                ));
            }

            if at.0 != bt.0 {
                return Err(format!(
                    "BitwiseLeftShift lhs and rhs must have same type, not {:?} and {:?}",
                    at.0, bt.0
                ));
            }

            TypedExpr(
                at.0.clone(),
                TypedExprKind::BitwiseLeftShift(Box::new(at), Box::new(bt)),
            )
        }

        Expr::Addition(a, b) => {
            let at = type_expr(value_scope, type_scope, *a)?;
            let bt = type_expr(value_scope, type_scope, *b)?;

            if !at.0.is_integer() {
                return Err(format!("Addition lhs must be integer, not {:?}", at.0));
            }

            if !bt.0.is_integer() {
                return Err(format!("Addition rhs must be integer, not {:?}", bt.0));
            }

            if at.0 != bt.0 {
                return Err(format!(
                    "Addition lhs and rhs must have same type, not {:?} and {:?}",
                    at.0, bt.0
                ));
            }

            TypedExpr(
                at.0.clone(),
                TypedExprKind::Addition(Box::new(at), Box::new(bt)),
            )
        }

        Expr::Subtraction(a, b) => {
            let at = type_expr(value_scope, type_scope, *a)?;
            let bt = type_expr(value_scope, type_scope, *b)?;

            if !at.0.is_integer() {
                return Err(format!("Subtraction lhs must be integer, not {:?}", at.0));
            }

            if !bt.0.is_integer() {
                return Err(format!("Subtraction rhs must be integer, not {:?}", bt.0));
            }

            if at.0 != bt.0 {
                return Err(format!(
                    "Subtraction lhs and rhs must have same type, not {:?} and {:?}",
                    at.0, bt.0
                ));
            }

            TypedExpr(
                at.0.clone(),
                TypedExprKind::Subtraction(Box::new(at), Box::new(bt)),
            )
        }

        Expr::Multiplication(a, b) => {
            let at = type_expr(value_scope, type_scope, *a)?;
            let bt = type_expr(value_scope, type_scope, *b)?;

            if !at.0.is_integer() {
                return Err(format!(
                    "Multiplication lhs must be integer, not {:?}",
                    at.0
                ));
            }

            if !bt.0.is_integer() {
                return Err(format!(
                    "Multiplication rhs must be integer, not {:?}",
                    bt.0
                ));
            }

            if at.0 != bt.0 {
                return Err(format!(
                    "Multiplication lhs and rhs must have same type, not {:?} and {:?}",
                    at.0, bt.0
                ));
            }

            TypedExpr(
                at.0.clone(),
                TypedExprKind::Multiplication(Box::new(at), Box::new(bt)),
            )
        }

        Expr::Division(a, b) => {
            let at = type_expr(value_scope, type_scope, *a)?;
            let bt = type_expr(value_scope, type_scope, *b)?;

            if !at.0.is_integer() {
                return Err(format!("Division lhs must be integer, not {:?}", at.0));
            }

            if !bt.0.is_integer() {
                return Err(format!("Division rhs must be integer, not {:?}", bt.0));
            }

            if at.0 != bt.0 {
                return Err(format!(
                    "Division lhs and rhs must have same type, not {:?} and {:?}",
                    at.0, bt.0
                ));
            }

            TypedExpr(
                at.0.clone(),
                TypedExprKind::Division(Box::new(at), Box::new(bt)),
            )
        }

        Expr::Modulo(a, b) => {
            let at = type_expr(value_scope, type_scope, *a)?;
            let bt = type_expr(value_scope, type_scope, *b)?;

            if !at.0.is_integer() {
                return Err(format!("Modulo lhs must be integer, not {:?}", at.0));
            }

            if !bt.0.is_integer() {
                return Err(format!("Modulo rhs must be integer, not {:?}", bt.0));
            }

            if at.0 != bt.0 {
                return Err(format!(
                    "Modulo lhs and rhs must have same type, not {:?} and {:?}",
                    at.0, bt.0
                ));
            }

            TypedExpr(
                at.0.clone(),
                TypedExprKind::Modulo(Box::new(at), Box::new(bt)),
            )
        }

        Expr::Cast(x, t) => {
            let xt = type_expr(value_scope, type_scope, *x)?;
            let tt = resolve_type(value_scope, t)?;

            if xt.0.is_integer() && tt.is_integer() {
                // int -> int cast is valid
                TypedExpr(tt.clone(), TypedExprKind::Cast(Box::new(xt), tt))
            } else if let (TastType::Ptr(_), TastType::Ptr(_)) = (xt.0.clone(), tt.clone()) {
                // *T -> *U cast is valid
                TypedExpr(tt.clone(), TypedExprKind::Cast(Box::new(xt), tt))
            } else {
                return Err(format!("Cannot cast {:?} to {:?}", xt.0, tt));
            }
        }

        Expr::NumberLiteral(n) => TypedExpr(TastType::I32, TypedExprKind::NumberLiteral(n)),
        Expr::StringLiteral(s) => TypedExpr(
            TastType::Ptr(Box::new(TastType::U8)),
            TypedExprKind::StringLiteral(s),
        ),
        Expr::Identifier(i) => {
            let t = value_scope
                .get(&i)
                .ok_or(format!("Unknown identifier {}", i))?;
            TypedExpr(t.clone(), TypedExprKind::Identifier(i))
        }
        Expr::BooleanLiteral(b) => TypedExpr(TastType::Bool, TypedExprKind::BooleanLiteral(b)),

        Expr::Error => return Err("Parse error encountered".to_string()),
    })
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockReturnKind {
    /// Does not return at all
    NoReturn,
    Void,
    Returns(TastType),
}

// Considerations when designing the type_block function:
// Must take an argument for the parent scopes
// Takes a block (Vec<Stmt>), statements make no sense on their own
// Desugars for to while
// Desugars "if (x) y;" to "if (x) {y;}"
// Takes an argument listing how its return behavior works. It either can't, may, or must return, and it is either void or of type T.
// The function must also provide a value to the caller indicating if this block ALWAYS returns or has some cases where it may not return (if it is passed the "may return") option. Because if in a block that must return, all of its sub-blocks *may* return, but one of them must be guaranteed to return if it does not include its own return statement. For example:
//
// { // This block must return.
//     { // This block MAY return.
//         if (x) return; // This MAY return.
//     } // This block WILL SOMETIMES return.
//     // Because the above block is not GUARANTEED to return, the "must return" is not yet satisfied.
//
// Some work is needed to determine what the behavior of MAY/WILL is in most cases. For example, if WILL else WILL is WILL, but if MAY else WILL is MAY.
// TODO: Add typeck_stmt. This might be done after (if) we add a HIR.

#[cfg(test)]
mod tests {
    // TODO: Tests
    use super::*;

    #[test]
    fn test() {
        let mut value_scope: HashMap<String, TastType> = HashMap::new();
        value_scope.insert("int".to_string(), TastType::I32);
        value_scope.insert("intptr".to_string(), TastType::Ptr(Box::new(TastType::I32)));
        value_scope.insert(
            "struct".to_string(),
            TastType::Struct(HashMap::from([
                ("int".to_string(), TastType::I32),
                ("intptr".to_string(), TastType::Ptr(Box::new(TastType::I32))),
            ])),
        );
        value_scope.insert(
            "structptr".to_string(),
            TastType::Ptr(Box::new(TastType::Struct(HashMap::from([
                ("int".to_string(), TastType::I32),
                ("intptr".to_string(), TastType::Ptr(Box::new(TastType::I32))),
            ])))),
        );
        value_scope.insert(
            "add3".to_string(),
            TastType::Fn(
                vec![TastType::I32, TastType::I32, TastType::I32],
                Box::new(TastType::I32),
            ),
        );

        let mut type_scope: HashMap<String, TastType> = HashMap::new();

        let result = type_expr(
            &value_scope,
            &type_scope,
            zrc_parser::parser::parse_expr(concat!(
                "int = 5,",
                "int += 5,",
                "int -= 5,",
                "int *= 5,",
                "int /= 5,",
                "int %= 5,",
                "int &= 5,",
                "int |= 5,",
                "int ^= 5,",
                "int <<= 5,",
                "int >>= 5,",
                "!int,",
                "~int,",
                "-int,",
                "&int,",
                "*intptr,",
                "*(&int),",
                "&intptr,",
                "intptr[5 as usize],",
                "intptr[*intptr as usize],",
                "struct.int,",
                "*struct.intptr,",
                "struct.intptr[5 as usize],",
                "(&struct)->int,",
                "(*structptr).int,",
                "structptr->int,",
                "add3(int, *intptr, structptr->int),",
                "add3(5, 6, 4),",
                "int > 10 ? 5 : 6,",
                "(int > 10 && int < 20) ? struct.intptr[structptr->int as usize] : int",
                // "int == 7 || int != 7,",
                // "(int & 32) | 17 ^ 3,",
                // "int >> 7,",
                // "int + 32,",
                // "*intptr - 4,",
                // "int * 32,",
                // "int % int == 0,",
                // "(!int) == true"
            ))
            .unwrap(),
        )
        .unwrap();
        // dbg!(result);
        println!("{}", result);

        assert_eq!(true, false);
        // assert_eq!(
        //     type_expr(&scope, zrc_parser::parser::parse_expr("x,5").unwrap()),
        //     Ok(TypedExpr(
        //         ZrType::I32,
        //         TypedExprKind::Comma(
        //             Box::new(TypedExpr(ZrType::I32, TypedExprKind::Identifier("x".to_string()))),
        //             Box::new(TypedExpr(
        //                 ZrType::I32,
        //                 TypedExprKind::NumberLiteral("5".to_string())
        //             )),
        //         )
        //     ))
        // );
    }
}
