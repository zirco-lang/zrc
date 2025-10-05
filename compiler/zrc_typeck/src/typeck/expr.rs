//! for expressions

mod access;
mod assignment;
mod binary;
mod call;
mod helpers;
mod literals;
mod misc;
mod unary;

pub use helpers::try_coerce_to;
use zrc_diagnostics::Diagnostic;
use zrc_parser::ast::expr::{Expr, ExprKind};

use super::scope::Scope;
use crate::tast::expr::TypedExpr;

// FIXME: this NEEDS to be rewritten to use references almost everywhere and be
// no-clone. We stack overflow for deep expressions which is VERY VERY BAD.
/// Type check and infer an [AST expression](Expr) to a [TAST
/// expression](TypedExpr).
///
/// # Errors
/// Errors if a type checker error is encountered.
#[allow(clippy::missing_panics_doc)]
pub fn type_expr<'input>(
    scope: &Scope<'input, '_>,
    expr: Expr<'input>,
) -> Result<TypedExpr<'input>, Diagnostic> {
    let expr_span = expr.0.span();
    Ok(match expr.0.into_value() {
        ExprKind::Comma(lhs, rhs) => misc::type_expr_comma(scope, expr_span, *lhs, *rhs)?,
        ExprKind::Assignment(mode, place, value) => {
            assignment::type_expr_assignment(scope, expr_span, mode, *place, *value)?
        }
        ExprKind::UnaryNot(x) => unary::type_expr_unary_not(scope, expr_span, *x)?,
        ExprKind::UnaryBitwiseNot(x) => unary::type_expr_unary_bitwise_not(scope, expr_span, *x)?,
        ExprKind::UnaryMinus(x) => unary::type_expr_unary_minus(scope, expr_span, *x)?,
        ExprKind::UnaryAddressOf(x) => unary::type_expr_unary_address_of(scope, expr_span, *x)?,
        ExprKind::UnaryDereference(x) => unary::type_expr_unary_dereference(scope, expr_span, *x)?,
        ExprKind::Index(ptr, offset) => access::type_expr_index(scope, expr_span, *ptr, *offset)?,
        ExprKind::Dot(obj, key) => access::type_expr_dot(scope, expr_span, *obj, key)?,
        ExprKind::Arrow(obj, key) => access::type_expr_arrow(scope, expr_span, obj, key)?,
        ExprKind::Call(f, args) => call::type_expr_call(scope, expr_span, *f, args)?,
        ExprKind::Ternary(cond, if_true, if_false) => {
            misc::type_expr_ternary(scope, expr_span, *cond, *if_true, *if_false)?
        }
        ExprKind::Logical(op, lhs, rhs) => {
            binary::type_expr_logical(scope, expr_span, op, *lhs, *rhs)?
        }
        ExprKind::Equality(op, lhs, rhs) => {
            binary::type_expr_equality(scope, expr_span, op, *lhs, *rhs)?
        }
        ExprKind::BinaryBitwise(op, lhs, rhs) => {
            binary::type_expr_binary_bitwise(scope, expr_span, op, *lhs, *rhs)?
        }
        ExprKind::Comparison(op, lhs, rhs) => {
            binary::type_expr_comparison(scope, expr_span, op, *lhs, *rhs)?
        }
        ExprKind::Arithmetic(op, lhs, rhs) => {
            binary::type_expr_arithmetic(scope, expr_span, op, *lhs, *rhs)?
        }
        ExprKind::Cast(x, ty) => misc::type_expr_cast(scope, expr_span, *x, ty)?,
        ExprKind::SizeOfType(ty) => misc::type_expr_size_of_type(scope, expr_span, ty)?,
        ExprKind::SizeOfExpr(x) => misc::type_expr_size_of_expr(scope, expr_span, *x)?,
        ExprKind::NumberLiteral(n, ty) => {
            literals::type_expr_number_literal(scope, expr_span, n, ty)?
        }
        ExprKind::StringLiteral(str) => literals::type_expr_string_literal(scope, expr_span, str),
        ExprKind::CharLiteral(ch) => literals::type_expr_char_literal(scope, expr_span, ch),
        ExprKind::Identifier(i) => literals::type_expr_identifier(scope, expr_span, i)?,
        ExprKind::BooleanLiteral(value) => {
            literals::type_expr_boolean_literal(scope, expr_span, value)
        }
        ExprKind::StructConstruction(ty, fields) => {
            misc::type_expr_struct_construction(scope, expr_span, ty, &fields)?
        }
    })
}

#[cfg(test)]
mod tests {

    use std::collections::HashMap;

    use indexmap::IndexMap;
    use zrc_diagnostics::DiagnosticKind;
    use zrc_utils::spanned;

    use super::*;
    use crate::{
        tast::{
            stmt::{ArgumentDeclaration, ArgumentDeclarationList},
            ty::{Fn, Type as TastType},
        },
        typeck::scope::{GlobalScope, TypeCtx, ValueCtx},
    };

    #[test]
    #[allow(clippy::too_many_lines)]
    fn various_expressions_infer_correctly() {
        let scope = GlobalScope {
            global_values: ValueCtx::from_mappings(HashMap::from([
                ("i8", TastType::I8),
                ("u8", TastType::U8),
                ("i32", TastType::I32),
                ("bool", TastType::Bool),
                (
                    "s",
                    TastType::Struct(IndexMap::from([("i8", TastType::I8)])),
                ),
                (
                    "get_bool",
                    TastType::Fn(Fn {
                        arguments: ArgumentDeclarationList::NonVariadic(vec![]),
                        returns: Box::new(TastType::Bool),
                    }),
                ),
                (
                    "id",
                    TastType::Fn(Fn {
                        arguments: ArgumentDeclarationList::NonVariadic(vec![
                            ArgumentDeclaration {
                                name: spanned!(0, "x", 1),
                                ty: spanned!(0, TastType::I32, 3),
                            },
                        ]),
                        returns: Box::new(TastType::I32),
                    }),
                ),
                (
                    "sink",
                    TastType::Fn(Fn {
                        arguments: ArgumentDeclarationList::Variadic(vec![ArgumentDeclaration {
                            name: spanned!(0, "i8", 3),
                            ty: spanned!(0, TastType::I8, 3),
                        }]),
                        returns: Box::new(TastType::unit()),
                    }),
                ),
                (
                    "void_ptr_func",
                    TastType::Fn(Fn {
                        arguments: ArgumentDeclarationList::NonVariadic(vec![
                            ArgumentDeclaration {
                                name: spanned!(0, "ptr", 3),
                                ty: spanned!(0, TastType::Ptr(Box::new(TastType::unit())), 3),
                            },
                        ]),
                        returns: Box::new(TastType::unit()),
                    }),
                ),
            ])),
            types: TypeCtx::from_defaults_and_mappings(HashMap::from([(
                "NonIntegerType",
                TastType::Struct(IndexMap::from([])),
            )])),
            ..Default::default()
        };

        let tests = [
            ("i8, i32", Ok(TastType::I32)),
            ("i8 = i8", Ok(TastType::I8)),
            (
                "i8 = i32",
                Err(DiagnosticKind::InvalidAssignmentRightHandSideType {
                    expected: "i8".to_string(),
                    got: "i32".to_string(),
                }),
            ),
            (
                "4 = i8",
                Err(DiagnosticKind::NotAnLvalue("{int}".to_string())),
            ),
            ("!bool", Ok(TastType::Bool)),
            (
                "!i8",
                Err(DiagnosticKind::ExpectedGot {
                    expected: "boolean".to_string(),
                    got: "i8".to_string(),
                }),
            ),
            ("~i8", Ok(TastType::I8)),
            (
                "~bool",
                Err(DiagnosticKind::ExpectedGot {
                    expected: "integer".to_string(),
                    got: "bool".to_string(),
                }),
            ),
            ("-i8", Ok(TastType::I8)),
            (
                "-bool",
                Err(DiagnosticKind::ExpectedGot {
                    expected: "signed integer".to_string(),
                    got: "bool".to_string(),
                }),
            ),
            ("&i8", Ok(TastType::Ptr(Box::new(TastType::I8)))),
            ("*&i8", Ok(TastType::I8)),
            (
                "*i8",
                Err(DiagnosticKind::CannotDereferenceNonPointer(
                    "i8".to_string(),
                )),
            ),
            ("(&i8)[4 as usize]", Ok(TastType::I8)),
            (
                "(&i8)['a']",
                Err(DiagnosticKind::ExpectedGot {
                    expected: "usize".to_string(),
                    got: "u8".to_string(),
                }),
            ),
            (
                "i8[4 as usize]",
                Err(DiagnosticKind::CannotIndexIntoNonPointer("i8".to_string())),
            ),
            ("s.i8", Ok(TastType::I8)),
            (
                "s.fake",
                Err(DiagnosticKind::StructOrUnionDoesNotHaveMember(
                    "struct { i8: i8 }".to_string(),
                    "fake".to_string(),
                )),
            ),
            (
                "i32.fake",
                Err(DiagnosticKind::StructMemberAccessOnNonStruct(
                    "i32".to_string(),
                )),
            ),
            ("(&s)->i8", Ok(TastType::I8)),
            ("get_bool()", Ok(TastType::Bool)),
            (
                "get_bool(i32)",
                Err(DiagnosticKind::FunctionArgumentCountMismatch {
                    expected: "0".to_string(),
                    got: "1".to_string(),
                }),
            ),
            (
                "id(i8)",
                Err(DiagnosticKind::FunctionArgumentTypeMismatch {
                    n: 0,
                    expected: "i32".to_string(),
                    got: "i8".to_string(),
                }),
            ),
            ("sink(i8, i32, bool)", Ok(TastType::unit())),
            (
                "sink()",
                Err(DiagnosticKind::FunctionArgumentCountMismatch {
                    expected: "at least 1".to_string(),
                    got: "0".to_string(),
                }),
            ),
            (
                "sink(i32)",
                Err(DiagnosticKind::FunctionArgumentTypeMismatch {
                    n: 0,
                    expected: "i8".to_string(),
                    got: "i32".to_string(),
                }),
            ),
            // Test void pointer implicit downcasts
            ("void_ptr_func(&i8)", Ok(TastType::unit())),
            ("void_ptr_func(&i32)", Ok(TastType::unit())),
            ("void_ptr_func(&bool)", Ok(TastType::unit())),
            ("void_ptr_func(&s)", Ok(TastType::unit())),
            (
                "bool()",
                Err(DiagnosticKind::CannotCallNonFunction("bool".to_string())),
            ),
            ("bool ? i8 : i8", Ok(TastType::I8)),
            (
                "i8 ? i8 : i8",
                Err(DiagnosticKind::ExpectedGot {
                    expected: "boolean".to_string(),
                    got: "i8".to_string(),
                }),
            ),
            (
                "bool ? i8 : i32",
                Err(DiagnosticKind::ExpectedSameType(
                    "i8".to_string(),
                    "i32".to_string(),
                )),
            ),
            ("bool && bool", Ok(TastType::Bool)),
            (
                "i32 && bool",
                Err(DiagnosticKind::ExpectedGot {
                    expected: "bool".to_string(),
                    got: "i32".to_string(),
                }),
            ),
            ("i8 == i8", Ok(TastType::Bool)),
            ("(&i8) == (&i8)", Ok(TastType::Bool)),
            (
                "(&i8) == i8",
                Err(DiagnosticKind::EqualityOperators(
                    "*i8".to_string(),
                    "i8".to_string(),
                )),
            ),
            ("bool == bool", Ok(TastType::Bool)),
            ("i8 >> u8", Ok(TastType::I8)),
            (
                "i8 >> i8",
                Err(DiagnosticKind::ExpectedGot {
                    expected: "unsigned integer".to_string(),
                    got: "i8".to_string(),
                }),
            ),
            (
                "i8 & i32",
                Err(DiagnosticKind::ExpectedSameType(
                    "i8".to_string(),
                    "i32".to_string(),
                )),
            ),
            ("i8 & i8", Ok(TastType::I8)),
            ("i8 > i8", Ok(TastType::Bool)),
            (
                "bool > i8",
                Err(DiagnosticKind::ExpectedGot {
                    expected: "integer".to_string(),
                    got: "bool".to_string(),
                }),
            ),
            (
                "i8 > i32",
                Err(DiagnosticKind::ExpectedSameType(
                    "i8".to_string(),
                    "i32".to_string(),
                )),
            ),
            ("i8 + i8", Ok(TastType::I8)),
            (
                "i32 + i8",
                Err(DiagnosticKind::ExpectedSameType(
                    "i32".to_string(),
                    "i8".to_string(),
                )),
            ),
            (
                "(&i8) / 2",
                Err(DiagnosticKind::InvalidPointerArithmeticOperation(
                    "/".to_string(),
                )),
            ),
            ("(&i8) + 2", Ok(TastType::Ptr(Box::new(TastType::I8)))),
            (
                "(&i8) + (2 as usize)",
                Ok(TastType::Ptr(Box::new(TastType::I8))),
            ),
            ("i8 as i32", Ok(TastType::I32)),
            ("(&i8) as *i32", Ok(TastType::Ptr(Box::new(TastType::I32)))),
            ("(&i8) as usize", Ok(TastType::Usize)),
            ("0 as *i8", Ok(TastType::Ptr(Box::new(TastType::I8)))),
            ("true as i32", Ok(TastType::I32)),
            (
                "s as i8",
                Err(DiagnosticKind::InvalidCast(
                    "struct { i8: i8 }".to_string(),
                    "i8".to_string(),
                )),
            ),
            ("sizeof(7)", Ok(TastType::Usize)),
            ("sizeof struct {}", Ok(TastType::Usize)),
            ("\"hello\"", Ok(TastType::Ptr(Box::new(TastType::U8)))),
            ("'a'", Ok(TastType::U8)),
            ("true", Ok(TastType::Bool)),
            ("4", Ok(TastType::Int)),
            ("4i8", Ok(TastType::I8)),
            ("-4i8", Ok(TastType::I8)),
            (
                "4 NonIntegerType",
                Err(DiagnosticKind::InvalidNumberLiteralType(
                    "struct {}".to_string(),
                )),
            ),
            (
                "bogus",
                Err(DiagnosticKind::UnableToResolveIdentifier(
                    "bogus".to_string(),
                )),
            ),
        ];

        for (input, expected_result) in tests {
            assert_eq!(
                type_expr(
                    &scope.create_subscope(),
                    zrc_parser::parser::parse_expr(input).expect("parsing should succeed")
                )
                .map(|result| result.inferred_type)
                .map_err(|diagnostic| diagnostic.1.into_value()),
                expected_result
            );
        }
    }
}
