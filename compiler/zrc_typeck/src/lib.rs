//! Type checker and TAST for the Zirco programming language
//!
//! This crate contains functions for generating the [Typed AST](tast) and check
//! program types in the process.

#![warn(
    clippy::cargo,
    clippy::nursery,
    clippy::pedantic,
    clippy::missing_docs_in_private_items,
    missing_docs
)]
#![allow(clippy::multiple_crate_versions, clippy::cargo_common_metadata)]

use std::collections::HashMap;

pub mod tast;

use tast::{
    expr::{TypedExpr, TypedExprKind},
    stmt::{
        ArgumentDeclaration as TastArgumentDeclaration, LetDeclaration as TastLetDeclaration,
        TypedDeclaration, TypedStmt,
    },
    ty::Type as TastType,
};
use zrc_parser::ast::{
    expr::Expr,
    stmt::{Declaration as AstDeclaration, Stmt},
    ty::Type as ParserType,
};

/// Contains scoping information during type checking.
///
/// Cloning it resembles the creation of a subscope.
#[derive(Debug, Clone)]
pub struct Scope {
    /// Maps identifiers for values to their types.
    value_scope: HashMap<String, TastType>,
    /// Maps type names to their actual type representations.
    type_scope: HashMap<String, TastType>,
}
impl Scope {
    /// Creates a new Scope with no values or types.
    #[must_use]
    pub fn new() -> Self {
        Self {
            value_scope: HashMap::new(),
            type_scope: HashMap::new(),
        }
    }

    /// Creates a new Scope from two [`HashMap`]s.
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn from_scopes(
        value_scope: HashMap<String, TastType>,
        type_scope: HashMap<String, TastType>,
    ) -> Self {
        Self {
            value_scope,
            type_scope,
        }
    }

    /// Gets a value-identifier's type from this [`Scope`]
    #[must_use]
    pub fn get_value(&self, identifier: &String) -> Option<&TastType> {
        self.value_scope.get(identifier)
    }

    /// Gets a type-identifier's type from this [`Scope`]
    #[must_use]
    pub fn get_type(&self, identifier: &String) -> Option<&TastType> {
        self.type_scope.get(identifier)
    }

    /// Sets a value-identifier's type in this [`Scope`]
    pub fn set_value(&mut self, identifier: String, ty: TastType) {
        self.value_scope.insert(identifier, ty);
    }

    /// Sets a type-identifier's type in this [`Scope`]
    pub fn set_type(&mut self, identifier: String, ty: TastType) {
        self.type_scope.insert(identifier, ty);
    }
}
impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

/// Resolve an identifier to its corresponding [`tast::ty::Type`].
///
/// # Errors
/// Errors if the identifier is not found in the type scope.
fn resolve_type(scope: &Scope, ty: ParserType) -> Result<TastType, String> {
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
            "bool" => TastType::Bool,
            _ => {
                if let Some(t) = scope.get_type(&x) {
                    t.clone()
                } else {
                    return Err(format!("Unknown type {x}"));
                }
            }
        },
        ParserType::Ptr(t) => TastType::Ptr(Box::new(resolve_type(scope, *t)?)),
        ParserType::Struct(members) => TastType::Struct(
            members
                .iter()
                .map(|(k, v)| Ok((k.clone(), resolve_type(scope, v.clone())?)))
                .collect::<Result<HashMap<String, TastType>, String>>()?,
        ),
    })
}

/// Describes whether a block returns void or a type.
#[derive(Debug, Clone, PartialEq)]
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
    pub fn into_tast_type(self) -> TastType {
        match self {
            Self::Void => TastType::Void,
            Self::Return(t) => t,
        }
    }
}

/// Describes if a block MAY, MUST, or MUST NOT return.
#[derive(Debug, Clone, PartialEq)]
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

/// Desugar an assignment like `x += y` to `x = x + y`.
fn desugar_assignment(
    mode: zrc_parser::ast::expr::Assignment,
    lhs: Expr,
    rhs: Expr,
) -> (Expr, Expr) {
    use zrc_parser::ast::expr::Assignment;
    match mode {
        Assignment::Standard => (lhs, rhs),
        Assignment::Arithmetic(op) => (
            lhs.clone(),
            Expr::Arithmetic(op, Box::new(lhs), Box::new(rhs)),
        ),
        Assignment::BinaryBitwise(op) => (
            lhs.clone(),
            Expr::BinaryBitwise(op, Box::new(lhs), Box::new(rhs)),
        ),
    }
}

/// Validate an expr into a place
fn expr_to_place(expr: TypedExpr) -> Result<tast::expr::Place, String> {
    use tast::expr::{Place, PlaceKind};
    Ok(match expr.1 {
        TypedExprKind::UnaryDereference(x) => Place(expr.0, PlaceKind::Deref(x)),
        TypedExprKind::Identifier(x) => Place(expr.0, PlaceKind::Variable(x)),
        TypedExprKind::Index(x, y) => Place(expr.0, PlaceKind::Index(x, y)),
        TypedExprKind::Dot(x, y) => Place(expr.0, PlaceKind::Dot(Box::new(expr_to_place(*x)?), y)),
        TypedExprKind::Arrow(x, y) => {
            Place(expr.0, PlaceKind::Arrow(Box::new(expr_to_place(*x)?), y))
        }
        _ => return Err(format!("Cannot assign to non-place expression {}", expr.0)),
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
pub fn type_expr(scope: &Scope, expr: Expr) -> Result<TypedExpr, String> {
    Ok(match expr {
        Expr::Comma(a, b) => {
            let at = type_expr(scope, *a)?;
            let bt = type_expr(scope, *b)?;
            TypedExpr(
                bt.clone().0,
                TypedExprKind::Comma(Box::new(at), Box::new(bt)),
            )
        }
        Expr::Assignment(mode, place, value) => {
            // TODO: Check place is a valid lvalue so that `7 = 4;` is invalid

            // Desugar `x += y` to `x = x + y`.
            let (place, value) = desugar_assignment(mode, *place, *value);

            let place_t = expr_to_place(type_expr(scope, place)?)?;
            let value_t = type_expr(scope, value)?;

            if place_t.0 != value_t.0 {
                return Err(format!("Type mismatch: {} != {}", place_t.0, value_t.0));
            }

            TypedExpr(
                place_t.clone().0,
                TypedExprKind::Assignment(Box::new(place_t), Box::new(value_t)),
            )
        }

        Expr::UnaryNot(x) => {
            let t = type_expr(scope, *x)?;
            if t.0 != TastType::Bool {
                return Err(format!("Cannot not {}", t.0));
            }
            TypedExpr(t.0.clone(), TypedExprKind::UnaryNot(Box::new(t)))
        }
        Expr::UnaryBitwiseNot(x) => {
            let t = type_expr(scope, *x)?;
            if !t.0.is_integer() {
                return Err(format!("Cannot bitwise not {}", t.0));
            }
            TypedExpr(t.0.clone(), TypedExprKind::UnaryBitwiseNot(Box::new(t)))
        }
        Expr::UnaryMinus(x) => {
            let t = type_expr(scope, *x)?;
            if !t.0.is_integer() {
                return Err(format!("Cannot unary minus {}", t.0));
            }
            if !t.0.is_signed_integer() {
                return Err(format!("Cannot unary minus unsigned integer {}", t.0));
            }
            TypedExpr(t.0.clone(), TypedExprKind::UnaryMinus(Box::new(t)))
        }
        Expr::UnaryAddressOf(x) => {
            let t = type_expr(scope, *x)?;
            TypedExpr(
                TastType::Ptr(Box::new(t.0.clone())),
                TypedExprKind::UnaryAddressOf(Box::new(t)),
            )
        }
        Expr::UnaryDereference(x) => {
            let t = type_expr(scope, *x)?;
            if let TastType::Ptr(tt) = t.clone().0 {
                TypedExpr(*tt, TypedExprKind::UnaryDereference(Box::new(t)))
            } else {
                return Err(format!("Cannot dereference {}", t.0));
            }
        }

        Expr::Index(ptr, offset) => {
            let ptr_t = type_expr(scope, *ptr)?;
            let offset_t = type_expr(scope, *offset)?;

            if !offset_t.0.is_integer() {
                return Err(format!("Index offset must be integer, not {}", offset_t.0));
            }

            if let TastType::Ptr(t) = ptr_t.0.clone() {
                TypedExpr(
                    *t,
                    TypedExprKind::Index(Box::new(ptr_t), Box::new(offset_t)),
                )
            } else {
                return Err(format!("Cannot index {}", ptr_t.0));
            }
        }

        Expr::Dot(obj, key) => {
            let obj_t = type_expr(scope, *obj)?;

            if let TastType::Struct(fields) = obj_t.0.clone() {
                if let Some(t) = fields.get(&key) {
                    TypedExpr(t.clone(), TypedExprKind::Dot(Box::new(obj_t), key.clone()))
                } else {
                    return Err(format!("Struct {} does not have field {}", obj_t.0, key));
                }
            } else {
                return Err(format!("Cannot dot into non-struct type {}", obj_t.0));
            }
        }
        Expr::Arrow(obj, key) => {
            let obj_t = type_expr(scope, *obj.clone())?;

            if let TastType::Ptr(_) = obj_t.0 {
                type_expr(scope, Expr::Dot(Box::new(Expr::UnaryDereference(obj)), key))?
            } else {
                return Err(format!(
                    "Cannot deref to access into non-pointer type {}",
                    obj_t.0
                ));
            }
        }
        Expr::Call(f, args) => {
            let ft = type_expr(scope, *f)?;
            let args_t = args
                .iter()
                .map(|x| type_expr(scope, x.clone()))
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
                            "Argument {} must be {}, not {}",
                            i, arg_type, arg_t.0
                        ));
                    }
                }

                TypedExpr(
                    ret_type.into_option().unwrap_or(TastType::Void),
                    TypedExprKind::Call(Box::new(ft), args_t),
                )
            } else {
                return Err(format!("Cannot call non-function type {}", ft.0));
            }
        }

        Expr::Ternary(cond, if_true, if_false) => {
            let cond_t = type_expr(scope, *cond)?;
            let if_true_t = type_expr(scope, *if_true)?;
            let if_false_t = type_expr(scope, *if_false)?;

            if cond_t.0 != TastType::Bool {
                return Err(format!("Ternary condition must be bool, not {}", cond_t.0));
            }

            if if_true_t.0 != if_false_t.0 {
                return Err(format!(
                    "Ternary branches must have same type, not {} and {}",
                    if_true_t.0, if_false_t.0
                ));
            }

            TypedExpr(
                if_true_t.0.clone(),
                TypedExprKind::Ternary(Box::new(cond_t), Box::new(if_true_t), Box::new(if_false_t)),
            )
        }

        Expr::Logical(op, a, b) => {
            let at = type_expr(scope, *a)?;
            let bt = type_expr(scope, *b)?;

            if at.0 != TastType::Bool {
                return Err(format!("Operator `{op}` lhs must be bool, not {}", at.0));
            }

            if bt.0 != TastType::Bool {
                return Err(format!("Operator `{op}` rhs must be bool, not {}", bt.0));
            }

            TypedExpr(
                TastType::Bool,
                TypedExprKind::Logical(op, Box::new(at), Box::new(bt)),
            )
        }
        Expr::Equality(op, a, b) => {
            let at = type_expr(scope, *a)?;
            let bt = type_expr(scope, *b)?;

            if at.0.is_integer() && bt.0.is_integer() && at.0 == bt.0 {
                // int == int is valid
            } else if let (TastType::Ptr(_), TastType::Ptr(_)) = (at.0.clone(), bt.0.clone()) {
                // *T == *U is valid
            } else {
                return Err(format!(
                    "Operator `{op}` lhs and rhs must be same-type integer or pointer, not {} and {}",
                    at.0, bt.0
                ));
            }

            TypedExpr(
                TastType::Bool,
                TypedExprKind::Equality(op, Box::new(at), Box::new(bt)),
            )
        }
        Expr::BinaryBitwise(op, a, b) => {
            let at = type_expr(scope, *a)?;
            let bt = type_expr(scope, *b)?;

            if !at.0.is_integer() {
                return Err(format!("Operator `{op}` lhs must be integer, not {}", at.0));
            }

            if !bt.0.is_integer() {
                return Err(format!("Operator `{op}` rhs must be integer, not {}", bt.0));
            }

            if at.0 != bt.0 {
                return Err(format!(
                    "Operator `{op}` lhs and rhs must have same type, not {} and {}",
                    at.0, bt.0
                ));
            }

            TypedExpr(
                at.0.clone(),
                TypedExprKind::BinaryBitwise(op, Box::new(at), Box::new(bt)),
            )
        }
        Expr::Comparison(op, a, b) => {
            let at = type_expr(scope, *a)?;
            let bt = type_expr(scope, *b)?;

            if !at.0.is_integer() {
                return Err(format!("Operator `{op}` lhs must be integer, not {}", at.0));
            }

            if !bt.0.is_integer() {
                return Err(format!("Operator `{op}` rhs must be integer, not {}", bt.0));
            }

            if at.0 != bt.0 {
                return Err(format!(
                    "Operator `{op}` lhs and rhs must have same type, not {} and {}",
                    at.0, bt.0
                ));
            }

            TypedExpr(
                TastType::Bool,
                TypedExprKind::Comparison(op, Box::new(at), Box::new(bt)),
            )
        }
        Expr::Arithmetic(op, a, b) => {
            let at = type_expr(scope, *a)?;
            let bt = type_expr(scope, *b)?;

            if !at.0.is_integer() {
                return Err(format!("Operator `{op}` lhs must be integer, not {}", at.0));
            }

            if !bt.0.is_integer() {
                return Err(format!("Operator `{op}` rhs must be integer, not {}", bt.0));
            }

            if at.0 != bt.0 {
                return Err(format!(
                    "Operator `{op}` lhs and rhs must have same type, not {} and {}",
                    at.0, bt.0
                ));
            }

            TypedExpr(
                at.0.clone(),
                TypedExprKind::Arithmetic(op, Box::new(at), Box::new(bt)),
            )
        }

        Expr::Cast(x, t) => {
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
                    return Err(format!("Cannot cast {} to {}", xt.0, tt));
                }
            } else {
                return Err(format!("Cannot cast {} to {}", xt.0, tt));
            }
        }

        Expr::NumberLiteral(n) => TypedExpr(TastType::I32, TypedExprKind::NumberLiteral(n)),
        Expr::StringLiteral(s) => TypedExpr(
            TastType::Ptr(Box::new(TastType::U8)),
            TypedExprKind::StringLiteral(s),
        ),
        Expr::Identifier(i) => {
            let t = scope
                .get_value(&i)
                .ok_or(format!("Unknown identifier {i}"))?;
            TypedExpr(t.clone(), TypedExprKind::Identifier(i))
        }
        Expr::BooleanLiteral(b) => TypedExpr(TastType::Bool, TypedExprKind::BooleanLiteral(b)),

        Expr::Error => return Err("Parse error encountered".to_string()),
    })
}

/// Convert a single [AST statement](Stmt) like `x;` to a block statement `{ x;
/// }` without converting `{ x; }` to `{ { x; } }`. This is preferred instead of
/// `vec![x]` as it prevents extra nesting layers.
fn coerce_stmt_into_block(stmt: Stmt) -> Vec<Stmt> {
    match stmt {
        Stmt::BlockStmt(stmts) => stmts,
        _ => vec![stmt],
    }
}

/// Process an [AST declaration](AstDeclaration) and place it in the appropriate
/// scope, returning a [TAST declaration](TypedDeclaration).
///
/// This mutates `value_scope` and `type_scope` to add the new declaration.
///
/// # Errors
/// Errors if a type checker error is encountered.
#[allow(clippy::too_many_lines)]
pub fn process_declaration(
    scope: &mut Scope,
    declaration: AstDeclaration,
) -> Result<TypedDeclaration, String> {
    Ok(match declaration {
        AstDeclaration::DeclarationList(declarations) => {
            TypedDeclaration::DeclarationList(
                declarations
                    .into_iter()
                    .map(|let_declaration| -> Result<TastLetDeclaration, String> {
                        if scope.get_value(&let_declaration.name).is_some() {
                            // TODO: In the future we may allow shadowing but currently no
                            return Err(format!(
                                "Identifier {} already in use",
                                let_declaration.name
                            ));
                        }

                        let typed_expr = let_declaration
                            .value
                            .map(|expr| type_expr(scope, expr))
                            .transpose()?;
                        let resolved_ty = let_declaration
                            .ty
                            .map(|ty| resolve_type(scope, ty))
                            .transpose()?;

                        let result_decl = match (typed_expr, resolved_ty) {
                            (None, None) => Err(
                                "No explicit variable type present and no value to infer from"
                                    .to_string(),
                            ),

                            // Explicitly typed with no value
                            (None, Some(ty)) => Ok(TastLetDeclaration {
                                name: let_declaration.name,
                                ty,
                                value: None,
                            }),

                            // Infer type from value
                            (Some(TypedExpr(ty, ex)), None) => Ok(TastLetDeclaration {
                                name: let_declaration.name,
                                ty: ty.clone(),
                                value: Some(TypedExpr(ty, ex)),
                            }),

                            // Both explicitly typed and inferable
                            (Some(TypedExpr(ty, ex)), Some(resolved_ty)) => {
                                if ty == resolved_ty {
                                    Ok(TastLetDeclaration {
                                        name: let_declaration.name,
                                        ty: ty.clone(),
                                        value: Some(TypedExpr(ty, ex)),
                                    })
                                } else {
                                    Err(format!(
                                        concat!(
                                            "Cannot assign value of type {} to binding of",
                                            " type {}"
                                        ),
                                        ty, resolved_ty
                                    ))
                                }
                            }
                        };
                        if let Ok(TastLetDeclaration { name, ty, .. }) = &result_decl {
                            scope.set_value(name.clone(), ty.clone());
                        }

                        result_decl
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            )
        }
        AstDeclaration::FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
        } => {
            if scope.get_value(&name).is_some() {
                return Err(format!("Identifier {name} already in use"));
            }

            let resolved_return_type = return_type
                .map(|ty| resolve_type(scope, ty))
                .transpose()?
                .map_or(BlockReturnType::Void, BlockReturnType::Return);

            let resolved_parameters = parameters
                .into_iter()
                .map(|parameter| -> Result<TastArgumentDeclaration, String> {
                    Ok(TastArgumentDeclaration {
                        name: parameter.name,
                        ty: resolve_type(scope, parameter.ty)?,
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;

            scope.set_value(
                name.clone(),
                TastType::Fn(
                    resolved_parameters
                        .iter()
                        .map(|x| x.ty.clone())
                        .collect::<Vec<_>>(),
                    Box::new(resolved_return_type.clone()),
                ),
            );

            TypedDeclaration::FunctionDeclaration {
                name,
                parameters: resolved_parameters.clone(),
                return_type: resolved_return_type.clone().into_option(),
                body: if let Some(body) = body {
                    // functions run in their own value scope because identifiers from the parent
                    // scope are not accessible the type scope is kept, however.
                    let function_scope = Scope::from_scopes(
                        resolved_parameters
                            .iter()
                            .map(|x| (x.name.clone(), x.ty.clone()))
                            .collect::<HashMap<_, _>>(),
                        scope.type_scope.clone(),
                    );

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
            if scope.get_type(&name).is_some() {
                return Err(format!("Type name {name} already in use"));
            }

            let resolved_pairs = fields
                .into_iter()
                .map(|(name, ty)| -> Result<(String, TastType), String> {
                    Ok((name, resolve_type(scope, ty)?))
                })
                .collect::<Result<HashMap<_, _>, _>>()?;

            scope.set_type(name.clone(), TastType::Struct(resolved_pairs.clone()));

            TypedDeclaration::StructDeclaration {
                name,
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
pub fn type_block(
    parent_scope: &Scope,
    input_block: Vec<Stmt>,
    can_use_break_continue: bool,
    return_ability: BlockReturnAbility,
) -> Result<(Vec<TypedStmt>, BlockReturnActuality), String> {
    let mut scope = parent_scope.clone();

    // At first, the block does not return.
    let (tast_block, return_actualities): (Vec<_>, Vec<_>) = input_block
        .into_iter()
        .map(
            |stmt| -> Result<(TypedStmt, BlockReturnActuality), String> {
                match stmt {
                    Stmt::EmptyStmt => {
                        Ok((TypedStmt::EmptyStmt, BlockReturnActuality::DoesNotReturn))
                    }
                    Stmt::BreakStmt if can_use_break_continue => {
                        Ok((TypedStmt::BreakStmt, BlockReturnActuality::DoesNotReturn))
                    }
                    Stmt::BreakStmt => Err("Cannot use break statement here".to_string()),

                    Stmt::ContinueStmt if can_use_break_continue => {
                        Ok((TypedStmt::BreakStmt, BlockReturnActuality::DoesNotReturn))
                    }
                    Stmt::ContinueStmt => Err("Cannot use continue statement here".to_string()),

                    Stmt::Declaration(declaration) => Ok((
                        TypedStmt::Declaration(process_declaration(&mut scope, declaration)?),
                        BlockReturnActuality::DoesNotReturn, /* because expressions can't return */
                    )),

                    Stmt::IfStmt(cond, then, then_else) => {
                        // TODO: if `cond` is always true at compile-time, we can prove the if
                        // branch is always taken (hence if it's WillReturn we can be WillReturn
                        // instead of MayReturn)

                        let typed_cond = type_expr(&scope, cond)?;

                        if typed_cond.0 != TastType::Bool {
                            return Err(format!("If condition must be bool, not {}", typed_cond.0));
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
                    Stmt::WhileStmt(cond, body) => {
                        // TODO: we might be able to prove that the body runs at least once or an
                        // infinite loop making this won't/will return statically

                        let typed_cond = type_expr(&scope, cond)?;

                        if typed_cond.0 != TastType::Bool {
                            return Err(format!(
                                "While condition must be bool, not {}",
                                typed_cond.0
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
                    Stmt::ForStmt {
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
                            .map(|decl| process_declaration(&mut loop_scope, *decl))
                            .transpose()?;

                        let typed_cond =
                            cond.map(|cond| type_expr(&loop_scope, cond)).transpose()?;
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

                    Stmt::BlockStmt(body) => {
                        let (typed_body, return_actuality) = type_block(
                            &scope,
                            body,
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

                    Stmt::ExprStmt(expr) => Ok((
                        TypedStmt::ExprStmt(type_expr(&scope, expr)?),
                        BlockReturnActuality::DoesNotReturn,
                    )),
                    Stmt::ReturnStmt(value) => {
                        let resolved_value =
                            value.map(|expr| type_expr(&scope, expr)).transpose()?;
                        match (resolved_value, return_ability.clone()) {
                            // expects no return
                            (_, BlockReturnAbility::MustNotReturn) => {
                                Err("Cannot return from a block that must not return".to_string())
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
                            ) => Err(format!(
                                "Cannot return void from a block expecting a return type of {t}",
                            )),

                            // return x; in fn expecting to return void
                            (
                                Some(TypedExpr(ty, _)),
                                BlockReturnAbility::MustReturn(BlockReturnType::Void)
                                | BlockReturnAbility::MayReturn(BlockReturnType::Void),
                            ) => Err(format!(
                                concat!(
                                    "Cannot return value of type {} from a block that must",
                                    " return void"
                                ),
                                ty
                            )),

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
                                    Err(format!(
                                        concat!(
                                            "Cannot return value of type {} from a block that",
                                            " must return type {}"
                                        ),
                                        ty, t
                                    ))
                                }
                            }
                        }
                    }
                }
            },
        )
        .collect::<Result<Vec<_>, _>>()?
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
            Err("Block must return, but no sub-block is guaranteed to return".to_string())
        }
        (BlockReturnAbility::MustReturn(_), BlockReturnActuality::DoesNotReturn) => {
            Err("Block must return, but no sub-block is guaranteed to return".to_string())
        }
        (BlockReturnAbility::MustNotReturn, BlockReturnActuality::MightReturn) => {
            Err("Block must not return, but a sub-block may return".to_string())
        }
        (BlockReturnAbility::MustNotReturn, BlockReturnActuality::WillReturn) => {
            Err("Block must not return, but a sub-block may return".to_string())
        }
    }
}

#[cfg(test)]
mod tests {
    // TODO: Tests
    use super::*;
}
