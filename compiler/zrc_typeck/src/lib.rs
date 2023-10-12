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

pub mod tast;
pub mod typeck;

use anyhow::{bail, Context as _};
use std::collections::HashMap;

use tast::{
    expr::{TypedExpr, TypedExprKind},
    stmt::{
        ArgumentDeclaration as TastArgumentDeclaration, LetDeclaration as TastLetDeclaration,
        TypedDeclaration, TypedStmt,
    },
    ty::Type as TastType,
};
use zrc_parser::ast::{
    expr::{Expr, ExprKind},
    stmt::{Declaration as AstDeclaration, LetDeclaration as AstLetDeclaration, Stmt, StmtKind},
    ty::{Type as ParserType, TypeKind as ParserTypeKind},
    Spanned,
};
