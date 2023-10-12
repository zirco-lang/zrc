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
