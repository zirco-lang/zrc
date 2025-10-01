//! Alternative error formatters using external crates
//!
//! This module provides alternative implementations of diagnostic formatting
//! using popular error formatting crates like `codespan-reporting` and `ariadne`.
//!
//! These are provided as optional features to investigate and compare different
//! error formatting approaches.

#[cfg(feature = "codespan")]
pub mod codespan_fmt;

#[cfg(feature = "ariadne-fmt")]
pub mod ariadne_fmt;
