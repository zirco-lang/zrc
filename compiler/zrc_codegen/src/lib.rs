#![doc = include_str!("../README.md")]
#![allow(unknown_lints)] // in case you use non-nightly clippy
#![warn(
    clippy::cargo,
    clippy::nursery,
    clippy::pedantic,
    clippy::missing_docs_in_private_items,
    missing_docs,
    clippy::absolute_paths,
    clippy::as_conversions,
    clippy::dbg_macro,
    clippy::decimal_literal_representation,
    clippy::deref_by_slicing,
    clippy::disallowed_script_idents,
    clippy::else_if_without_else,
    clippy::empty_structs_with_brackets,
    clippy::format_push_string,
    clippy::if_then_some_else_none,
    clippy::let_underscore_must_use,
    clippy::min_ident_chars,
    clippy::mixed_read_write_in_expression,
    clippy::multiple_inherent_impl,
    clippy::multiple_unsafe_ops_per_block,
    clippy::non_ascii_literal,
    clippy::redundant_type_annotations,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::same_name_method,
    clippy::semicolon_inside_block,
    clippy::unseparated_literal_suffix,
    clippy::implicit_clone,
    clippy::todo,
    clippy::undocumented_unsafe_blocks,
    clippy::unimplemented,
    clippy::unneeded_field_pattern,
    clippy::wildcard_enum_match_arm,
    let_underscore_drop,
    macro_use_extern_crate,
    missing_debug_implementations,
    non_exhaustive_omitted_patterns,
    unsafe_op_in_unsafe_fn,
    unused_crate_dependencies,
    variant_size_differences,
    unused_qualifications,
    clippy::unwrap_used,

    // These should be enabled in any non-user-facing code, like the parser, but not in the
    // frontend.
    clippy::print_stderr,
    clippy::print_stdout
)]
#![allow(
    clippy::multiple_crate_versions,
    clippy::cargo_common_metadata,
    clippy::module_name_repetitions,
    clippy::doc_comment_double_space_linebreaks
)]

// Ordering matters! Declared here so other modules have access
/// Convenience macro to unpack a `BasicBlockAnd` -- assigns to the provided
/// `bb` and yields the value
macro_rules! unpack {
    ($bb:ident = $call:expr) => {
        match $call {
            BasicBlockAnd {
                bb: unpacked_bb,
                value,
            } => {
                $bb = unpacked_bb;
                value
            }
        }
    };
}

use std::collections::HashMap;

use inkwell::{basic_block::BasicBlock, targets::TargetMachine, values::PointerValue};

mod ctx;
mod expr;
mod program;
mod stmt;
#[cfg(test)]
mod test_utils;
mod ty;

pub use inkwell::{
    OptimizationLevel,
    debug_info::DWARFEmissionKind as DebugLevel,
    targets::{FileType, TargetTriple},
};
pub use program::{cg_program_to_buffer, cg_program_to_string};

/// Gets the native [`TargetTriple`].
#[must_use]
pub fn get_native_triple() -> TargetTriple {
    TargetMachine::get_default_triple()
}

/// Represents the code generation scope, or the mapping from identifiers to
/// their LLVM [`PointerValue`]s.
#[derive(Debug, Clone, PartialEq)]
struct CgScope<'input, 'ctx> {
    /// The contained mappings from identifiers to [`PointerValue`]s
    identifiers: HashMap<&'input str, PointerValue<'ctx>>,
}
impl<'input, 'ctx> CgScope<'input, 'ctx> {
    /// Get the [`PointerValue`] of a particular identifier, if it exists
    pub fn get(&self, id: &'input str) -> Option<PointerValue<'ctx>> {
        self.identifiers.get(id).copied()
    }

    /// Insert a [`PointerValue`] of an identifier into the scope
    pub fn insert(&mut self, id: &'input str, value: PointerValue<'ctx>) {
        self.identifiers.insert(id, value);
    }

    /// Create a new [`CgScope`] with no values
    pub fn new() -> Self {
        Self {
            identifiers: HashMap::new(),
        }
    }
}

impl Default for CgScope<'_, '_> {
    fn default() -> Self {
        Self::new()
    }
}

/// Represents some value along with a basic block.
/// This is used for code generation functions that may produce new basic blocks
/// along with some result value.
struct BasicBlockAnd<'ctx, T> {
    /// The basic block returned
    bb: BasicBlock<'ctx>,
    /// Any other data the function wishes to pass
    value: T,
}
impl<T> BasicBlockAnd<'_, T> {
    /// Discard the basic block and return the value
    pub fn into_value(self) -> T {
        self.value
    }
}
/// Extends Inkwell [`BasicBlock`]s with a method to easily produce a
/// [`BasicBlockAnd`] value
trait BasicBlockExt<'ctx> {
    /// Wrap a [`BasicBlock`] and a value into a [`BasicBlockAnd`] instance, to
    /// allow easier composition of functions which return basic blocks
    /// along with some other value
    fn and<T>(self, value: T) -> BasicBlockAnd<'ctx, T>;
}
impl<'ctx> BasicBlockExt<'ctx> for BasicBlock<'ctx> {
    fn and<T>(self, value: T) -> BasicBlockAnd<'ctx, T> {
        BasicBlockAnd { bb: self, value }
    }
}
