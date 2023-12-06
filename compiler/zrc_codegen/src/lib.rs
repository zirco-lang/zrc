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
    clippy::string_to_string,
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
#![allow(clippy::multiple_crate_versions, clippy::cargo_common_metadata)]

use std::{cmp::Ordering, collections::HashMap};

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    debug_info::DebugInfoBuilder,
    module::Module,
    targets::TargetMachine,
    values::{FunctionValue, PointerValue},
};

mod expr;
mod program;
mod stmt;
#[cfg(test)]
mod test_utils;
mod ty;

pub use inkwell::{
    debug_info::DWARFEmissionKind as DebugLevel,
    targets::{FileType, TargetTriple},
    OptimizationLevel,
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

impl<'input, 'ctx> Default for CgScope<'input, 'ctx> {
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

/// Common LLVM structures passed onto most code generation functions
#[derive(Debug, Clone, Copy)]
struct CgContext<'ctx, 'a> {
    /// The LLVM context
    ctx: &'ctx Context,
    /// The LLVM target machine
    target_machine: &'a TargetMachine,
    /// The LLVM builder used to build instructions
    builder: &'a Builder<'ctx>,
    /// The lookup for lines in the source file
    line_lookup: &'a CgLineLookup,
    /// The LLVM builder for debug info
    dbg_builder: &'a DebugInfoBuilder<'ctx>,
    /// The LLVM compile unit for debug info
    compilation_unit: &'a inkwell::debug_info::DICompileUnit<'ctx>,
    /// The LLVM module we are building in
    #[allow(dead_code)]
    module: &'a Module<'ctx>,
    /// The LLVM function we are building in
    fn_value: FunctionValue<'ctx>,
}

/// Wrapper around a line and column
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct LineAndCol {
    /// The 1-indexed line number
    line: u32,
    /// The 1-indexed column number
    // REVIEW: Is it truly 1-indexed? If it's 0, LLVM will treat it as "no column known"
    col: u32,
}

/// Simple tool to lookup the line and column of a location in the source file
#[derive(Debug)]
struct CgLineLookup {
    /// The spans of each line in the input
    line_spans: Vec<(usize, usize)>,
}

impl CgLineLookup {
    /// Creates a new [`CgLineLookup`] over a string
    pub fn new(input: &str) -> Self {
        let mut line_start = 0;
        let mut line_spans = vec![];
        for line in input.split('\n') {
            let line_end = line_start + line.len() + "\n".len();
            // TODO: this assumes lines terminate with \n, not \r\n.
            line_spans.push((line_start, line_end - 1));
            line_start = line_end;
        }

        Self { line_spans }
    }

    /// Look up the `1`-indexed line number from an offset in the string
    pub fn lookup_from_index(&self, index: usize) -> LineAndCol {
        let line = self
            .line_spans
            .binary_search_by(|(line_start, line_end)| {
                if *line_end < index {
                    return Ordering::Less;
                }
                if *line_start > index {
                    return Ordering::Greater;
                }

                Ordering::Equal
            })
            .expect("line should be present");

        LineAndCol {
            line: u32::try_from(line)
                .expect("there should probably never be more than u32::MAX lines in a file")
                + 1,
            col: u32::try_from(index - self.line_spans[line].0)
                .expect("there should probably never be more than u32::MAX columns in a line")
                + 1,
        }
    }
}
