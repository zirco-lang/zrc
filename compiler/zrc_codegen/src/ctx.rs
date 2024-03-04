//! Structures used internally within the code generator for context and state management

use inkwell::{
    builder::Builder,
    context::Context,
    debug_info::{DICompileUnit, DebugInfoBuilder},
    module::Module,
    targets::TargetMachine,
    values::FunctionValue,
};
use zrc_utils::line_finder::LineLookup;

/// Common LLVM structures passed onto most code generation functions
#[derive(Debug, Clone, Copy)]
pub struct CgContext<'ctx, 'a> {
    /// The LLVM context
    pub ctx: &'ctx Context,
    /// The LLVM target machine
    pub target_machine: &'a TargetMachine,
    /// The LLVM builder used to build instructions
    pub builder: &'a Builder<'ctx>,
    /// The lookup for lines in the source file
    pub line_lookup: &'a LineLookup,
    /// The LLVM builder for debug info
    pub dbg_builder: &'a DebugInfoBuilder<'ctx>,
    /// The LLVM compile unit for debug info
    pub compilation_unit: &'a DICompileUnit<'ctx>,
    /// The LLVM module we are building in
    #[allow(dead_code)]
    pub module: &'a Module<'ctx>,
    /// The LLVM function we are building in
    pub fn_value: FunctionValue<'ctx>,
}
