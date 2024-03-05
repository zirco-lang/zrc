//! Structures used internally within the code generator for context and state
//! management

use inkwell::{
    builder::Builder,
    context::Context,
    debug_info::{DICompileUnit, DebugInfoBuilder},
    module::Module,
    targets::TargetMachine,
    values::FunctionValue,
};
use zrc_utils::line_finder::LineLookup;

/// Trait for any context with at least the fields of [`CompilationUnitCtx`]
#[allow(dead_code)]
pub trait AsCompilationUnitCtx<'ctx, 'a> {
    /// The LLVM context
    fn ctx(&self) -> &'ctx Context;
    /// The LLVM target machine
    fn target_machine(&self) -> &'a TargetMachine;
    /// The LLVM builder used to build instructions
    fn builder(&self) -> &'a Builder<'ctx>;
    /// The lookup for lines in the source file
    fn line_lookup(&self) -> &'a LineLookup;
    /// The LLVM builder for debug info
    fn dbg_builder(&self) -> &'a DebugInfoBuilder<'ctx>;
    /// The LLVM compile unit for debug info
    fn compilation_unit(&self) -> &'a DICompileUnit<'ctx>;
    /// The LLVM module we are building in
    fn module(&self) -> &'a Module<'ctx>;
}

/// LLVM structures common to a single compilation unit (file)
#[derive(Debug, Clone, Copy)]
pub struct CompilationUnitCtx<'ctx, 'a> {
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
    pub module: &'a Module<'ctx>,
}
impl<'ctx, 'a> AsCompilationUnitCtx<'ctx, 'a> for CompilationUnitCtx<'ctx, 'a> {
    fn ctx(&self) -> &'ctx Context {
        self.ctx
    }
    fn target_machine(&self) -> &'a TargetMachine {
        self.target_machine
    }
    fn builder(&self) -> &'a Builder<'ctx> {
        self.builder
    }
    fn line_lookup(&self) -> &'a LineLookup {
        self.line_lookup
    }
    fn dbg_builder(&self) -> &'a DebugInfoBuilder<'ctx> {
        self.dbg_builder
    }
    fn compilation_unit(&self) -> &'a DICompileUnit<'ctx> {
        self.compilation_unit
    }
    fn module(&self) -> &'a Module<'ctx> {
        self.module
    }
}

/// Common LLVM structures passed onto most code generation functions
#[derive(Debug, Clone, Copy)]
pub struct FunctionCtx<'ctx, 'a> {
    // == FROM CompilationUnitCtx ==
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
    pub module: &'a Module<'ctx>,

    /// The LLVM function we are building in
    pub fn_value: FunctionValue<'ctx>,
}
impl<'ctx, 'a> AsCompilationUnitCtx<'ctx, 'a> for FunctionCtx<'ctx, 'a> {
    fn ctx(&self) -> &'ctx Context {
        self.ctx
    }
    fn target_machine(&self) -> &'a TargetMachine {
        self.target_machine
    }
    fn builder(&self) -> &'a Builder<'ctx> {
        self.builder
    }
    fn line_lookup(&self) -> &'a LineLookup {
        self.line_lookup
    }
    fn dbg_builder(&self) -> &'a DebugInfoBuilder<'ctx> {
        self.dbg_builder
    }
    fn compilation_unit(&self) -> &'a DICompileUnit<'ctx> {
        self.compilation_unit
    }
    fn module(&self) -> &'a Module<'ctx> {
        self.module
    }
}
impl<'ctx, 'a> FunctionCtx<'ctx, 'a> {
    /// Create a function context from a [`CompilationUnitCtx`] and
    /// [`FunctionValue`]
    pub const fn from_unit_and_fn(
        unit: CompilationUnitCtx<'ctx, 'a>,
        fn_value: FunctionValue<'ctx>,
    ) -> Self {
        Self {
            ctx: unit.ctx,
            target_machine: unit.target_machine,
            builder: unit.builder,
            line_lookup: unit.line_lookup,
            dbg_builder: unit.dbg_builder,
            compilation_unit: unit.compilation_unit,
            module: unit.module,
            fn_value,
        }
    }
}
