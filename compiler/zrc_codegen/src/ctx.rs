//! Structures used internally within the code generator for context and state
//! management

use inkwell::{
    builder::Builder,
    context::Context,
    debug_info::{DICompileUnit, DILexicalBlock, DebugInfoBuilder},
    module::Module,
    targets::TargetMachine,
    values::FunctionValue,
};
use zrc_utils::line_finder::LineLookup;

use crate::CgScope;

/// Trait for any context with at least the fields of [`CompilationUnitCtx`]
#[allow(dead_code)]
pub trait AsCompilationUnitCtx<'ctx: 'a, 'a> {
    /// Convert some specific context to a [`CompilationUnitCtx`]
    fn as_unit_ctx(&self) -> CompilationUnitCtx<'ctx, 'a>;

    /// The LLVM context
    fn ctx(&self) -> &'ctx Context {
        self.as_unit_ctx().ctx
    }
    /// The LLVM target machine
    fn target_machine(&self) -> &'a TargetMachine {
        self.as_unit_ctx().target_machine
    }
    /// The LLVM builder used to build instructions
    fn builder(&self) -> &'a Builder<'ctx> {
        self.as_unit_ctx().builder
    }
    /// The lookup for lines in the source file
    fn line_lookup(&self) -> &'a LineLookup {
        self.as_unit_ctx().line_lookup
    }
    /// The LLVM builder for debug info
    fn dbg_builder(&self) -> &'a DebugInfoBuilder<'ctx> {
        self.as_unit_ctx().dbg_builder
    }
    /// The LLVM compile unit for debug info
    fn compilation_unit(&self) -> &'a DICompileUnit<'ctx> {
        self.as_unit_ctx().compilation_unit
    }
    /// The LLVM module we are building in
    fn module(&self) -> &'a Module<'ctx> {
        self.as_unit_ctx().module
    }
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
    fn as_unit_ctx(&self) -> CompilationUnitCtx<'ctx, 'a> {
        *self
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
    fn as_unit_ctx(&self) -> CompilationUnitCtx<'ctx, 'a> {
        CompilationUnitCtx {
            ctx: self.ctx,
            target_machine: self.target_machine,
            builder: self.builder,
            line_lookup: self.line_lookup,
            dbg_builder: self.dbg_builder,
            compilation_unit: self.compilation_unit,
            module: self.module,
        }
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

/// An extension of [`FunctionCtx`] containing values found in an individual
/// lexical block (except `bb`, as this is mutated often)
#[derive(Debug, Clone, Copy)]
pub struct BlockCtx<'ctx, 'input, 'a> {
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

    // == FROM FunctionCtx ==
    /// The LLVM function we are building in
    pub fn_value: FunctionValue<'ctx>,

    /// The code generation type/value scope this block lives in
    pub scope: &'a CgScope<'input, 'ctx>,
    /// The LLVM [`DILexicalBlock`] we're currently in
    pub dbg_scope: DILexicalBlock<'ctx>,
}
impl<'ctx, 'input, 'a> AsCompilationUnitCtx<'ctx, 'a> for BlockCtx<'ctx, 'input, 'a> {
    fn as_unit_ctx(&self) -> CompilationUnitCtx<'ctx, 'a> {
        CompilationUnitCtx {
            ctx: self.ctx,
            target_machine: self.target_machine,
            builder: self.builder,
            line_lookup: self.line_lookup,
            dbg_builder: self.dbg_builder,
            compilation_unit: self.compilation_unit,
            module: self.module,
        }
    }
}
impl<'ctx, 'input, 'a> BlockCtx<'ctx, 'input, 'a> {
    /// Create a new [`BlockCtx`] from a [`FunctionCtx`] and needed block
    /// scoping parameters
    pub const fn new(
        function_ctx: FunctionCtx<'ctx, 'a>,
        scope: &'a CgScope<'input, 'ctx>,
        dbg_scope: DILexicalBlock<'ctx>,
    ) -> Self {
        Self {
            ctx: function_ctx.ctx,
            target_machine: function_ctx.target_machine,
            builder: function_ctx.builder,
            line_lookup: function_ctx.line_lookup,
            dbg_builder: function_ctx.dbg_builder,
            compilation_unit: function_ctx.compilation_unit,
            module: function_ctx.module,
            fn_value: function_ctx.fn_value,
            scope,
            dbg_scope,
        }
    }
}
