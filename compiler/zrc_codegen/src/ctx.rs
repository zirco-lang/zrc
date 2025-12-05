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

use crate::scope::CgScope;

/// Trait for any context with at least the fields of [`CompilationUnitCtx`]
#[allow(dead_code)]
pub trait AsCompilationUnitCtx<'ctx: 'a, 'a> {
    /// Convert some specific context to a [`CompilationUnitCtx`]
    ///
    /// This is useful for accessing common fields without needing to
    /// explicitly pass around a `CompilationUnitCtx`.
    fn as_unit_ctx(&self) -> CompilationUnitCtx<'ctx, 'a>;

    /// The LLVM context
    ///
    /// This is useful for creating new LLVM types and values.
    fn ctx(&self) -> &'ctx Context {
        self.as_unit_ctx().ctx
    }
    /// The LLVM target machine
    ///
    /// This is useful for generating code specific to the target architecture.
    fn target_machine(&self) -> &'a TargetMachine {
        self.as_unit_ctx().target_machine
    }
    /// The LLVM builder used to build instructions
    ///
    /// This is useful for creating new LLVM instructions.
    fn builder(&self) -> &'a Builder<'ctx> {
        self.as_unit_ctx().builder
    }
    /// The lookup for lines in the source file
    ///
    /// This is useful for generating debug info and error messages.
    fn line_lookup(&self) -> &'a LineLookup {
        self.as_unit_ctx().line_lookup
    }
    /// The LLVM builder for debug info
    ///
    /// This is useful for creating debug info metadata.
    fn dbg_builder(&self) -> Option<&'a DebugInfoBuilder<'ctx>> {
        self.as_unit_ctx().dbg_builder
    }
    /// The LLVM compile unit for debug info
    ///
    /// This is useful for creating debug info metadata.
    fn compilation_unit(&self) -> Option<&'a DICompileUnit<'ctx>> {
        self.as_unit_ctx().compilation_unit
    }
    /// The LLVM module we are building in
    ///
    /// This is useful for adding new functions and global variables.
    fn module(&self) -> &'a Module<'ctx> {
        self.as_unit_ctx().module
    }
}

/// LLVM structures common to a single compilation unit (file)
///
/// This is passed to most code generation functions, as it contains
/// the common LLVM structures needed for code generation.
///
/// It does not contain function-specific or block-specific data, which
/// are contained in [`FunctionCtx`] and [`BlockCtx`], respectively.
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
    pub dbg_builder: Option<&'a DebugInfoBuilder<'ctx>>,
    /// The LLVM compile unit for debug info
    pub compilation_unit: Option<&'a DICompileUnit<'ctx>>,
    /// The LLVM module we are building in
    pub module: &'a Module<'ctx>,
}
impl<'ctx, 'a> AsCompilationUnitCtx<'ctx, 'a> for CompilationUnitCtx<'ctx, 'a> {
    fn as_unit_ctx(&self) -> Self {
        *self
    }
}

/// Common LLVM structures passed onto most code generation functions
///
/// This extends [`CompilationUnitCtx`] with the current function being
/// generated, as many code generation functions need to know the current
/// function in order to generate correct LLVM IR.
///
/// It does not contain block-specific data, which is contained in [`BlockCtx`].
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
    pub dbg_builder: Option<&'a DebugInfoBuilder<'ctx>>,
    /// The LLVM compile unit for debug info
    pub compilation_unit: Option<&'a DICompileUnit<'ctx>>,
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
    ///
    /// This is useful for creating a function context when starting
    /// code generation for a new function.
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

    /// Create a builder positioned at the entry block
    ///
    /// This is useful for allocating stack space that should live for the
    /// entire function, not just a specific basic block. The builder will be
    /// positioned before the first instruction in the entry block, or at the
    /// end if there are no instructions yet.
    ///
    /// This is commonly used for allocating temporaries that need to persist
    /// across loop iterations or other control flow.
    pub fn create_entry_block_builder(&self) -> Builder<'ctx> {
        let entry_block_builder = self.ctx.create_builder();
        let first_bb = self
            .fn_value
            .get_first_basic_block()
            .expect("function should have at least one basic block");

        match first_bb.get_first_instruction() {
            Some(first_instruction) => {
                entry_block_builder.position_before(&first_instruction);
            }
            None => {
                entry_block_builder.position_at_end(first_bb);
            }
        }

        entry_block_builder
    }
}

/// An extension of [`FunctionCtx`] containing values found in an individual
/// lexical block (except `bb`, as this is mutated often)
///
/// This is passed to code generation functions that need to know the current
/// function and block, as well as the current scope for variable lookups.
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
    pub dbg_builder: Option<&'a DebugInfoBuilder<'ctx>>,
    /// The LLVM compile unit for debug info
    pub compilation_unit: Option<&'a DICompileUnit<'ctx>>,
    /// The LLVM module we are building in
    pub module: &'a Module<'ctx>,

    // == FROM FunctionCtx ==
    /// The LLVM function we are building in
    pub fn_value: FunctionValue<'ctx>,

    /// The code generation type/value scope this block lives in
    pub scope: &'a CgScope<'input, 'ctx>,
    /// The LLVM [`DILexicalBlock`] we're currently in
    pub dbg_scope: Option<DILexicalBlock<'ctx>>,
}
impl<'ctx, 'a> AsCompilationUnitCtx<'ctx, 'a> for BlockCtx<'ctx, '_, 'a> {
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
        dbg_scope: Option<DILexicalBlock<'ctx>>,
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

    /// Create a builder positioned at the entry block
    ///
    /// This is useful for allocating stack space that should live for the
    /// entire function, not just a specific basic block. The builder will be
    /// positioned before the first instruction in the entry block, or at the
    /// end if there are no instructions yet.
    ///
    /// This is commonly used for allocating temporaries that need to persist
    /// across loop iterations or other control flow.
    pub fn create_entry_block_builder(&self) -> Builder<'ctx> {
        let entry_block_builder = self.ctx.create_builder();
        let first_bb = self
            .fn_value
            .get_first_basic_block()
            .expect("function should have at least one basic block");

        match first_bb.get_first_instruction() {
            Some(first_instruction) => {
                entry_block_builder.position_before(&first_instruction);
            }
            None => {
                entry_block_builder.position_at_end(first_bb);
            }
        }

        entry_block_builder
    }
}
