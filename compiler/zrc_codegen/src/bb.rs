//! Declares the `BasicBlockAnd` struct and related utilities
//!
//! In the Zirco code generator, it is very common for a function to need to
//! return both a value and a new basic block, since LLVM IR is in SSA form and
//! control flow is explicit. This module provides the [`BasicBlockAnd`] struct
//! to encapsulate both a basic block and a value, along with utilities to make
//! it easy to work with.

// Ordering matters! Declared here so other modules have access
/// Convenience macro to unpack a `BasicBlockAnd` -- assigns to the provided
/// `bb` and yields the value
///
/// # Example
///
/// ```ignore
/// let BasicBlockAnd {bb, value} = some_function_returning_bb_and(old_bb);
/// // is equivalent to
/// let value = unpack!(bb = some_function_returning_bb_and(old_bb));
/// ```
#[macro_export]
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

use inkwell::basic_block::BasicBlock;

/// Represents some value along with a basic block.
///
/// In the Zirco code generator, it is very common for a function to need to
/// return both a value and a new basic block, since LLVM IR is in SSA form and
/// control flow is explicit. This struct encapsulates both a basic block and a
/// value, allowing functions to return them together.
pub struct BasicBlockAnd<'ctx, T> {
    /// The basic block returned
    ///
    /// This represents the current point in the control flow of the
    /// LLVM IR being generated.
    pub bb: BasicBlock<'ctx>,
    /// Any other data the function wishes to pass
    ///
    /// This is typically some LLVM value, such as a
    /// [`inkwell::values::BasicValue`], but can be any type.
    pub value: T,
}
impl<T> BasicBlockAnd<'_, T> {
    /// Discard the basic block and return the value
    ///
    /// This is useful when you only care about the value and not the
    /// control flow.
    #[must_use]
    pub fn into_value(self) -> T {
        self.value
    }
}
/// Extends Inkwell [`BasicBlock`]s with a method to easily produce a
/// [`BasicBlockAnd`] value
///
/// This is useful for chaining together functions that return both a
/// basic block and a value.
pub trait BasicBlockExt<'ctx> {
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
