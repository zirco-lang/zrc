//! Declares the `BasicBlockAnd` struct and related utilities

// Ordering matters! Declared here so other modules have access
/// Convenience macro to unpack a `BasicBlockAnd` -- assigns to the provided
/// `bb` and yields the value
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
/// This is used for code generation functions that may produce new basic blocks
/// along with some result value.
pub struct BasicBlockAnd<'ctx, T> {
    /// The basic block returned
    pub bb: BasicBlock<'ctx>,
    /// Any other data the function wishes to pass
    pub value: T,
}
impl<T> BasicBlockAnd<'_, T> {
    /// Discard the basic block and return the value
    pub fn into_value(self) -> T {
        self.value
    }
}
/// Extends Inkwell [`BasicBlock`]s with a method to easily produce a
/// [`BasicBlockAnd`] value
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
