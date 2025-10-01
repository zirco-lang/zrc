//! Defines the return status of a function

use crate::tast::ty::Type as TastType;

/// Describes if a block MAY, MUST, or MUST NOT return.
#[derive(Debug, Clone, PartialEq)]
pub enum BlockReturnAbility<'input> {
    /// The block MUST NOT return at any point.
    MustNotReturn,

    /// The block MAY return, but it is not required.
    ///
    /// Any sub-blocks of this block MAY return.
    MayReturn(TastType<'input>),

    /// The block MUST return.
    ///
    /// Any sub-blocks of this block MAY return. At least one MUST return.
    MustReturn(TastType<'input>),
}
impl BlockReturnAbility<'_> {
    /// Determine the [`BlockReturnAbility`] of a sub-scope. `MustReturn`
    /// become`MayReturn`.
    #[must_use]
    pub fn demote(self) -> Self {
        match self {
            Self::MustNotReturn => Self::MustNotReturn,
            Self::MayReturn(x) | Self::MustReturn(x) => Self::MayReturn(x),
        }
    }
}

/// Describes if a block labeled [MAY return](BlockReturnAbility::MayReturn)
/// actually returns.
///
/// This is necessary for determining the fulfillment of a [MUST
/// return](BlockReturnAbility::MustReturn) when a block contains a nested block
/// (because the outer block must have at least *one* path which is guaranteed
/// to return)
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum BlockReturnActuality {
    /// The block is guaranteed to never return on any path.
    NeverReturns,

    /// The block will return on some paths but not all. In some cases, this may
    /// be selected even if the block will always return, as it is sometimes
    /// unknown.
    SometimesReturns,

    /// The block is guaranteed to return on any path.
    AlwaysReturns,
}
impl BlockReturnActuality {
    /// Determine the [`BlockReturnActuality`] if a code path is not always
    /// guaranteed to execute. `AlwaysReturns` becomes `SometimesReturns`.
    #[must_use]
    pub const fn demote(self) -> Self {
        match self {
            Self::NeverReturns => Self::NeverReturns,
            Self::SometimesReturns | Self::AlwaysReturns => Self::SometimesReturns,
        }
    }

    /// Take two [`BlockReturnActuality`] instances corresponding to two
    /// different code paths: one or the other may execute (not neither and
    /// not both). Determine the [`BlockReturnActuality`] of this compound
    /// statement.
    ///
    /// Never + Never => Never
    /// Never + Sometimes => Sometimes
    /// Never + Always => Sometimes
    /// Sometimes + Sometimes => Sometimes
    /// Sometimes + Always => Sometimes
    /// Always + Always => Always
    #[must_use]
    #[allow(clippy::min_ident_chars)]
    pub const fn join(a: Self, b: Self) -> Self {
        match (a, b) {
            (Self::NeverReturns, Self::NeverReturns) => Self::NeverReturns,

            (
                Self::NeverReturns | Self::SometimesReturns,
                Self::SometimesReturns | Self::AlwaysReturns,
            )
            | (
                Self::SometimesReturns | Self::AlwaysReturns,
                Self::NeverReturns | Self::SometimesReturns,
            ) => Self::SometimesReturns,

            (Self::AlwaysReturns, Self::AlwaysReturns) => Self::AlwaysReturns,
        }
    }

    /// Join an iterator of [`BlockReturnActuality`] values.
    #[must_use]
    pub fn join_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = Self>,
    {
        iter.into_iter().fold(Self::NeverReturns, Self::join)
    }
}
