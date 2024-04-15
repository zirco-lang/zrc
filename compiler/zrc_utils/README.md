# common utilities used in `zrc`

This crate exists to bring some of the utility types like [`span::Spanned`] and [`span::Span`] to
a common place to prevent circular dependencies on things like `zrc_parser` (where the `Spanned`
type used to be declared).

Read the module-level documentation for more details.
