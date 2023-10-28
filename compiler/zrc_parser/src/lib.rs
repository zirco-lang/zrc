#![doc=include_str!("../README.md")]
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
    unreachable_pub,
    unsafe_op_in_unsafe_fn,
    unused_crate_dependencies,
    variant_size_differences,

    // These should be enabled in any non-user-facing code, like the parser, but not in the
    // frontend.
    clippy::print_stderr,
    clippy::print_stdout
)]
#![allow(clippy::multiple_crate_versions, clippy::cargo_common_metadata)]

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(
    /// Direct access to the LALRPOP parser. **Do not use this module.** Use the
    /// [`parser`] module instead.
    ///
    /// Again. **Don't use this.** The second you import this into your code,
    /// you're setting yourself up to shoot yourself in the foot. **Just use
    /// [`parser`].** There is almost *no* reason you would need this module
    /// instead, unless you need to handle the underlying errors manually,
    /// which I doubt.
    #[allow(
        clippy::all,
        clippy::nursery,
        clippy::pedantic,
        missing_docs,
        clippy::missing_docs_in_private_items,
        clippy::restriction,
        unreachable_pub
    )]
    internal_parser
);

pub mod ast;
pub mod lexer;
pub mod parser;
