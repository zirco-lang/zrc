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
    clippy::implicit_clone,
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
    clippy::unwrap_used
)]
#![allow(
    clippy::multiple_crate_versions,
    clippy::cargo_common_metadata,
    unused_crate_dependencies,
    clippy::module_name_repetitions,
    clippy::doc_comment_double_space_linebreaks,
    clippy::missing_errors_doc,
    clippy::result_large_err
)]

mod cli;

use std::{error::Error, path::Path};

use clap::Parser;
use repline::{Response, prebaked::read_and_mut};
use zrc_parser::{lexer, parser};
use zrc_typeck::typeck::{self, GlobalScope};

use crate::cli::Cli;

/// Get the current zrx version.
fn version_string() -> String {
    zrc_buildinfo::generate_version_string(env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"))
}

/// Wrap a function that returns a Diagnostic in order to print the diagnostic
fn diag_wrapper<F, T>(cb: F, source: Option<&str>) -> Result<T, ()>
where
    F: FnOnce() -> Result<T, zrc_diagnostics::Diagnostic>,
{
    match cb() {
        Ok(val) => Ok(val),
        Err(diag) => {
            println!("{}", diag.print(source));
            Err(())
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// The mode of the REPL.
enum Mode {
    /// Declaration mode
    Decl,
    /// Statement mode
    Stmt,
    /// Expression mode
    Expr,
    /// Type mode
    Type,
}

impl Mode {
    /// Get the Repline prompt color and string for this mode.
    const fn prompt_data(self) -> (&'static str, &'static str) {
        match self {
            Self::Decl => (ansi_color_constants::L_RED, "decl> "),
            Self::Stmt => (ansi_color_constants::L_BLUE, "stmt> "),
            Self::Expr => (ansi_color_constants::L_GREEN, "expr> "),
            Self::Type => (ansi_color_constants::L_YELLOW, "type> "),
        }
    }
}

/// Diff a scope, to recognize and render any newly added or changed items.
fn diff_scope<'input>(old: &typeck::Scope<'input>, new: &typeck::Scope<'input>) -> String {
    // For scope.values and scope.types, list any Created (green + lines) or Changed
    // (minus and plus). They will never be deleted.

    let mut output: Vec<String> = Vec::new();

    for (name, old_item) in old.types.iter() {
        if !new.types.has(name) {
            output.push(format!(
                "{}- type {name}: {}{}",
                ansi_color_constants::L_RED,
                old_item.clone(),
                ansi_color_constants::RESET
            ));
        }
    }

    for (name, new_item) in new.types.iter() {
        let old_item = old.types.resolve(name);

        match old_item {
            None => {
                // Item created
                output.push(format!(
                    "{}+ type {name}: {}{}",
                    ansi_color_constants::L_GREEN,
                    new_item.clone(),
                    ansi_color_constants::RESET
                ));
            }
            Some(old_item) if old_item != new_item => {
                // Item changed
                output.push(format!(
                    "{}- type {name}: {}{}",
                    ansi_color_constants::L_RED,
                    old_item.clone(),
                    ansi_color_constants::RESET
                ));
                output.push(format!(
                    "{}+ type {name}: {}{}",
                    ansi_color_constants::L_GREEN,
                    new_item.clone(),
                    ansi_color_constants::RESET
                ));
            }
            Some(_) => {
                // Item unchanged
            }
        }
    }

    for (name, old_item) in old.values.iter() {
        if !new.values.has(name) {
            let rc = old_item.clone();
            let ent = rc.borrow();
            output.push(format!(
                "{}- value {name}: {} (const={}){}",
                ansi_color_constants::L_RED,
                ent.ty,
                ent.is_constant,
                ansi_color_constants::RESET
            ));
        }
    }

    for (name, new_item) in new.values.iter() {
        let old_item = old.values.resolve(name);

        match old_item {
            None => {
                let rc = new_item.clone();
                let ent = rc.borrow();
                // Item created
                output.push(format!(
                    "{}+ value {name}: {} (const={}){}",
                    ansi_color_constants::L_GREEN,
                    ent.ty,
                    ent.is_constant,
                    ansi_color_constants::RESET
                ));
            }
            Some(old_item) if old_item != new_item => {
                // Item changed
                let old_rc = old_item.clone();
                let old_ent = old_rc.borrow();
                let new_rc = new_item.clone();
                let new_ent = new_rc.borrow();
                output.push(format!(
                    "{}- value {name}: {} (const={}){}",
                    ansi_color_constants::L_RED,
                    old_ent.ty,
                    old_ent.is_constant,
                    ansi_color_constants::RESET
                ));
                output.push(format!(
                    "{}+ value {name}: {} (const={}){}",
                    ansi_color_constants::L_GREEN,
                    new_ent.ty,
                    new_ent.is_constant,
                    ansi_color_constants::RESET
                ));
            }
            Some(_) => {
                // Item unchanged
            }
        }
    }

    output.join("\n")
}

/// Diff a global scope
#[expect(clippy::format_push_string)]
fn diff_gs<'input>(old: &GlobalScope<'input>, new: &GlobalScope<'input>) -> String {
    let old_scope = old.create_subscope();
    let new_scope = new.create_subscope();
    let mut result = diff_scope(&old_scope, &new_scope);

    for (name, old_gdec) in &old.declarations {
        if !new.declarations.contains_key(name) {
            result += &format!(
                "\n{}- global fn {name} has impl={}{}",
                ansi_color_constants::L_RED,
                old_gdec.has_implementation,
                ansi_color_constants::RESET
            );
        }
    }

    for (name, new_gdec) in &new.declarations {
        let old_gdec = old.declarations.get(name);

        match old_gdec {
            None => {
                // Item created
                result += &format!(
                    "\n{}+ global fn {name} has impl={}{}",
                    ansi_color_constants::L_GREEN,
                    new_gdec.has_implementation,
                    ansi_color_constants::RESET
                );
            }
            Some(old_gdec) if old_gdec != new_gdec => {
                // Item changed
                result += &format!(
                    "\n{}- global fn {name} has impl={}{}",
                    ansi_color_constants::L_RED,
                    old_gdec.has_implementation,
                    ansi_color_constants::RESET
                );
                result += &format!(
                    "\n{}+ global fn {name} has impl={}{}",
                    ansi_color_constants::L_GREEN,
                    new_gdec.has_implementation,
                    ansi_color_constants::RESET
                );
            }
            Some(_) => {
                // Item unchanged
            }
        }
    }

    result
}

/// Call the #help cmd
fn handle_help() -> Response {
    println!("Available commands:");
    println!("  .help       Show this help message");
    println!("  .exit       Exit the REPL");
    println!("  .quit       Exit the REPL");
    println!("  .decl       Switch to declaration mode");
    println!("  .stmt       Switch to statement mode");
    println!("  .expr       Switch to expression mode");
    println!("  .type       Switch to type mode");
    println!("  .reset      Reset the global scope");
    println!("  #include <file>   Include declarations from a file");
    Response::Accept
}

/// Call the #include cmd (zpp)
fn handle_include(line: &str, include_paths: &'static [&Path], gs: &mut GlobalScope) -> Response {
    // feed it to the preprocessor
    let chunks = diag_wrapper(
        || zrc_preprocessor::preprocess(Path::new("/dev"), include_paths, "<stdin>", line),
        Some(line),
    );
    let Ok(chunks) = chunks else {
        // Repline does not like it when you Continue or Reject after printing
        return Response::Accept;
    };

    // HACK: Leak the chunks to get 'static lifetimes (which is our 'input)
    let chunks = Box::leak(Box::new(chunks));

    let mut ast = Vec::new();
    for chunk in chunks {
        let decls = diag_wrapper(|| parser::parse_source_chunk(chunk), None);
        let Ok(decls) = decls else {
            // Repline does not like it when you Continue or Reject after printing
            return Response::Accept;
        };

        ast.extend(decls);
    }

    let old_gs = gs.clone();

    let typed_ast = diag_wrapper(|| typeck::type_program(gs, ast), Some(line));

    let Ok(_typed_ast) = typed_ast else {
        // Repline does not like it when you Continue or Reject after printing
        return Response::Accept;
    };

    println!("{}", diff_gs(&old_gs, gs));

    Response::Accept
}

#[expect(clippy::too_many_lines)]
fn main() -> Result<(), Box<dyn Error>> {
    let cli = Cli::parse();

    if cli.version {
        print!("{}", version_string());
        return Ok(());
    }

    println!("Welcome to {}", version_string());
    println!("Type .help for more information.");
    println!("zrepl currently only supports type checking - not expression evaluation.");
    println!();

    // Spin up Zirco stuff. Prepare things the preprocessor needs:
    let include_paths = Box::leak(Box::new(cli::get_include_paths(&cli)));
    let mut gs = GlobalScope::new();
    let mut local_scope: Option<typeck::Scope> = None;

    let mut mode = Mode::Decl;

    read_and_mut(
        mode.prompt_data().0,
        mode.prompt_data().1,
        "  ... ",
        |repl, line| {
            match (mode, line.trim()) {
                (_, ".help") => Ok(handle_help()),
                (_, ".exit" | ".quit" | ".q") => Ok(Response::Break),
                (_, "") => Ok(Response::Accept),
                (Mode::Decl, ".d" | ".de" | ".decl") => {
                    // Don't do anything because diffing the scope would cause a None unwrap
                    Ok(Response::Accept)
                }
                (_, ".d" | ".de" | ".decl") => {
                    mode = Mode::Decl;
                    println!(
                        "{}",
                        diff_scope(
                            local_scope.as_ref().expect("we are in a subscope"),
                            &gs.create_subscope()
                        )
                    );
                    local_scope = None;
                    println!("Exiting statement scope - discarding locals.");
                    let (color, prompt) = mode.prompt_data();
                    repl.set_color(color);
                    repl.set_begin(prompt);
                    Ok(Response::Accept)
                }
                (_, ".s" | ".st" | ".stmt") => {
                    if local_scope.is_none() {
                        local_scope = Some(gs.create_subscope());
                    }
                    mode = Mode::Stmt;
                    let (color, prompt) = mode.prompt_data();
                    repl.set_color(color);
                    repl.set_begin(prompt);
                    Ok(Response::Accept)
                }
                (_, ".e" | ".ex" | ".expr") => {
                    if local_scope.is_none() {
                        local_scope = Some(gs.create_subscope());
                    }
                    mode = Mode::Expr;
                    let (color, prompt) = mode.prompt_data();
                    repl.set_color(color);
                    repl.set_begin(prompt);
                    Ok(Response::Accept)
                }
                (_, ".t" | ".ty" | ".type") => {
                    if local_scope.is_none() {
                        local_scope = Some(gs.create_subscope());
                    }
                    mode = Mode::Type;
                    let (color, prompt) = mode.prompt_data();
                    repl.set_color(color);
                    repl.set_begin(prompt);
                    Ok(Response::Accept)
                }
                (Mode::Decl, ".reset") => {
                    let old_gs = gs.clone();
                    gs = GlobalScope::new();
                    println!("Global scope reset.");
                    println!("{}", diff_gs(&old_gs, &gs));
                    Ok(Response::Accept)
                }
                (_, ".reset") => {
                    println!("The .reset command is only available in declaration mode.");
                    Ok(Response::Accept)
                }
                (Mode::Decl, line) if line == "#include" || line.starts_with("#include ") => {
                    Ok(handle_include(line, include_paths, &mut gs))
                }
                (_, line) if line == "#include" || line.starts_with("#include ") => {
                    println!("The #include command is only available in declaration mode.");
                    Ok(Response::Accept)
                }
                (Mode::Decl, decl) => {
                    // 'input is 'static in the REPL, so we can leak the string
                    let decl: &'static str = Box::leak(Box::new(decl.to_string()));

                    let balanced = lexer::are_delimiters_balanced(decl);
                    if !balanced {
                        return Err("unbalanced delimiters".into());
                    }

                    let decls =
                        diag_wrapper(|| parser::parse_program(decl, "/dev/<stdin>"), Some(decl));
                    let Ok(decls) = decls else {
                        return Ok(Response::Accept);
                    };

                    let old_gs = gs.clone();

                    let typed_decls =
                        diag_wrapper(|| typeck::type_program(&mut gs, decls), Some(decl));
                    let Ok(_typed_decls) = typed_decls else {
                        println!("Diagnostic encountered - restoring old scope");
                        gs = old_gs;
                        return Ok(Response::Accept);
                    };

                    println!("{}", diff_gs(&old_gs, &gs));

                    Ok(Response::Accept)
                }
                (Mode::Stmt, stmt) => {
                    // 'input is 'static in the REPL, so we can leak the string
                    let stmt: &'static str = Box::leak(Box::new(stmt.to_string()));

                    let balanced = lexer::are_delimiters_balanced(stmt);
                    if !balanced {
                        return Err("unbalanced delimiters".into());
                    }

                    let stmts =
                        diag_wrapper(|| parser::parse_stmt_list(stmt, "/dev/<stdin>"), Some(stmt));
                    let Ok(stmts) = stmts else {
                        return Ok(Response::Accept);
                    };

                    let scope = local_scope.as_mut().expect("We're in stmt mode");

                    let old_scope = scope.clone();

                    let typed_stmts = diag_wrapper(
                        || {
                            typeck::type_block(
                                scope,
                                stmts,
                                false,
                                typeck::BlockReturnAbility::MustNotReturn,
                            )
                        },
                        Some(stmt),
                    );
                    let Ok(typed_stmts) = typed_stmts else {
                        println!("Diagnostic encountered - restoring old scope");
                        local_scope = Some(old_scope);
                        return Ok(Response::Accept);
                    };
                    // Extract the scope back out of the implicit block formed
                    local_scope = Some(typed_stmts.scope);

                    println!(
                        "{}",
                        diff_scope(&old_scope, local_scope.as_ref().expect("We are in a stmt"))
                    );

                    Ok(Response::Accept)
                }
                (Mode::Expr, expr) => {
                    // 'input is 'static in the REPL, so we can leak the string
                    let expr: &'static str = Box::leak(Box::new(expr.to_string()));

                    let balanced = lexer::are_delimiters_balanced(expr);
                    if !balanced {
                        return Err("unbalanced delimiters".into());
                    }

                    let expr_ast =
                        diag_wrapper(|| parser::parse_expr(expr, "/dev/<stdin>"), Some(expr));
                    let Ok(expr_ast) = expr_ast else {
                        return Ok(Response::Accept);
                    };

                    let tast = diag_wrapper(
                        || {
                            typeck::type_expr(
                                local_scope.as_mut().expect("We are in an expr position"),
                                expr_ast,
                            )
                        },
                        Some(expr),
                    );
                    let Ok(tast) = tast else {
                        return Ok(Response::Accept);
                    };

                    println!("{tast}");
                    println!("-> {}", tast.inferred_type);

                    Ok(Response::Accept)
                }
                (Mode::Type, ty) => {
                    // 'input is 'static in the REPL, so we can leak the string
                    let ty: &'static str = Box::leak(Box::new(ty.to_string()));

                    let balanced = lexer::are_delimiters_balanced(ty);
                    if !balanced {
                        return Err("unbalanced delimiters".into());
                    }

                    let tys = diag_wrapper(|| parser::parse_type(ty, "/dev/<stdin>"), Some(ty));
                    let Ok(tys) = tys else {
                        return Ok(Response::Accept);
                    };

                    let resolved_ty = diag_wrapper(
                        || typeck::resolve_type(&gs.create_subscope(), tys),
                        Some(ty),
                    );
                    let Ok(resolved_ty) = resolved_ty else {
                        return Ok(Response::Accept);
                    };

                    println!("type {ty}: {resolved_ty}");

                    Ok(Response::Accept)
                }
            }
        },
    )?;

    Ok(())
}
