use anyhow::bail;

use super::{
    cg_alloc, cg_expr, cg_load, get_llvm_typename, BasicBlock, CgScope, FunctionCg, ModuleCg,
};

/// Consists of the [`BasicBlock`]s to `br` to when encountering certain
/// instructions. It is passed to [`cg_block`] to allow it to properly handle
/// break and continue.
#[derive(PartialEq, Debug, Clone)]
pub struct LoopBreakaway {
    /// Points to the exit basic block.
    on_break: BasicBlock,
    /// For `for` loops, points to the latch. For `while` loops, points to the
    /// header.
    on_continue: BasicBlock,
}

/// Declares the variable, creating its allocation and also evaluating the
/// assignment.
pub fn cg_let_declaration(
    module: &mut ModuleCg,
    cg: &mut FunctionCg,
    bb: &BasicBlock,
    scope: &mut CgScope,
    declarations: Vec<zrc_typeck::tast::stmt::LetDeclaration>,
) -> anyhow::Result<BasicBlock> {
    let mut bb = bb.clone();

    for let_declaration in declarations {
        // allocate space for it
        let ptr = cg_alloc(cg, &bb, &get_llvm_typename(let_declaration.ty.clone()));

        // store it in scope
        scope.insert(&let_declaration.name, &ptr);

        if let Some(value) = let_declaration.value {
            bb = cg_expr(
                module,
                cg,
                &bb,
                scope,
                zrc_typeck::tast::expr::TypedExpr(
                    let_declaration.ty.clone(),
                    zrc_typeck::tast::expr::TypedExprKind::Assignment(
                        Box::new(zrc_typeck::tast::expr::Place(
                            let_declaration.ty,
                            zrc_typeck::tast::expr::PlaceKind::Variable(let_declaration.name),
                        )),
                        Box::new(value),
                    ),
                ),
            )?
            .1;
        }
    }

    Ok(bb)
}

/// Returns the basic block to continue adding instructions to.
/// If it is None, a return statement was encountered and no more instructions
/// should be added.
pub fn cg_block(
    module: &mut ModuleCg,
    cg: &mut FunctionCg,
    bb: &BasicBlock,
    parent_scope: &CgScope,
    block: Vec<zrc_typeck::tast::stmt::TypedStmt>,
    // If set to None, break and continue are invalid.
    // If set to Some, break and continue will break to the bbs listed in there.
    breakaway: Option<LoopBreakaway>,
) -> anyhow::Result<Option<BasicBlock>> {
    use zrc_typeck::tast::stmt::*;

    let mut scope = parent_scope.clone();

    block.into_iter().try_fold(
        Some(bb.clone()),
        |bb, stmt| -> anyhow::Result<Option<BasicBlock>> {
            let Some(bb) = bb else {
                // we hit a return statement already, so we're done
                return Ok(None);
            };
            Ok(match stmt {
                TypedStmt::EmptyStmt => Some(bb),
                TypedStmt::ExprStmt(expr) => Some(cg_expr(module, cg, &bb, &scope, expr)?.1),
                TypedStmt::IfStmt(cond, then, then_else) => {
                    let (cond_ptr, bb) = cg_expr(module, cg, &bb, &scope, cond)?;

                    let then_else = then_else.unwrap_or(vec![]);

                    let then_bb = cg.new_bb();
                    let then_else_bb = cg.new_bb();

                    let cond_reg = cg_load(cg, &bb, "i1", &cond_ptr)?;
                    bb.add_instruction(
                        cg,
                        &format!("br i1 {cond_reg}, label {then_bb}, label {then_else_bb}",),
                    )?;

                    let then_bb = cg_block(module, cg, &then_bb, &scope, then, breakaway.clone())?;
                    let then_else_bb = cg_block(
                        module,
                        cg,
                        &then_else_bb,
                        &scope,
                        then_else,
                        breakaway.clone(),
                    )?;

                    match (then_bb, then_else_bb) {
                        (Some(then_bb), Some(then_else_bb)) => {
                            let terminating_bb = cg.new_bb();
                            then_bb.add_instruction(cg, &format!("br label {terminating_bb}"))?;
                            then_else_bb
                                .add_instruction(cg, &format!("br label {terminating_bb}"))?;

                            Some(terminating_bb)
                        }
                        (Some(then_bb), None) => {
                            let terminating_bb = cg.new_bb();
                            then_bb.add_instruction(cg, &format!("br label {terminating_bb}"))?;
                            Some(terminating_bb)
                        }
                        (None, Some(then_else_bb)) => {
                            let terminating_bb = cg.new_bb();
                            then_else_bb
                                .add_instruction(cg, &format!("br label {terminating_bb}"))?;
                            Some(terminating_bb)
                        }
                        (None, None) => None,
                    }
                }

                TypedStmt::BlockStmt(body) => {
                    cg_block(module, cg, &bb, &scope, body, breakaway.clone())?
                }

                TypedStmt::ReturnStmt(Some(ex)) => {
                    let (ex_ptr, bb) = cg_expr(module, cg, &bb, &scope, ex.clone())?;
                    let ex_type = get_llvm_typename(ex.0);
                    let ex_reg = cg_load(cg, &bb, &ex_type, &ex_ptr)?;
                    bb.add_instruction(cg, &format!("ret {ex_type} {ex_reg}"))?;

                    None
                }
                TypedStmt::ReturnStmt(None) => {
                    bb.add_instruction(cg, "ret void")?;
                    None
                }

                TypedStmt::ContinueStmt => {
                    // We can jump into whatever the 'continue' target is in `breakaway`
                    // This is going to be the loop header for `while` loops and the loop latch for
                    // `for` loops

                    match breakaway.clone() {
                        Some(LoopBreakaway { on_continue, .. }) => {
                            bb.add_instruction(cg, &format!("br label {on_continue}"))?;
                            // make sure to use 'bb' here not 'on_continue' so later statements are
                            // appended to this block
                            None
                        }
                        None => bail!("continue statement outside of loop"),
                    }
                }

                TypedStmt::BreakStmt => {
                    // Jump into the 'break' target in `breakaway`
                    // This is the exit portion of the loop

                    match breakaway.clone() {
                        Some(LoopBreakaway { on_break, .. }) => {
                            bb.add_instruction(cg, &format!("br label {on_break}"))?;
                            None
                        }
                        None => bail!("break statement outside of loop"),
                    }
                }

                TypedStmt::DeclarationList(d) => {
                    Some(cg_let_declaration(module, cg, &bb, &mut scope, d)?)
                }

                TypedStmt::ForStmt {
                    init,
                    cond,
                    post,
                    body: body_code,
                } => {
                    // For loops generate a somewhat more complicated CFG, with a few parts.
                    // The preheader, where `init` runs. Breaks to the header.
                    // The header, where `cond` is checked and breaks to either the exit or the
                    // body. The body, where most of the body runs. Breaks to
                    // the latch. `break` transfers to the exit by force and `continue` transfers to
                    // the latch by force. The latch, where `post` runs and
                    // breaks back to the header The exit, which is the basic
                    // block we return.

                    // loops lie in an implicit subscope
                    let mut scope = scope.clone();

                    // The block we are currently in will become the preheader. Generate the `init`
                    // code if there is any.
                    let bb = match init {
                        None => bb,
                        Some(init) => cg_let_declaration(module, cg, &bb, &mut scope, *init)?,
                    };

                    let header = cg.new_bb();
                    bb.add_instruction(cg, &format!("br label {header}"))?;

                    let body = cg.new_bb();
                    let latch = cg.new_bb();
                    let exit = cg.new_bb();

                    // You are now officially in the loop. Within the header, we check `cond` and
                    // use that to jump to either the body or the exit condition.
                    let header = match cond {
                        None => {
                            header.add_instruction(cg, &format!("br label {body}"))?;
                            header
                        }
                        Some(cond) => {
                            let (cond_ptr, header) =
                                cg_expr(module, cg, &header, &scope, cond.clone())?;

                            let cond_reg =
                                cg_load(cg, &header, &get_llvm_typename(cond.0), &cond_ptr)?;

                            header.add_instruction(
                                cg,
                                &format!("br i1 {cond_reg}, label {body}, label {exit}"),
                            )?;

                            header
                        }
                    };

                    // Generate the body
                    let body = cg_block(
                        module,
                        cg,
                        &body,
                        &scope,
                        body_code,
                        Some(LoopBreakaway {
                            on_break: exit.clone(),
                            on_continue: latch.clone(),
                        }),
                    )?;

                    // The body breaks to latch
                    if let Some(body) = body {
                        body.add_instruction(cg, &format!("br label {latch}"))?;
                    }

                    // Latch runs post and then breaks right back to the header.
                    let latch = if let Some(post) = post {
                        cg_expr(module, cg, &latch, &scope, post)?.1
                    } else {
                        latch
                    };

                    latch.add_instruction(cg, &format!("br label {header}"))?;

                    Some(exit)
                }
                TypedStmt::WhileStmt(cond, body_code) => {
                    // While loops are similar to for loops but much simpler.
                    // The preheader simply just breaks to the header.
                    // The header checks the condition and breaks to the exit or the body.
                    // The body simply breaks to the header.
                    // The exit is the continued code

                    // `break` => exit
                    // `continue` => header

                    let header = cg.new_bb();
                    bb.add_instruction(cg, &format!("br label {header}"))?;

                    let body = cg.new_bb();
                    let exit = cg.new_bb();

                    let (cond_ptr, header) = cg_expr(module, cg, &header, &scope, cond.clone())?;
                    let cond_reg = cg_load(cg, &header, &get_llvm_typename(cond.0), &cond_ptr)?;

                    header.add_instruction(
                        cg,
                        &format!("br i1 {cond_reg}, label {body}, label {exit}"),
                    )?;

                    let body = cg_block(
                        module,
                        cg,
                        &body,
                        &scope,
                        body_code,
                        Some(LoopBreakaway {
                            on_break: exit.clone(),
                            on_continue: header.clone(),
                        }),
                    )?;

                    // The body breaks to header
                    if let Some(body) = body {
                        body.add_instruction(cg, &format!("br label {header}"))?;
                    }

                    Some(exit)
                }
            })
        },
    )
}

pub fn cg_program(
    program: Vec<zrc_typeck::tast::stmt::TypedDeclaration>,
) -> anyhow::Result<String> {
    let mut module = ModuleCg::new();
    let mut global_scope = CgScope::new();

    for declaration in program {
        match declaration {
            // Struct declarations don't need code generation as they're all inlined
            zrc_typeck::tast::stmt::TypedDeclaration::StructDeclaration { .. } => {}

            zrc_typeck::tast::stmt::TypedDeclaration::FunctionDeclaration {
                name,
                parameters,
                return_type,
                body: Some(body),
            } => {
                // Must happen before creating `cg` as FunctionCg::new() clones it and
                // recursion would be impossible w/o this
                global_scope.insert(&name, &format!("@{name}"));

                let (mut cg, bb, fn_scope) = FunctionCg::new(
                    format!("@{name}"),
                    match return_type {
                        Some(x) => zrc_typeck::typeck::BlockReturnType::Return(x),
                        None => zrc_typeck::typeck::BlockReturnType::Void,
                    },
                    parameters.into_iter().map(|x| (x.name, x.ty)).collect(),
                    &global_scope,
                );

                cg_block(&mut module, &mut cg, &bb, &fn_scope, body, None)?;

                module.declarations.push(cg.to_string());
            }

            zrc_typeck::tast::stmt::TypedDeclaration::FunctionDeclaration {
                name,
                parameters,
                return_type,
                body: None,
            } => {
                global_scope.insert(&name, &format!("@{name}"));

                module.declarations.push(format!(
                    "declare {} @{}({})",
                    get_llvm_typename(return_type.unwrap_or(zrc_typeck::tast::ty::Type::Void)),
                    name,
                    parameters
                        .iter()
                        .map(|zrc_typeck::tast::stmt::ArgumentDeclaration { ty, .. }| {
                            get_llvm_typename(ty.clone())
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                ));
            }
        }
    }

    Ok(module.to_string())
}
