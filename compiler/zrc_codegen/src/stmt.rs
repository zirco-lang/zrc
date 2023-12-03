//! Code generation for statements

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    memory_buffer::MemoryBuffer,
    module::Module,
    passes::{PassManager, PassManagerBuilder},
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
    },
    types::AnyType,
    values::{BasicValueEnum, FunctionValue},
    OptimizationLevel,
};
use zrc_typeck::tast::{
    expr::{Place, PlaceKind, TypedExpr, TypedExprKind},
    stmt::{ArgumentDeclaration, LetDeclaration, TypedDeclaration, TypedStmt},
    ty::Type,
};

use crate::{
    expr::cg_expr,
    ty::{create_fn, llvm_basic_type, llvm_type},
    CgScope,
};

/// Consists of the [`BasicBlock`]s to `br` to when encountering certain
/// instructions. It is passed to [`cg_block`] to allow it to properly handle
/// break and continue.
#[derive(PartialEq, Eq, Debug, Clone)]
struct LoopBreakaway<'ctx> {
    /// Points to the exit basic block.
    on_break: BasicBlock<'ctx>,
    /// For `for` loops, points to the latch. For `while` loops, points to the
    /// header.
    on_continue: BasicBlock<'ctx>,
}

/// Generates the `alloca`tion, `store` instruction, and adds a new identifier
/// to the [`CgScope`].
///
/// # Panics
/// Panics if an internal code generation error is encountered.
#[allow(clippy::trivially_copy_pass_by_ref, clippy::too_many_arguments)]
fn cg_let_declaration<'ctx, 'input, 'a>(
    ctx: &'ctx Context,
    target_machine: &TargetMachine,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
    function: &'a FunctionValue<'ctx>,
    bb: &'a BasicBlock<'ctx>,
    scope: &'a mut CgScope<'input, 'ctx>,
    declarations: Vec<LetDeclaration<'input>>,
) -> BasicBlock<'ctx> {
    let mut bb = *bb;

    for let_declaration in declarations {
        // we create our own builder here because we need to insert the alloca
        // at the beginning of the entry block, and that is easier than trying to
        // somehow save our position.

        let entry_block_builder = ctx.create_builder();
        let first_bb = function.get_first_basic_block().unwrap();
        #[allow(clippy::option_if_let_else)]
        match first_bb.get_first_instruction() {
            Some(first_instruction) => {
                entry_block_builder.position_before(&first_instruction);
            }
            None => {
                entry_block_builder.position_at_end(first_bb);
            }
        }

        let ptr = entry_block_builder
            .build_alloca(
                llvm_basic_type(ctx, target_machine, &let_declaration.ty),
                &format!("let_{}", let_declaration.name),
            )
            .unwrap();

        scope.insert(let_declaration.name, ptr);

        if let Some(value) = let_declaration.value {
            bb = cg_expr(
                ctx,
                target_machine,
                builder,
                module,
                function,
                &bb,
                scope,
                TypedExpr(
                    let_declaration.ty.clone(),
                    TypedExprKind::Assignment(
                        Box::new(Place(
                            let_declaration.ty,
                            PlaceKind::Variable(let_declaration.name),
                        )),
                        Box::new(value),
                    ),
                ),
            )
            .1;
        }
    }

    bb
}

/// Process a vector of [`TypedStmt`]s (a block) and handle each statement.
///
/// # Panics
/// Panics if an internal code generation error is encountered.
#[allow(
    clippy::too_many_arguments,
    clippy::too_many_lines,
    clippy::needless_pass_by_value,
    clippy::trivially_copy_pass_by_ref
)]
fn cg_block<'ctx, 'input, 'a>(
    ctx: &'ctx Context,
    target_machine: &TargetMachine,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
    function: &'a FunctionValue<'ctx>,
    bb: &'a BasicBlock<'ctx>,
    parent_scope: &'a CgScope<'input, 'ctx>,
    block: Vec<TypedStmt<'input>>,
    breakaway: &Option<LoopBreakaway<'ctx>>,
) -> Option<BasicBlock<'ctx>> {
    let mut scope = parent_scope.clone();

    block
        .into_iter()
        .try_fold(*bb, |bb, stmt| -> Option<BasicBlock> {
            match stmt {
                TypedStmt::ExprStmt(expr) => Some(
                    cg_expr(
                        ctx,
                        target_machine,
                        builder,
                        module,
                        function,
                        &bb,
                        &scope,
                        expr,
                    )
                    .1,
                ),

                TypedStmt::IfStmt(cond, then, then_else) => {
                    let then_else = then_else.unwrap_or(vec![]);

                    let (cond, _) = cg_expr(
                        ctx,
                        target_machine,
                        builder,
                        module,
                        function,
                        &bb,
                        &scope,
                        cond,
                    );

                    let then_bb = ctx.append_basic_block(*function, "then");
                    let then_else_bb = ctx.append_basic_block(*function, "then_else");

                    builder
                        .build_conditional_branch(cond.into_int_value(), then_bb, then_else_bb)
                        .unwrap();

                    builder.position_at_end(then_bb);
                    let maybe_then_bb = cg_block(
                        ctx,
                        target_machine,
                        builder,
                        module,
                        function,
                        &then_bb,
                        &scope,
                        then,
                        breakaway,
                    );

                    builder.position_at_end(then_else_bb);
                    let maybe_then_else_bb = cg_block(
                        ctx,
                        target_machine,
                        builder,
                        module,
                        function,
                        &then_else_bb,
                        &scope,
                        then_else,
                        breakaway,
                    );

                    match (maybe_then_bb, maybe_then_else_bb) {
                        (None, None) => None,
                        (Some(single_bb), None) | (None, Some(single_bb)) => {
                            let end = ctx.append_basic_block(*function, "end");

                            builder.position_at_end(single_bb);
                            builder.build_unconditional_branch(end).unwrap();

                            builder.position_at_end(end);
                            Some(end)
                        }
                        (Some(then_bb), Some(then_else_bb)) => {
                            let end = ctx.append_basic_block(*function, "end");

                            builder.position_at_end(then_bb);
                            builder.build_unconditional_branch(end).unwrap();

                            builder.position_at_end(then_else_bb);
                            builder.build_unconditional_branch(end).unwrap();

                            builder.position_at_end(end);
                            Some(end)
                        }
                    }
                }

                TypedStmt::BlockStmt(block) => cg_block(
                    ctx,
                    target_machine,
                    builder,
                    module,
                    function,
                    &bb,
                    &scope,
                    block,
                    breakaway,
                ),

                TypedStmt::ReturnStmt(Some(expr)) => {
                    let (expr, _) = cg_expr(
                        ctx,
                        target_machine,
                        builder,
                        module,
                        function,
                        &bb,
                        &scope,
                        expr,
                    );

                    builder.build_return(Some(&expr)).unwrap();

                    None
                }

                TypedStmt::ReturnStmt(None) => {
                    builder.build_return(None).unwrap();

                    None
                }

                TypedStmt::ContinueStmt => {
                    builder
                        .build_unconditional_branch(breakaway.as_ref().unwrap().on_continue)
                        .unwrap();

                    None
                }

                TypedStmt::BreakStmt => {
                    builder
                        .build_unconditional_branch(breakaway.as_ref().unwrap().on_break)
                        .unwrap();

                    None
                }

                TypedStmt::DeclarationList(declarations) => Some(cg_let_declaration(
                    ctx,
                    target_machine,
                    builder,
                    module,
                    function,
                    &bb,
                    &mut scope,
                    declarations,
                )),

                TypedStmt::ForStmt {
                    init,
                    cond,
                    post,
                    body,
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
                    if let Some(init) = init {
                        cg_let_declaration(
                            ctx,
                            target_machine,
                            builder,
                            module,
                            function,
                            &bb,
                            &mut scope,
                            *init,
                        );
                    }

                    let header = ctx.append_basic_block(*function, "header");
                    let body_bb = ctx.append_basic_block(*function, "body");
                    let latch = ctx.append_basic_block(*function, "latch");
                    let exit = ctx.append_basic_block(*function, "exit");

                    // Branch to the header from the preheader.
                    builder.build_unconditional_branch(header).unwrap();

                    // Generate the header.
                    builder.position_at_end(header);
                    let header = cond.map_or_else(
                        || {
                            // If there is no condition, we always branch to the body.
                            builder.build_unconditional_branch(body_bb).unwrap();

                            header
                        },
                        |cond| {
                            let (cond, header) = cg_expr(
                                ctx,
                                target_machine,
                                builder,
                                module,
                                function,
                                &header,
                                &scope,
                                cond,
                            );

                            builder
                                .build_conditional_branch(cond.into_int_value(), body_bb, exit)
                                .unwrap();

                            header
                        },
                    );

                    // Generate the body.
                    builder.position_at_end(body_bb);
                    let body_bb = cg_block(
                        ctx,
                        target_machine,
                        builder,
                        module,
                        function,
                        &body_bb,
                        &scope,
                        body,
                        &Some(LoopBreakaway {
                            on_break: exit,
                            on_continue: latch,
                        }),
                    );

                    // The body breaks to latch
                    if body_bb.is_some() {
                        builder.build_unconditional_branch(latch).unwrap();
                    }

                    // Latch runs post and then breaks right back to the header.
                    builder.position_at_end(latch);
                    if let Some(post) = post {
                        cg_expr(
                            ctx,
                            target_machine,
                            builder,
                            module,
                            function,
                            &latch,
                            &scope,
                            post,
                        );
                    }

                    builder.build_unconditional_branch(header).unwrap();

                    builder.position_at_end(exit);

                    Some(exit)
                }

                TypedStmt::WhileStmt(cond, body) => {
                    // While loops are similar to for loops but much simpler.
                    // The preheader simply just breaks to the header.
                    // The header checks the condition and breaks to the exit or the body.
                    // The body simply breaks to the header.
                    // The exit is the continued code

                    // `break` => exit
                    // `continue` => header

                    let header = ctx.append_basic_block(*function, "header");

                    let body_bb = ctx.append_basic_block(*function, "body");

                    let exit = ctx.append_basic_block(*function, "exit");

                    builder.build_unconditional_branch(header).unwrap();

                    builder.position_at_end(header);

                    let (cond, _) = cg_expr(
                        ctx,
                        target_machine,
                        builder,
                        module,
                        function,
                        &header,
                        &scope,
                        cond,
                    );

                    builder
                        .build_conditional_branch(cond.into_int_value(), body_bb, exit)
                        .unwrap();

                    builder.position_at_end(body_bb);

                    let body_bb = cg_block(
                        ctx,
                        target_machine,
                        builder,
                        module,
                        function,
                        &body_bb,
                        &scope,
                        body,
                        &Some(LoopBreakaway {
                            on_break: exit,
                            on_continue: header,
                        }),
                    );

                    if body_bb.is_some() {
                        builder.build_unconditional_branch(header).unwrap();
                    }

                    builder.position_at_end(exit);

                    Some(exit)
                }
            }
        })
}

/// Initialize the LLVM [`FunctionValue`] for a given function prototype
pub fn cg_init_fn<'ctx>(
    ctx: &'ctx Context,
    module: &Module<'ctx>,
    target_machine: &TargetMachine,
    name: &str,
    ret: Option<Type>,
    args: &[&Type],
    is_variadic: bool,
) -> FunctionValue<'ctx> {
    let ret_type = llvm_type(ctx, target_machine, &ret.unwrap_or(Type::Void));
    let arg_types = args
        .iter()
        .map(|ty| llvm_basic_type(ctx, target_machine, ty).into())
        .collect::<Vec<_>>();

    let fn_type = create_fn(
        ret_type.as_any_type_enum(),
        arg_types.as_slice(),
        is_variadic,
    );
    let fn_val = module.add_function(name, fn_type, None);

    fn_val
}

/// Code generate and verify a program given a [`Context`] and return the final
/// global [`CgScope`], and [`Module`].
///
/// # Panics
/// Panics if code generation fails. This can be caused by an invalid TAST being
/// passed, so make sure to type check it so invariants are upheld.
#[must_use]
fn cg_program<'input, 'ctx>(
    ctx: &'ctx Context,
    target_machine: &TargetMachine,
    module_name: &str,
    program: Vec<TypedDeclaration<'input>>,
) -> (Module<'ctx>, CgScope<'input, 'ctx>) {
    let builder = ctx.create_builder();
    let module = ctx.create_module(module_name);

    let mut global_scope = CgScope::new();

    for declaration in program {
        match declaration {
            TypedDeclaration::FunctionDeclaration {
                name,
                parameters,
                return_type,
                body,
            } => {
                let fn_value = cg_init_fn(
                    ctx,
                    &module,
                    target_machine,
                    name,
                    return_type,
                    &parameters
                        .as_arguments()
                        .iter()
                        .map(|ArgumentDeclaration { ty, .. }| ty)
                        .collect::<Vec<_>>(),
                    parameters.is_variadic(),
                );
                global_scope.insert(name, fn_value.as_global_value().as_pointer_value());
                // must come after the insert call so that recursion is valid
                let mut fn_scope = global_scope.clone();

                if let Some(body) = body {
                    let entry = ctx.append_basic_block(fn_value, "entry");
                    builder.position_at_end(entry);

                    for (n, ArgumentDeclaration { name, ty }) in
                        parameters.into_arguments().into_iter().enumerate()
                    {
                        if entry.get_first_instruction().is_some() {
                            builder.position_before(&entry.get_first_instruction().unwrap());
                        } else {
                            builder.position_at_end(entry);
                        }

                        let alloc = builder
                            .build_alloca(
                                llvm_basic_type(ctx, target_machine, &ty),
                                &format!("arg_{name}"),
                            )
                            .unwrap();

                        builder.position_at_end(entry);

                        builder
                            .build_store::<BasicValueEnum>(
                                alloc,
                                fn_value
                                    .get_nth_param(
                                        n.try_into()
                                            .expect("over u32::MAX parameters in a function? HOW?"),
                                    )
                                    .unwrap(),
                            )
                            .unwrap();

                        fn_scope.insert(name, alloc);
                    }

                    cg_block(
                        ctx,
                        target_machine,
                        &builder,
                        &module,
                        &fn_value,
                        &entry,
                        &fn_scope,
                        body,
                        &None,
                    );
                }
            }
        }
    }

    match module.verify() {
        Ok(()) => {}

        Err(error_as_llvm_string) => {
            panic!(
                "code generation failure:\n{}\nGenerated IR:\n{}",
                error_as_llvm_string.to_string(),
                module.print_to_string().to_string()
            );
        }
    }

    module.verify().expect("Generated invalid LLVM IR");

    (module, global_scope)
}

/// Run optimizations on the given program.
fn optimize_module(module: &Module<'_>, optimization_level: OptimizationLevel) {
    let pmb = PassManagerBuilder::create();
    pmb.set_optimization_level(optimization_level);
    let pm = PassManager::<Module>::create(());
    pmb.populate_module_pass_manager(&pm);
    pm.run_on(module);
}

/// Code generate a LLVM program to a string.
///
/// # Panics
/// Panics on internal code generation failure.
#[must_use]
pub fn cg_program_to_string(
    module_name: &str,
    program: Vec<TypedDeclaration>,
    optimization_level: OptimizationLevel,
    triple: &TargetTriple,
    cpu: &str,
) -> String {
    let ctx = Context::create();

    Target::initialize_all(&InitializationConfig::default());
    let target = Target::from_triple(triple).unwrap();

    let target_machine = target
        .create_target_machine(
            triple,
            cpu,
            "",
            // FIXME: Does this potentially run the optimizer twice? That may be inefficient.
            optimization_level,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .unwrap();

    let (module, _global_scope) = cg_program(&ctx, &target_machine, module_name, program);
    optimize_module(&module, optimization_level);

    module.print_to_string().to_string()
}

/// Code generate a LLVM program to a [`MemoryBuffer`] based on the given
/// [`FileType`].
///
/// # Panics
/// Panics on internal code generation failure.
#[must_use]
pub fn cg_program_to_buffer(
    module_name: &str,
    program: Vec<TypedDeclaration>,
    file_type: FileType,
    optimization_level: OptimizationLevel,
    triple: &TargetTriple,
    cpu: &str,
) -> MemoryBuffer {
    let ctx = Context::create();

    Target::initialize_all(&InitializationConfig::default());
    let target = Target::from_triple(triple).unwrap();

    let target_machine = target
        .create_target_machine(
            triple,
            cpu,
            "",
            // FIXME: Does this potentially run the optimizer twice? That may be inefficient.
            optimization_level,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .unwrap();

    let (module, _global_scope) = cg_program(&ctx, &target_machine, module_name, program);
    optimize_module(&module, optimization_level);

    target_machine
        .write_to_memory_buffer(&module, file_type)
        .unwrap()
}

#[cfg(test)]
mod tests {
    // Please read the "Common patterns in tests" section of crate::test_utils for
    // more information on how code generator tests are structured.

    use indoc::indoc;

    use crate::cg_snapshot_test;

    #[test]
    fn let_declarations_are_properly_generated() {
        cg_snapshot_test!(indoc! {"
            fn test() {
                // TEST: should allocate twice and assign to one.
                let a: i32;
                let b: i32 = 7;

                return;
            }
        "});
    }

    mod cg_block {
        use super::*;

        mod conditionals {
            use super::*;

            #[test]
            fn if_statements_generate_as_expected() {
                cg_snapshot_test!(indoc! {"
                    fn get_bool() -> bool;
                    fn nop();

                    fn test() {
                        // TEST: properly produces a conditional break over the call result and
                        // both code paths join at the end
                        if (get_bool()) nop();

                        // TEST: code generation properly continues in the last block
                        nop();
                        return;
                    }
                "});
            }

            #[test]
            fn if_else_statements_generate_as_expected() {
                cg_snapshot_test!(indoc! {"
                    fn get_bool() -> bool;
                    fn nop();

                    fn test() {
                        // TEST: properly produces a conditional break over the call result and
                        // both code baths call nop().
                        if (get_bool()) nop();
                        else {
                            nop();
                            // TEST: this path diverges
                            return;
                        }

                        // TEST: code generation properly continues from the if_true block
                        nop();
                        return;
                    }
                "});
            }

            #[test]
            fn if_else_statements_where_both_blocks_terminate_do_not_continue_generating() {
                cg_snapshot_test!(indoc! {"
                    fn get_bool() -> bool;

                    fn test() {
                        // TEST: properly produces a conditional break over the call result
                        // and both code paths return (diverge). there should be no %end bb.
                        if (get_bool()) return;
                        else return;
                    }
                "});
            }
        }

        mod loops {
            use super::*;

            #[test]
            fn while_loops_along_with_break_and_continue_generate_as_expected() {
                cg_snapshot_test!(indoc! {"
                    fn get_bool() -> bool;

                    fn test() {
                        // TEST: the proper while loop structure is created
                        while (get_bool()) {
                            // TEST: break jumps to the `end` block
                            if (get_bool()) break;
                            else {
                                // TEST: continue jumps to the header block
                                if (get_bool()) continue;
                                // TEST: otherwise, we proceed
                                else {}
                            }

                            // TEST: the loop jumps back to the header block
                        }

                        // TEST: ...and code generation properly continues.
                        return;
                    }
                "});
            }

            #[test]
            fn for_loops_along_with_break_and_continue_generate_as_expected() {
                cg_snapshot_test!(indoc! {"
                    fn get_int() -> i32;

                    fn test() {
                        // TEST: the proper while loop structure is created
                        for (let i = 0; i < get_int(); i += 1) {
                            // TEST: break jumps to the `end` block
                            if (i > get_int()) break;
                            else {
                                // TEST: continue jumps to the latch block
                                if (i < get_int()) continue;
                                else {}
                            }

                            // TEST: the loop jumps to the latch block which jumps back to the
                            // header
                        }

                        return;
                    }
                "});
            }
        }
    }
}
