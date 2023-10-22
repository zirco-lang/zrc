use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    types::AnyType,
    values::{BasicValueEnum, FunctionValue},
};
use zrc_typeck::tast::{
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
#[derive(PartialEq, Debug, Clone)]
pub struct LoopBreakaway<'ctx> {
    /// Points to the exit basic block.
    on_break: BasicBlock<'ctx>,
    /// For `for` loops, points to the latch. For `while` loops, points to the
    /// header.
    on_continue: BasicBlock<'ctx>,
}

pub fn cg_let_declaration<'ctx, 'input, 'a>(
    ctx: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
    function: &'a FunctionValue<'ctx>,
    bb: &'a BasicBlock<'ctx>,
    scope: &'a mut CgScope<'input, 'ctx>,
    declarations: Vec<LetDeclaration<'input>>,
) -> BasicBlock<'ctx> {
    let mut bb = *bb;

    for let_declaration in declarations {
        let ptr = builder
            .build_alloca(
                llvm_basic_type(ctx, let_declaration.ty.clone()),
                &format!("let_{}", let_declaration.name),
            )
            .unwrap();

        scope.insert(let_declaration.name, ptr);

        if let Some(value) = let_declaration.value {
            bb = cg_expr(
                ctx,
                builder,
                module,
                function,
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
            )
            .1;
        }
    }

    bb
}

#[allow(clippy::too_many_arguments)]
pub fn cg_block<'ctx, 'input, 'a>(
    ctx: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
    function: &'a FunctionValue<'ctx>,
    bb: &'a BasicBlock<'ctx>,
    parent_scope: &'a CgScope<'input, 'ctx>,
    block: Vec<TypedStmt<'input>>,
    breakaway: Option<LoopBreakaway<'ctx>>,
) -> Option<BasicBlock<'ctx>> {
    let mut scope = parent_scope.clone();

    block
        .into_iter()
        .fold(Some(*bb), |bb, stmt| -> Option<BasicBlock> {
            let Some(bb) = bb else {
                // we've hit a return statement / terminator already
                return None;
            };

            match stmt {
                TypedStmt::EmptyStmt => Some(bb),
                TypedStmt::ExprStmt(expr) => {
                    Some(cg_expr(ctx, builder, module, function, &bb, &scope, expr).1)
                }

                TypedStmt::IfStmt(cond, then, then_else) => {
                    let then_else = then_else.unwrap_or(vec![]);

                    let (cond, _) = cg_expr(ctx, builder, module, function, &bb, &scope, cond);

                    let then_bb = ctx.append_basic_block(*function, "then");
                    let then_else_bb = ctx.append_basic_block(*function, "then_else");
                    let terminating_bb = ctx.append_basic_block(*function, "end");

                    builder
                        .build_conditional_branch(cond.into_int_value(), then_bb, then_else_bb)
                        .unwrap();

                    builder.position_at_end(then_bb);
                    let maybe_then_bb = cg_block(
                        ctx,
                        builder,
                        module,
                        function,
                        &then_bb,
                        &scope,
                        then,
                        breakaway.clone(),
                    );
                    if maybe_then_bb.is_some() {
                        builder.build_unconditional_branch(terminating_bb).unwrap();
                    }

                    builder.position_at_end(then_else_bb);
                    let maybe_then_else_bb = cg_block(
                        ctx,
                        builder,
                        module,
                        function,
                        &then_else_bb,
                        &scope,
                        then_else,
                        breakaway.clone(),
                    );
                    if maybe_then_else_bb.is_some() {
                        builder.build_unconditional_branch(terminating_bb).unwrap();
                    }

                    builder.position_at_end(terminating_bb);

                    if maybe_then_bb.is_none() && maybe_then_else_bb.is_none() {
                        None
                    } else {
                        Some(terminating_bb)
                    }
                }

                TypedStmt::BlockStmt(block) => cg_block(
                    ctx,
                    builder,
                    module,
                    function,
                    &bb,
                    &scope,
                    block,
                    breakaway.clone(),
                ),

                TypedStmt::ReturnStmt(Some(expr)) => {
                    let (expr, _) = cg_expr(ctx, builder, module, function, &bb, &scope, expr);

                    builder.build_return(Some(&expr)).unwrap();

                    None
                }

                TypedStmt::ReturnStmt(None) => {
                    builder.build_return(None).unwrap();

                    None
                }

                TypedStmt::ContinueStmt => {
                    if let Some(breakaway) = breakaway.clone() {
                        builder
                            .build_unconditional_branch(breakaway.on_continue)
                            .unwrap();

                        None
                    } else {
                        panic!("continue statement outside of loop");
                    }
                }

                TypedStmt::BreakStmt => {
                    if let Some(breakaway) = breakaway.clone() {
                        builder
                            .build_unconditional_branch(breakaway.on_break)
                            .unwrap();

                        None
                    } else {
                        panic!("break statement outside of loop");
                    }
                }

                TypedStmt::DeclarationList(d) => Some(cg_let_declaration(
                    ctx, builder, module, function, &bb, &mut scope, d,
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
                    match init {
                        None => bb,
                        Some(init) => cg_let_declaration(
                            ctx, builder, module, function, &bb, &mut scope, *init,
                        ),
                    };

                    let header = ctx.append_basic_block(*function, "header");
                    let body_bb = ctx.append_basic_block(*function, "body");
                    let latch = ctx.append_basic_block(*function, "latch");
                    let exit = ctx.append_basic_block(*function, "exit");

                    // Branch to the header from the preheader.
                    builder.build_unconditional_branch(header).unwrap();

                    // Generate the header.
                    builder.position_at_end(header);
                    let header = match cond {
                        None => {
                            // If there is no condition, we always branch to the body.
                            builder.build_unconditional_branch(body_bb).unwrap();

                            header
                        }

                        Some(cond) => {
                            let (cond, header) =
                                cg_expr(ctx, builder, module, function, &header, &scope, cond);

                            builder
                                .build_conditional_branch(cond.into_int_value(), body_bb, exit)
                                .unwrap();

                            header
                        }
                    };

                    // Generate the body.
                    builder.position_at_end(body_bb);
                    let body_bb = cg_block(
                        ctx,
                        builder,
                        module,
                        function,
                        &body_bb,
                        &scope,
                        body,
                        Some(LoopBreakaway {
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
                        cg_expr(ctx, builder, module, function, &latch, &scope, post).1
                    } else {
                        latch
                    };

                    builder.build_unconditional_branch(header).unwrap();

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

                    let (cond, header) =
                        cg_expr(ctx, builder, module, function, &header, &scope, cond);

                    builder
                        .build_conditional_branch(cond.into_int_value(), body_bb, exit)
                        .unwrap();

                    builder.position_at_end(body_bb);

                    let body_bb = cg_block(
                        ctx,
                        builder,
                        module,
                        function,
                        &body_bb,
                        &scope,
                        body,
                        Some(LoopBreakaway {
                            on_break: exit,
                            on_continue: header,
                        }),
                    );

                    if body_bb.is_some() {
                        builder.build_unconditional_branch(header).unwrap();
                    }

                    Some(exit)
                }
            }
        })
}

pub fn cg_init_fn<'ctx>(
    ctx: &'ctx Context,
    module: &Module<'ctx>,
    name: &str,
    ret: Option<Type>,
    args: Vec<Type>,
) -> FunctionValue<'ctx> {
    let ret_type = llvm_type(ctx, ret.unwrap_or(Type::Void));
    let arg_types = args
        .into_iter()
        .map(|t| llvm_basic_type(ctx, t).into())
        .collect::<Vec<_>>();

    let fn_type = create_fn(ret_type.as_any_type_enum(), arg_types.as_slice());
    let fn_val = module.add_function(name, fn_type, None);

    fn_val
}

pub fn cg_program(program: Vec<TypedDeclaration>) -> String {
    let ctx = Context::create();
    let builder = ctx.create_builder();
    let module = ctx.create_module("main");

    let mut global_scope = CgScope::new();

    for declaration in program {
        match declaration {
            // Struct declarations do not need to be emitted
            TypedDeclaration::StructDeclaration { .. } => {}

            TypedDeclaration::FunctionDeclaration {
                name,
                parameters,
                return_type,
                body,
            } => {
                let fn_value = cg_init_fn(
                    &ctx,
                    &module,
                    name,
                    return_type,
                    parameters
                        .iter()
                        .map(|ArgumentDeclaration { ty, .. }| ty.clone())
                        .collect::<Vec<_>>(),
                );
                global_scope.insert(name, fn_value.as_global_value().as_pointer_value());
                // must come after the insert call so that recursion is valid
                let mut fn_scope = global_scope.clone();

                if let Some(body) = body {
                    let entry = ctx.append_basic_block(fn_value, "entry");
                    builder.position_at_end(entry);

                    for (n, ArgumentDeclaration { name, ty }) in parameters.into_iter().enumerate()
                    {
                        if entry.get_first_instruction().is_some() {
                            builder.position_before(&entry.get_first_instruction().unwrap());
                        } else {
                            builder.position_at_end(entry);
                        }

                        let alloc = builder
                            .build_alloca(llvm_basic_type(&ctx, ty), &format!("arg_{}", name))
                            .unwrap();

                        builder.position_at_end(entry);

                        builder
                            .build_store::<BasicValueEnum>(
                                alloc,
                                fn_value.get_nth_param(n as u32).unwrap(),
                            )
                            .unwrap();

                        fn_scope.insert(name, alloc);
                    }

                    cg_block(
                        &ctx, &builder, &module, &fn_value, &entry, &fn_scope, body, None,
                    );
                }
            }
        }
    }

    module.print_to_string().to_string()
}
