//! The Zirco compiler

#![warn(
    clippy::cargo,
    clippy::nursery,
    clippy::pedantic,
    clippy::missing_docs_in_private_items,
    missing_docs
)]
#![allow(clippy::multiple_crate_versions, clippy::cargo_common_metadata)]

fn main() {
    let mut tck_scope = zrc_typeck::Scope::new();
    let mut cg_scope = zrc_codegen::CgScope::new();

    tck_scope.set_value(
        "is_fib".to_string(),
        zrc_typeck::tast::ty::Type::Fn(
            vec![zrc_typeck::tast::ty::Type::U32],
            Box::new(zrc_typeck::BlockReturnType::Return(
                zrc_typeck::tast::ty::Type::Bool,
            )),
        ),
    );
    tck_scope.set_value(
        "get_fib".to_string(),
        zrc_typeck::tast::ty::Type::Fn(
            vec![],
            Box::new(zrc_typeck::BlockReturnType::Return(
                zrc_typeck::tast::ty::Type::Ptr(Box::new(zrc_typeck::tast::ty::Type::U8)),
            )),
        ),
    );
    tck_scope.set_value(
        "get_buzz".to_string(),
        zrc_typeck::tast::ty::Type::Fn(
            vec![],
            Box::new(zrc_typeck::BlockReturnType::Return(
                zrc_typeck::tast::ty::Type::Ptr(Box::new(zrc_typeck::tast::ty::Type::U8)),
            )),
        ),
    );
    tck_scope.set_value(
        "get_fibbuzz".to_string(),
        zrc_typeck::tast::ty::Type::Fn(
            vec![],
            Box::new(zrc_typeck::BlockReturnType::Return(
                zrc_typeck::tast::ty::Type::Ptr(Box::new(zrc_typeck::tast::ty::Type::U8)),
            )),
        ),
    );
    tck_scope.set_value(
        "itoa".to_string(),
        zrc_typeck::tast::ty::Type::Fn(
            vec![zrc_typeck::tast::ty::Type::I32],
            Box::new(zrc_typeck::BlockReturnType::Return(
                zrc_typeck::tast::ty::Type::Ptr(Box::new(zrc_typeck::tast::ty::Type::U8)),
            )),
        ),
    );
    tck_scope.set_value(
        "puts".to_string(),
        zrc_typeck::tast::ty::Type::Fn(
            vec![zrc_typeck::tast::ty::Type::Ptr(Box::new(
                zrc_typeck::tast::ty::Type::U8,
            ))],
            Box::new(zrc_typeck::BlockReturnType::Void),
        ),
    );
    cg_scope.insert("is_fib", "@is_fib");
    cg_scope.insert("get_fib", "@get_fib");
    cg_scope.insert("get_buzz", "@get_buzz");
    cg_scope.insert("get_fibbuzz", "@get_fibbuzz");
    cg_scope.insert("puts", "@puts");
    cg_scope.insert("itoa", "@itoa");

    tck_scope.set_value("max".to_string(), zrc_typeck::tast::ty::Type::I32);

    let (mut cg, bb, cg_scope) = zrc_codegen::FunctionCg::new(
        "@main".to_string(),
        zrc_typeck::BlockReturnType::Return(zrc_typeck::tast::ty::Type::I32),
        vec![("max".to_string(), zrc_typeck::tast::ty::Type::I32)],
        &cg_scope,
    );

    let bb = zrc_codegen::cg_block(
        &mut cg,
        &bb,
        &cg_scope,
        zrc_typeck::type_block(
            &tck_scope,
            vec![zrc_parser::parser::parse_stmt(concat!(
                "{",
                "    for (let x = 0; x <= max; x += 1) {",
                "        let as_u32 = x as u32;",
                "        if (is_fib(as_u32) && x % 5 == 0) puts(get_fibbuzz());",
                "        else if (is_fib(as_u32)) puts(get_fib());",
                "        else if (x % 5 == 0) puts(get_buzz());",
                "        else puts(itoa(x));",
                "    }",
                "    return 0;",
                "}"
            ))
            .unwrap()],
            false,
            zrc_typeck::BlockReturnAbility::MustReturn(zrc_typeck::BlockReturnType::Return(
                zrc_typeck::tast::ty::Type::I32,
            )),
        )
        .unwrap()
        .0,
        None,
    )
    .unwrap();

    println!("{cg}");
    println!("Yields basic block {bb}");
}
