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
    cg_scope.insert("puts", "@puts");
    cg_scope.insert("itoa", "@itoa");

    tck_scope.set_value("max".to_string(), zrc_typeck::tast::ty::Type::I32);

    let (mut cg, bb, cg_scope) = zrc_codegen::FunctionCg::new(
        "@main".to_string(),
        zrc_typeck::BlockReturnType::Return(zrc_typeck::tast::ty::Type::I32),
        vec![("max".to_string(), zrc_typeck::tast::ty::Type::I32)],
        &cg_scope,
    );
    let mut mod_cg = zrc_codegen::ModuleCg::new();

    let bb = zrc_codegen::cg_block(
        &mut mod_cg,
        &mut cg,
        &bb,
        &cg_scope,
        zrc_typeck::type_block(
            &tck_scope,
            vec![zrc_parser::parser::parse_stmt(concat!(
                "{",
                "    for (let x = 0; x <= max; x += 1) {",
                "        let as_u32 = x as u32;",
                "        if (is_fib(as_u32) && x % 5 == 0) puts(\"fibbuzz\");",
                "        else if (is_fib(as_u32)) puts(\"fib\");",
                "        else if (x % 5 == 0) puts(\"buzz\");",
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

    mod_cg.declarations.push(cg.to_string());

    println!("{mod_cg}");
}
