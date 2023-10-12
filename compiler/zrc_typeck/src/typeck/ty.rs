use super::Scope;
use crate::tast::ty::Type as TastType;
use anyhow::bail;
use std::collections::HashMap;
use zrc_parser::ast::ty::{Type as ParserType, TypeKind as ParserTypeKind};

/// Resolve an identifier to its corresponding [`tast::ty::Type`].
///
/// # Errors
/// Errors if the identifier is not found in the type scope.
pub fn resolve_type(scope: &Scope, ty: ParserType) -> anyhow::Result<TastType> {
    Ok(match ty.0 .1 {
        ParserTypeKind::Identifier(x) => match x.as_str() {
            "i8" => TastType::I8,
            "u8" => TastType::U8,
            "i16" => TastType::I16,
            "u16" => TastType::U16,
            "i32" => TastType::I32,
            "u32" => TastType::U32,
            "i64" => TastType::I64,
            "u64" => TastType::U64,
            "bool" => TastType::Bool,
            _ => {
                if let Some(t) = scope.get_type(&x) {
                    t.clone()
                } else {
                    bail!("Unknown type {x}");
                }
            }
        },
        ParserTypeKind::Ptr(t) => TastType::Ptr(Box::new(resolve_type(scope, *t)?)),
        ParserTypeKind::Struct(members) => TastType::Struct(
            members
                .iter()
                .map(|(k, v)| Ok((k.clone(), resolve_type(scope, v.clone())?)))
                .collect::<anyhow::Result<HashMap<String, TastType>>>()?,
        ),
    })
}
