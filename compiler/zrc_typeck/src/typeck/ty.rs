//! for types

use std::collections::HashMap;

use anyhow::bail;
use zrc_diagnostics::Diagnostic;
use zrc_parser::ast::ty::{Type as ParserType, TypeKind as ParserTypeKind};

use super::Scope;
use crate::tast::ty::Type as TastType;

/// Resolve an identifier to its corresponding [`tast::ty::Type`].
///
/// # Errors
/// Errors if the identifier is not found in the type scope.
pub fn resolve_type(
    scope: &Scope,
    ty: ParserType,
) -> Result<TastType, zrc_diagnostics::Diagnostic> {
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
                    return Err(Diagnostic(
                        zrc_diagnostics::Severity::Error,
                        zrc_diagnostics::Spanned(
                            ty.0 .0,
                            zrc_diagnostics::DiagnosticKind::UnableToResolveType(x),
                            ty.0 .2,
                        ),
                    ));
                }
            }
        },
        ParserTypeKind::Ptr(t) => TastType::Ptr(Box::new(resolve_type(scope, *t)?)),
        ParserTypeKind::Struct(members) => TastType::Struct(
            members
                .iter()
                .map(|(k, v)| Ok((k.clone(), resolve_type(scope, v.clone())?)))
                .collect::<Result<HashMap<String, TastType>, Diagnostic>>()?,
        ),
    })
}
