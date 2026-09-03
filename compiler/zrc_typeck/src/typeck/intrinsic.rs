//! Type checking rules zrc intrinsics, function calls prefixed with `@`.

use zrc_diagnostics::{Diagnostic, DiagnosticKind, LabelKind, diagnostic::GenericLabel};
use zrc_utils::{
	ordered_fields::OrderedFields,
	span::{Span, Spannable},
};

use crate::tast::{expr::TypedExpr, ty::Type as TastType};

/// Available zrc intrinsics
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Intrinsic {
	/// for `T: {int}`, `@shl(T, usize) -> T`
	Shl,
	/// for `T: {int}`, `@shr(T, usize) -> T`
	Shr,
	/// forall T, `@volatile_write(*T, T) -> void`
	VolatileWrite,
	/// forall T, `@volatile_read(*T) -> T`
	VolatileRead,
}

impl Intrinsic {
	/// Get an intrinsic from its name, if it exists.
	pub fn from_name(name: &str) -> Option<Self> {
		match name {
			"shl" => Some(Self::Shl),
			"shr" => Some(Self::Shr),
			"volatile_write" => Some(Self::VolatileWrite),
			"volatile_read" => Some(Self::VolatileRead),
			_ => None,
		}
	}

	/// Type check an intrinsic call.
	///
	/// Intrinsics have special type rules that functions can't use yet, so they
	/// get dedicated type checking machinery.
	pub fn check<'input>(
		self,
		args: &[TypedExpr<'input>],
		span: Span,
	) -> Result<TastType<'input>, Diagnostic> {
		match self {
			Self::Shl | Self::Shr => {
				// for any integer type T,
				// @shl/@shr(T, usize) -> T

				if args.len() != 2 {
					return Err(DiagnosticKind::FunctionArgumentCountMismatch {
						expected: "2".to_string(),
						got: args.len().to_string(),
					}
					.error_in(span)
					.with_label(GenericLabel::error(
						LabelKind::FunctionArgumentCountMismatch {
							expected: "2".to_string(),
							got: args.len().to_string(),
						}
						.in_span(span),
					)));
				}

				if !args[0].inferred_type.is_integer() {
					return Err(DiagnosticKind::FunctionArgumentTypeMismatch {
						n: 0,
						expected: "{int}".to_string(),
						got: args[0].inferred_type.to_string(),
					}
					.error_in(span));
				}

				if args[1].inferred_type != TastType::Usize {
					return Err(DiagnosticKind::FunctionArgumentTypeMismatch {
						n: 1,
						expected: "usize".to_string(),
						got: args[1].inferred_type.to_string(),
					}
					.error_in(span));
				}

				Ok(args[0].inferred_type.clone())
			}
			Self::VolatileRead => {
				// @volatile_read(*T) -> T
				if args.len() != 1 {
					return Err(DiagnosticKind::FunctionArgumentCountMismatch {
						expected: "1".to_string(),
						got: args.len().to_string(),
					}
					.error_in(span)
					.with_label(GenericLabel::error(
						LabelKind::FunctionArgumentCountMismatch {
							expected: "1".to_string(),
							got: args.len().to_string(),
						}
						.in_span(span),
					)));
				}

				if let TastType::Ptr(inner) = &args[0].inferred_type {
					Ok(*inner.clone())
				} else {
					Err(DiagnosticKind::FunctionArgumentTypeMismatch {
						n: 0,
						expected: "*T".to_string(),
						got: args[0].inferred_type.to_string(),
					}
					.error_in(span))
				}
			}
			Self::VolatileWrite => {
				// @volatile_write(*T, T) -> void
				if args.len() != 2 {
					return Err(DiagnosticKind::FunctionArgumentCountMismatch {
						expected: "2".to_string(),
						got: args.len().to_string(),
					}
					.error_in(span)
					.with_label(GenericLabel::error(
						LabelKind::FunctionArgumentCountMismatch {
							expected: "2".to_string(),
							got: args.len().to_string(),
						}
						.in_span(span),
					)));
				}

				if let TastType::Ptr(inner) = &args[0].inferred_type {
					if **inner != args[1].inferred_type {
						return Err(DiagnosticKind::FunctionArgumentTypeMismatch {
							n: 1,
							expected: inner.to_string(),
							got: args[1].inferred_type.to_string(),
						}
						.error_in(span));
					}
				} else {
					return Err(DiagnosticKind::FunctionArgumentTypeMismatch {
						n: 0,
						expected: "*T".to_string(),
						got: args[0].inferred_type.to_string(),
					}
					.error_in(span));
				}

				Ok(TastType::Struct {
					fields: OrderedFields::new(),
					packed: false,
				})
			}
		}
	}
}
