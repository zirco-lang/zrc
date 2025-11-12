//! Statement representation for the Zirco [TAST](super)

use std::fmt::Display;

use derive_more::Display;
use zrc_utils::{code_fmt::indent_lines, span::Spanned};

use super::{expr::TypedExpr, ty::Type};

/// A declaration created with `let`.
#[derive(Debug, Clone, PartialEq)]
pub struct LetDeclaration<'input> {
    /// The name of the identifier.
    pub name: Spanned<&'input str>,
    /// The type of the new symbol. If set to [`None`], the type will be
    /// inferred.
    // no span is added because it may be inferred
    pub ty: Type<'input>, // types are definite after inference
    /// The value to associate with the new symbol.
    pub value: Option<TypedExpr<'input>>,
}

/// A zirco statement after typeck
#[derive(Debug, Clone, PartialEq)]
pub struct TypedStmt<'input>(pub Spanned<TypedStmtKind<'input>>);

/// The enum representing all of the different kinds of statements in Zirco
/// after type checking
#[derive(Debug, Clone, PartialEq)]
pub enum TypedStmtKind<'input> {
    // all of the Box<Stmt>s for "possibly blocks" have been desugared into vec[single stmt] here
    // (basically if (x) y has become if (x) {y})
    /// `if (x) y` or `if (x) y else z`
    IfStmt(
        TypedExpr<'input>,
        Spanned<Vec<TypedStmt<'input>>>,
        Option<Spanned<Vec<TypedStmt<'input>>>>,
    ),
    /// `while (x) y`
    WhileStmt(TypedExpr<'input>, Spanned<Vec<TypedStmt<'input>>>),
    /// `do x while (y)`
    DoWhileStmt(Spanned<Vec<TypedStmt<'input>>>, TypedExpr<'input>),
    /// `for (init; cond; post) body`
    ForStmt {
        /// Runs once before the loop starts.
        init: Option<Box<Vec<Spanned<LetDeclaration<'input>>>>>,
        /// Runs before each iteration of the loop. If this evaluates to
        /// `false`, the loop will end. If this is [`None`], the loop
        /// will run forever.
        cond: Option<TypedExpr<'input>>,
        /// Runs after each iteration of the loop.
        post: Option<TypedExpr<'input>>,
        /// The body of the loop.
        body: Spanned<Vec<TypedStmt<'input>>>,
    },
    /// `switch`
    SwitchCase {
        /// The value to be switched over (`x` in `switch (x) {}`)
        scrutinee: TypedExpr<'input>,
        /// The default case
        default: Vec<TypedStmt<'input>>,
        /// The list of other cases
        cases: Vec<(TypedExpr<'input>, Vec<TypedStmt<'input>>)>,
    },
    /// `{ ... }`
    BlockStmt(Vec<TypedStmt<'input>>),
    /// `x;`
    ExprStmt(TypedExpr<'input>),
    /// `continue;`
    ContinueStmt,
    /// `break;`
    BreakStmt,
    /// `return;` or `return x;`. `return;` is the same as a `return
    /// CONST_UNIT;`
    ReturnStmt(Option<TypedExpr<'input>>),
    /// `unreachable;`
    UnreachableStmt,
    /// A let declaration
    DeclarationList(Vec<Spanned<LetDeclaration<'input>>>),
}

/// A struct or function declaration at the top level of a file
#[derive(Debug, Clone, PartialEq)]
pub enum TypedDeclaration<'input> {
    /// A declaration of a function
    FunctionDeclaration {
        /// The name of the function.
        name: Spanned<&'input str>,
        /// The parameters of the function.
        parameters: Spanned<ArgumentDeclarationList<'input>>,
        /// The return type of the function.
        return_type: Spanned<Type<'input>>,
        /// The body of the function. If set to [`None`], this is an extern
        /// declaration.
        body: Option<Spanned<Vec<TypedStmt<'input>>>>,
    },
    /// A global let declaration
    GlobalLetDeclaration(Vec<Spanned<LetDeclaration<'input>>>),
}

/// The list of arguments on a [`TypedDeclaration::FunctionDeclaration`]
///
/// May be variadic or not. Variadic only exists on extern.
#[derive(Debug, Clone, PartialEq)]
pub enum ArgumentDeclarationList<'input> {
    /// `(a, b, ...)`
    Variadic(Vec<ArgumentDeclaration<'input>>),
    /// `(a, b)` without `...`
    NonVariadic(Vec<ArgumentDeclaration<'input>>),
}
impl<'input> ArgumentDeclarationList<'input> {
    /// Create the [`ArgumentDeclarationList`] for just `()`
    #[must_use]
    pub const fn empty() -> Self {
        Self::NonVariadic(vec![])
    }

    /// Get the contained [`Vec`], variadic or not
    #[must_use]
    pub fn into_arguments(self) -> Vec<ArgumentDeclaration<'input>> {
        match self {
            ArgumentDeclarationList::NonVariadic(x) | Self::Variadic(x) => x,
        }
    }

    /// Get the contained [`Vec`], variadic or not
    #[must_use]
    pub const fn as_arguments(&self) -> &Vec<ArgumentDeclaration<'input>> {
        match self {
            ArgumentDeclarationList::NonVariadic(x) | Self::Variadic(x) => x,
        }
    }

    /// Returns `true` if this is [`ArgumentDeclarationList::Variadic`].
    #[must_use]
    pub const fn is_variadic(&self) -> bool {
        match self {
            ArgumentDeclarationList::Variadic(_) => true,
            ArgumentDeclarationList::NonVariadic(_) => false,
        }
    }
}
impl Display for ArgumentDeclarationList<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (Self::Variadic(args) | Self::NonVariadic(args)) = self;

        write!(
            f,
            "{}{}",
            args.iter()
                .map(ToString::to_string)
                .collect::<Vec<String>>()
                .join(", "),
            match self {
                Self::Variadic(_) => ", ...",
                Self::NonVariadic(_) => "",
            }
        )
    }
}

/// A special form of [`LetDeclaration`] used for function parameters.
#[derive(PartialEq, Debug, Clone, Display)]
#[display("{name}: {ty}")]
pub struct ArgumentDeclaration<'input> {
    /// The name of the parameter.
    pub name: Spanned<&'input str>,
    /// The type of the parameter.
    pub ty: Spanned<Type<'input>>,
}

impl Display for LetDeclaration<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name.value(), self.ty)?;
        if let Some(value) = &self.value {
            write!(f, " = {value}")?;
        }
        Ok(())
    }
}

impl Display for TypedStmt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.value())
    }
}

impl Display for TypedStmtKind<'_> {
    #[expect(clippy::too_many_lines)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IfStmt(cond, if_true, None) => {
                write!(
                    f,
                    "if ({cond}) {{\n{}\n}}",
                    if_true
                        .value()
                        .iter()
                        .map(|stmt| indent_lines(&stmt.to_string(), "    "))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
            Self::IfStmt(cond, if_true, Some(if_false)) => {
                write!(
                    f,
                    "if ({cond}) {{\n{}\n}} else {{\n{}\n}}",
                    if_true
                        .value()
                        .iter()
                        .map(|stmt| indent_lines(&stmt.to_string(), "    "))
                        .collect::<Vec<_>>()
                        .join("\n"),
                    if_false
                        .value()
                        .iter()
                        .map(|stmt| indent_lines(&stmt.to_string(), "    "))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
            Self::WhileStmt(cond, body) => {
                write!(
                    f,
                    "while ({cond}) {{\n{}\n}}",
                    body.value()
                        .iter()
                        .map(|stmt| indent_lines(&stmt.to_string(), "    "))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
            Self::DoWhileStmt(body, cond) => {
                write!(
                    f,
                    "do {{\n{}\n}} while ({cond});",
                    body.value()
                        .iter()
                        .map(|stmt| indent_lines(&stmt.to_string(), "    "))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
            Self::ForStmt {
                init,
                cond,
                post,
                body,
            } => {
                write!(
                    f,
                    "for ({} {}; {}) {{\n{}\n}}",
                    init.as_ref().map_or_else(
                        || ";".to_string(),
                        |x| format!(
                            "let {};",
                            x.iter()
                                .map(ToString::to_string)
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    ),
                    cond.as_ref().map_or(String::new(), ToString::to_string),
                    post.as_ref().map_or(String::new(), ToString::to_string),
                    body.value()
                        .iter()
                        .map(|stmt| indent_lines(&stmt.to_string(), "    "))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
            Self::SwitchCase {
                scrutinee,
                default,
                cases,
            } => {
                write!(f, "switch ({scrutinee}) {{")?;
                for (case_expr, case_stmts) in cases {
                    write!(
                        f,
                        " {} => {{\n{}\n}}",
                        case_expr,
                        case_stmts
                            .iter()
                            .map(|stmt| indent_lines(&stmt.to_string(), "    "))
                            .collect::<Vec<_>>()
                            .join("\n")
                    )?;
                }
                if !default.is_empty() {
                    write!(
                        f,
                        " default => {{\n{}\n}}",
                        default
                            .iter()
                            .map(|stmt| indent_lines(&stmt.to_string(), "    "))
                            .collect::<Vec<_>>()
                            .join("\n")
                    )?;
                }
                write!(f, " }}")
            }
            Self::BlockStmt(stmts) => {
                if stmts.is_empty() {
                    write!(f, "{{}}")
                } else {
                    write!(
                        f,
                        "{{\n{}\n}}",
                        stmts
                            .iter()
                            .map(|stmt| indent_lines(&stmt.to_string(), "    "))
                            .collect::<Vec<_>>()
                            .join("\n")
                    )
                }
            }
            Self::ExprStmt(expr) => write!(f, "{expr};"),
            Self::ContinueStmt => write!(f, "continue;"),
            Self::BreakStmt => write!(f, "break;"),
            Self::ReturnStmt(Some(expr)) => write!(f, "return {expr};"),
            Self::ReturnStmt(None) => write!(f, "return;"),
            Self::UnreachableStmt => write!(f, "unreachable;"),
            Self::DeclarationList(list) => {
                write!(
                    f,
                    "let {};",
                    list.iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

impl Display for TypedDeclaration<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FunctionDeclaration {
                name,
                parameters,
                return_type,
                body: Some(body),
            } => write!(
                f,
                "fn {name}({parameters}) -> {return_type} {{\n{}\n}}",
                body.value()
                    .iter()
                    .map(|stmt| indent_lines(&stmt.to_string(), "    "))
                    .collect::<Vec<String>>()
                    .join("\n")
            ),
            Self::FunctionDeclaration {
                name,
                parameters,
                return_type,
                body: None,
            } => write!(f, "fn {name}({parameters}) -> {return_type};"),
            Self::GlobalLetDeclaration(list) => {
                write!(
                    f,
                    "let {};",
                    list.iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use zrc_utils::spanned_test;

    use super::*;

    #[test]
    fn let_declaration_displays_correctly() {
        let decl = LetDeclaration {
            name: spanned_test!(0, "x", 1),
            ty: Type::I32,
            value: None,
        };
        assert_eq!(decl.to_string(), "x: i32");
    }

    #[test]
    fn let_declaration_with_value_displays_correctly() {
        let decl = LetDeclaration {
            name: spanned_test!(0, "x", 1),
            ty: Type::I32,
            value: Some(TypedExpr {
                inferred_type: Type::I32,
                kind: spanned_test!(0, super::super::expr::TypedExprKind::Identifier("y"), 1),
            }),
        };
        let display = decl.to_string();
        assert!(display.contains("x: i32"));
        assert!(display.contains('='));
    }

    #[test]
    fn typed_stmt_continue_displays_correctly() {
        let stmt = TypedStmt(spanned_test!(0, TypedStmtKind::ContinueStmt, 8));
        assert_eq!(stmt.to_string(), "continue;");
    }

    #[test]
    fn typed_stmt_break_displays_correctly() {
        let stmt = TypedStmt(spanned_test!(0, TypedStmtKind::BreakStmt, 5));
        assert_eq!(stmt.to_string(), "break;");
    }

    #[test]
    fn typed_stmt_return_without_value_displays_correctly() {
        let stmt = TypedStmt(spanned_test!(0, TypedStmtKind::ReturnStmt(None), 6));
        assert_eq!(stmt.to_string(), "return;");
    }

    #[test]
    fn argument_declaration_displays_correctly() {
        let arg = ArgumentDeclaration {
            name: spanned_test!(0, "x", 1),
            ty: spanned_test!(3, Type::I32, 6),
        };
        assert_eq!(arg.to_string(), "x: i32");
    }

    #[test]
    fn argument_declaration_list_non_variadic_displays_correctly() {
        let list = ArgumentDeclarationList::NonVariadic(vec![
            ArgumentDeclaration {
                name: spanned_test!(0, "x", 1),
                ty: spanned_test!(3, Type::I32, 6),
            },
            ArgumentDeclaration {
                name: spanned_test!(8, "y", 9),
                ty: spanned_test!(11, Type::Bool, 15),
            },
        ]);
        assert_eq!(list.to_string(), "x: i32, y: bool");
    }

    #[test]
    fn argument_declaration_list_variadic_displays_correctly() {
        let list = ArgumentDeclarationList::Variadic(vec![ArgumentDeclaration {
            name: spanned_test!(0, "x", 1),
            ty: spanned_test!(3, Type::I32, 6),
        }]);
        let display = list.to_string();
        assert!(display.contains("x: i32"));
        assert!(display.contains("..."));
    }

    #[test]
    fn argument_declaration_list_is_variadic_works() {
        let variadic = ArgumentDeclarationList::Variadic(vec![]);
        let non_variadic = ArgumentDeclarationList::NonVariadic(vec![]);

        assert!(variadic.is_variadic());
        assert!(!non_variadic.is_variadic());
    }
}
