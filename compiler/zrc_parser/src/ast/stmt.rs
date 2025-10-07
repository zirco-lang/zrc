//! Statement representation for the Zirco AST
//!
//! The main thing within this module you will need is the [`Stmt`] enum. It
//! contains all the different statement kinds in Zirco. Some other structs and
//! enums exist to supplement this enum.

use std::fmt::Display;

use derive_more::Display;
use zrc_utils::{code_fmt::indent_lines, span::Spanned};

use super::{expr::Expr, ty::Type};

/// A Zirco statement
#[derive(PartialEq, Debug, Clone, Display)]
#[display("{_0}")]
pub struct Stmt<'input>(pub Spanned<StmtKind<'input>>);

/// Represents the trigger (portion before the `=>`) in a [`SwitchCase`].
#[derive(PartialEq, Eq, Debug, Clone, Display)]
pub enum SwitchTrigger<'input> {
    /// An expression used, e.g. `2 => ...`
    #[display("{_0}")]
    Expr(Expr<'input>),
    /// The `default` keyword was used
    #[display("default")]
    Default,
}
impl<'input> SwitchTrigger<'input> {
    /// Extract the [`Expr`] from the [`SwitchTrigger::Expr`] variant, or
    /// [None].
    #[must_use]
    pub fn into_expr_value(self) -> Option<Expr<'input>> {
        match self {
            Self::Expr(x) => Some(x),
            Self::Default => None,
        }
    }
}

/// Represents a matcher within a `switch` statement.
#[derive(PartialEq, Debug, Clone, Display)]
#[display("{_0} => {_1}")]
pub struct SwitchCase<'input>(pub SwitchTrigger<'input>, pub Stmt<'input>);

/// The enum representing all the different kinds of statements in Zirco
///
/// This enum represents all the different kinds of statements in Zirco. It is
/// used by the parser to represent the AST in the statement position.
#[derive(PartialEq, Debug, Clone)]
pub enum StmtKind<'input> {
    /// `if (x) y` or `if (x) y else z`
    IfStmt(Expr<'input>, Box<Stmt<'input>>, Option<Box<Stmt<'input>>>),
    /// `while (x) y`
    WhileStmt(Expr<'input>, Box<Stmt<'input>>),
    /// `do x while (y)`
    DoWhileStmt(Box<Stmt<'input>>, Expr<'input>),
    /// `for (init; cond; post) body`
    ForStmt {
        /// Runs once before the loop starts.
        // TODO: May also be able to be expressions?
        init: Option<Box<Spanned<Vec<Spanned<LetDeclaration<'input>>>>>>,
        /// Runs before each iteration of the loop. If this evaluates to
        /// `false`, the loop will end. If this is [`None`], the loop
        /// will run forever.
        cond: Option<Expr<'input>>,
        /// Runs after each iteration of the loop.
        post: Option<Expr<'input>>,
        /// The body of the loop.
        body: Box<Stmt<'input>>,
    },
    /// `{ ... }`
    BlockStmt(Vec<Stmt<'input>>),
    /// `x;`
    ExprStmt(Expr<'input>),
    /// `;`
    EmptyStmt,
    /// `continue;`
    ContinueStmt,
    /// `break;`
    BreakStmt,
    /// `return;` or `return x;`
    ReturnStmt(Option<Expr<'input>>),
    /// `unreachable;`
    UnreachableStmt,
    /// A let declaration
    DeclarationList(Spanned<Vec<Spanned<LetDeclaration<'input>>>>),
    /// A switch case
    SwitchCase {
        /// The value to be matched over
        scrutinee: Expr<'input>,
        /// The list of value => stmt pairs
        cases: Vec<Spanned<SwitchCase<'input>>>,
    },
}
impl Display for StmtKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IfStmt(cond, if_true, None) => write!(f, "if ({cond}) {if_true}"),
            Self::IfStmt(cond, if_true, Some(if_false)) => {
                write!(f, "if ({cond}) {if_true} else {if_false}")
            }
            Self::WhileStmt(cond, body) => write!(f, "while ({cond}) {body}"),
            Self::DoWhileStmt(body, cond) => write!(f, "do {body} while ({cond});"),
            Self::ForStmt {
                init,
                cond,
                post,
                body,
            } => {
                write!(
                    f,
                    "for ({} {}; {}) {body}",
                    init.as_ref().map_or_else(
                        || ";".to_string(),
                        |x| format!(
                            "let {};",
                            x.value()
                                .iter()
                                .map(ToString::to_string)
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    ),
                    cond.as_ref().map_or(String::new(), ToString::to_string),
                    post.as_ref().map_or(String::new(), ToString::to_string),
                )
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
            Self::EmptyStmt => write!(f, ";"),
            Self::ContinueStmt => write!(f, "continue;"),
            Self::BreakStmt => write!(f, "break;"),
            Self::ReturnStmt(Some(expr)) => write!(f, "return {expr};",),
            Self::ReturnStmt(None) => write!(f, "return;"),
            Self::UnreachableStmt => write!(f, "unreachable;"),
            Self::DeclarationList(list) => {
                write!(
                    f,
                    "let {};",
                    list.value()
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Self::SwitchCase { scrutinee, cases } => write!(
                f,
                "switch ({scrutinee}) {{ {} }}",
                cases
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
        }
    }
}

/// A struct or function declaration at the top level of a file
#[derive(PartialEq, Debug, Clone)]
pub enum Declaration<'input> {
    /// A declaration of a function
    FunctionDeclaration {
        /// The name of the function.
        name: Spanned<&'input str>,
        /// The parameters of the function.
        parameters: Spanned<ArgumentDeclarationList<'input>>,
        /// The return type of the function. If set to [`None`], the function is
        /// void.
        return_type: Option<Type<'input>>,
        /// The body of the function. If set to [`None`], this is an extern
        /// declaration.
        body: Option<Spanned<Vec<Stmt<'input>>>>,
    },
    /// A named type alias (`type U = T;`)
    /// This is also used for structs and unions.
    TypeAliasDeclaration {
        /// The name of the newtype.
        name: Spanned<&'input str>,
        /// The type to associate.
        ty: Type<'input>,
    },
    /// A global let declaration
    GlobalLetDeclaration(Spanned<Vec<Spanned<LetDeclaration<'input>>>>),
}
impl Display for Declaration<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FunctionDeclaration {
                name,
                parameters,
                return_type: Some(return_ty),
                body: Some(body),
            } => write!(
                f,
                "fn {name}({parameters}) -> {return_ty} {{\n{}\n}}",
                body.value()
                    .iter()
                    .map(|stmt| indent_lines(&stmt.to_string(), "    "))
                    .collect::<Vec<String>>()
                    .join("\n")
            ),
            Self::FunctionDeclaration {
                name,
                parameters,
                return_type: Some(return_ty),
                body: None,
            } => write!(f, "fn {name}({parameters}) -> {return_ty};"),
            Self::FunctionDeclaration {
                name,
                parameters,
                return_type: None,
                body: Some(body),
            } => write!(
                f,
                "fn {name}({parameters}) {{\n{}\n}}",
                body.value()
                    .iter()
                    .map(|stmt| indent_lines(&stmt.to_string(), "    "))
                    .collect::<Vec<String>>()
                    .join("\n")
            ),
            Self::FunctionDeclaration {
                name,
                parameters,
                return_type: None,
                body: None,
            } => write!(f, "fn {name}({parameters});"),

            Self::TypeAliasDeclaration { name, ty } => write!(f, "type {name} = {ty};"),

            Self::GlobalLetDeclaration(list) => write!(
                f,
                "let {};",
                list.value()
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

/// The list of arguments on a [`Declaration::FunctionDeclaration`]
///
/// May be variadic or not.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArgumentDeclarationList<'input> {
    /// `(a, b, ...)`
    Variadic(Vec<Spanned<ArgumentDeclaration<'input>>>),
    /// `(a, b)` without `...`
    NonVariadic(Vec<Spanned<ArgumentDeclaration<'input>>>),
}
impl ArgumentDeclarationList<'_> {
    /// Create the [`ArgumentDeclarationList`] for just `()`
    #[must_use]
    pub const fn empty() -> Self {
        Self::NonVariadic(vec![])
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

/// A declaration created with `let`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetDeclaration<'input> {
    /// The name of the identifier.
    pub name: Spanned<&'input str>,
    /// The type of the new symbol. If set to [`None`], the type will be
    /// inferred.
    pub ty: Option<Type<'input>>,
    /// The value to associate with the new symbol.
    pub value: Option<Expr<'input>>,
}
impl Display for LetDeclaration<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ty {
            None => match &self.value {
                None => write!(f, "{}", self.name),
                Some(value) => write!(f, "{} = {}", self.name, value),
            },
            Some(ty) => match &self.value {
                None => write!(f, "{}: {}", self.name, ty),
                Some(value) => write!(f, "{}: {} = {}", self.name, ty, value),
            },
        }
    }
}

/// A special form of [`LetDeclaration`] used for function parameters.
#[derive(PartialEq, Eq, Debug, Clone, Display)]
#[display("{name}: {ty}")]
pub struct ArgumentDeclaration<'input> {
    /// The name of the parameter.
    pub name: Spanned<&'input str>,
    /// The type of the parameter.
    pub ty: Type<'input>,
}

#[cfg(test)]
mod tests {
    #[test]
    fn statements_stringify_to_their_canonical_form() {
        // A list of sample statements in "canonical form."
        // These are parsed then stringified again, and tested for equality.

        let test_cases = vec![
            "return;",
            "break;",
            "continue;",
            "return 4;",
            "f(x);",
            "{}",
            ";",
            "if (true) {\n    ;\n}",
            "if (true) {\n    ;\n} else {\n    ;\n}",
            "while (true) {\n    ;\n}",
            "do {\n    ;\n} while (true);",
            "for (; ; ) {\n    ;\n}",
            "for (let x = 4; ; ) {\n    ;\n}",
            "for (let x = 4, y = 5; ; ) {\n    ;\n}",
            "for (let x = 4; true; ) {\n    ;\n}",
            "let x;",
            "let x = 4;",
            "let x: i32;",
            "let x: i32 = 4;",
            "{\n    let x = 4;\n}",
            "switch (7) { 4 => false; default => {\n    12;\n} }",
        ];

        for input in test_cases {
            assert_eq!(
                crate::parser::parse_stmt_list(input, "<test>")
                    .expect("test cases should have parsed correctly")
                    .into_value()
                    .into_iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join("; "),
                input
            );
        }
    }

    #[test]
    fn functions_stringify_to_their_canonical_form() {
        let test_case = indoc::indoc! {"
            fn add(a: i32, b: i32) -> bool;
            fn add(a: i32, b: i32) -> i32 {
                return a + b;
            }
            fn no_return_extern();
            fn no_return() {

            }"};

        assert_eq!(
            crate::parser::parse_program(test_case, "<test>")
                .expect("test cases should have parsed correctly")
                .into_iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join("\n"),
            test_case
        );
    }

    #[test]
    fn nested_blocks_are_properly_indented() {
        // Test case from issue: AST to_string() should indent blocks properly
        let test_case = indoc::indoc! {"
            fn main() -> i32 {
                if (2 + 2 == 5) {
                    return 1;
                } else {
                    return 0;
                }
            }"};

        assert_eq!(
            crate::parser::parse_program(test_case, "<test>")
                .expect("test cases should have parsed correctly")
                .into_iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join("\n"),
            test_case
        );
    }
}
