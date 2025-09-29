//! Statement representation for the Zirco AST
//!
//! The main thing within this module you will need is the [`Stmt`] enum. It
//! contains all the different statement kinds in Zirco. Some other structs and
//! enums exist to supplement this enum.

use std::fmt::Display;

use zrc_utils::span::Spanned;

use super::{expr::Expr, ty::Type};

/// A Zirco statement
#[derive(PartialEq, Debug, Clone)]
pub struct Stmt<'input>(pub Spanned<StmtKind<'input>>);
impl Display for Stmt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.value().fmt(f)
    }
}

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
    /// A let declaration
    DeclarationList(Spanned<Vec<Spanned<LetDeclaration<'input>>>>),
    /// A switch case
    SwitchCase {
        /// The value to be matched over
        scrutinee: Expr<'input>,
        /// The list of value => stmt pairs
        cases: Vec<Spanned<(Expr<'input>, Stmt<'input>)>>,
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
                write!(f, "{{")?;
                for stmt in stmts {
                    write!(f, "{stmt}")?;
                }
                write!(f, "}}")
            }
            Self::ExprStmt(expr) => write!(f, "{expr};"),
            Self::EmptyStmt => write!(f, ";"),
            Self::ContinueStmt => write!(f, "continue;"),
            Self::BreakStmt => write!(f, "break;"),
            Self::ReturnStmt(Some(expr)) => write!(f, "return {expr};",),
            Self::ReturnStmt(None) => write!(f, "return;"),
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
            Self::SwitchCase { .. } => todo!(), // TODO
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
                    .map(|stmt| format!("    {stmt}"))
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
                    .map(|stmt| format!("    {stmt}"))
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
#[derive(Debug, Clone, PartialEq)]
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
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ArgumentDeclaration<'input> {
    /// The name of the parameter.
    pub name: Spanned<&'input str>,
    /// The type of the parameter.
    pub ty: Type<'input>,
}
impl Display for ArgumentDeclaration<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.ty)
    }
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
            "return (4);",
            "((f)((x)));",
            "{}",
            ";",
            "if ((true)) {;}",
            "if ((true)) {;} else {;}",
            "while ((true)) {;}",
            "do {;} while ((true));",
            "for (; ; ) {;}",
            "for (let x = (4); ; ) {;}",
            "for (let x = (4), y = (5); ; ) {;}",
            "for (let x = (4); (true); ) {;}",
            "let x;",
            "let x = (4);",
            "let x: i32;",
            "let x: i32 = (4);",
            "{let x = (4);}",
        ];

        for input in test_cases {
            assert_eq!(
                crate::parser::parse_stmt_list(input)
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
                return ((a) + (b));
            }
            fn no_return_extern();
            fn no_return() {

            }"};

        assert_eq!(
            crate::parser::parse_program(test_case)
                .expect("test cases should have parsed correctly")
                .into_iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join("\n"),
            test_case
        );
    }
}
