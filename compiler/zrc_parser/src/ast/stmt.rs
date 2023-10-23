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

/// The enum representing all the different kinds of statements in Zirco
///
/// This enum represents all the different kinds of statements in Zirco. It is
/// used by the parser to represent the AST in the statement position.
#[derive(PartialEq, Debug, Clone)]
#[allow(clippy::module_name_repetitions)]
pub enum StmtKind<'input> {
    /// `if (x) y` or `if (x) y else z`
    IfStmt(Expr<'input>, Box<Stmt<'input>>, Option<Box<Stmt<'input>>>),
    /// `while (x) y`
    WhileStmt(Expr<'input>, Box<Stmt<'input>>),
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
    /// A named declaration for a `struct`.
    StructDeclaration {
        /// The name of the newtype.
        name: Spanned<&'input str>,
        /// The key-value pairs of the struct. Ordered by declaration order.
        #[allow(clippy::type_complexity)]
        fields: Spanned<Vec<Spanned<(Spanned<&'input str>, super::ty::Type<'input>)>>>,
    },
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
impl<'input> ArgumentDeclarationList<'input> {
    /// Create the [`ArgumentDeclarationList`] for just `()`
    #[must_use]
    pub const fn empty() -> Self {
        Self::NonVariadic(vec![])
    }
}
impl<'input> Display for ArgumentDeclarationList<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (Self::Variadic(args) | Self::NonVariadic(args)) = self;

        write!(
            f,
            "{}{}",
            args.iter()
                .map(|x| x.value().to_string())
                .collect::<Vec<String>>()
                .join(", "),
            match self {
                Self::Variadic(_) => ", ...",
                Self::NonVariadic(_) => "",
            }
        )
    }
}

impl<'input> Display for Declaration<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FunctionDeclaration {
                name,
                parameters,
                return_type: Some(return_ty),
                body: Some(body),
            } => write!(
                f,
                "fn {}({}) -> {} {{\n{}\n}}",
                name.value(),
                parameters.value(),
                return_ty.0.value(),
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
            } => write!(
                f,
                "fn {}({}) -> {};",
                name.value(),
                parameters.value(),
                return_ty.0.value()
            ),
            Self::FunctionDeclaration {
                name,
                parameters,
                return_type: None,
                body: Some(body),
            } => write!(
                f,
                "fn {}({}) {{\n{}\n}}",
                name.value(),
                parameters.value(),
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
            } => write!(f, "fn {}({});", name.value(), parameters.value()),
            Self::StructDeclaration { name, fields } => write!(
                f,
                "struct {} {{\n{}\n}}",
                name.value(),
                fields
                    .value()
                    .iter()
                    .map(|sp| format!("    {}: {}", sp.value().0.value(), sp.value().1))
                    .collect::<Vec<_>>()
                    .join(",\n")
            ),
        }
    }
}

impl<'input> Display for Stmt<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.value().fmt(f)
    }
}
impl<'input> Display for StmtKind<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IfStmt(cond, if_true, None) => write!(f, "if ({cond}) {if_true}"),
            Self::IfStmt(cond, if_true, Some(if_false)) => {
                write!(f, "if ({cond}) {if_true} else {if_false}")
            }
            Self::WhileStmt(cond, body) => write!(f, "while ({cond}) {body}"),
            Self::ForStmt {
                init,
                cond,
                post,
                body,
            } => {
                write!(
                    f,
                    "for ({} {}; {}) {body}",
                    init.clone().map_or(";".to_string(), |x| format!(
                        "let {};",
                        x.value()
                            .iter()
                            .map(|x| x.value().to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )),
                    cond.clone().map_or(String::new(), |x| x.to_string()),
                    post.clone().map_or(String::new(), |x| x.to_string()),
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
                        .map(|x| x.value().to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
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

impl<'input> Display for LetDeclaration<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ty {
            None => match &self.value {
                None => write!(f, "{}", self.name.value()),
                Some(value) => write!(f, "{} = {}", self.name.value(), value),
            },
            Some(ty) => match &self.value {
                None => write!(f, "{}: {}", self.name.value(), ty),
                Some(value) => write!(f, "{}: {} = {}", self.name.value(), ty, value),
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

impl<'input> Display for ArgumentDeclaration<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name.value(), self.ty)
    }
}
