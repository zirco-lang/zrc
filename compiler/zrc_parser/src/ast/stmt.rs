//! Statement representation for the Zirco AST
//!
//! The main thing within this module you will need is the [`Stmt`] enum. It
//! contains all the different statement kinds in Zirco. Some other structs and
//! enums exist to supplement this enum.

use std::fmt::Display;

use super::{expr::Expr, ty::Type};

/// A declaration created with `let`.
#[derive(Debug, Clone, PartialEq)]
pub struct LetDeclaration {
    /// The name of the identifier.
    pub name: String,
    /// The type of the new symbol. If set to [`None`], the type will be
    /// inferred.
    pub ty: Option<Type>,
    /// The value to associate with the new symbol.
    pub value: Option<Expr>,
}

impl Display for LetDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ty {
            None => match &self.value {
                None => write!(f, "{}", self.name),
                Some(v) => write!(f, "{} = {}", self.name, v),
            },
            Some(t) => match &self.value {
                None => write!(f, "{}: {}", self.name, t),
                Some(v) => write!(f, "{}: {} = {}", self.name, t, v),
            },
        }
    }
}

/// The enum representing all the different kinds of statements in Zirco
///
/// This enum represents all the different kinds of statements in Zirco. It is
/// used by the parser to represent the AST in the statement position.
#[derive(PartialEq, Debug, Clone)]
pub enum Stmt {
    /// `if (x) y` or `if (x) y else z`
    IfStmt(Expr, Box<Stmt>, Option<Box<Stmt>>),
    /// `while (x) y`
    WhileStmt(Expr, Box<Stmt>),
    /// `for (init; cond; post) body`
    ForStmt {
        /// Runs once before the loop starts.
        // TODO: May also be able to be expressions?
        init: Option<Box<Declaration>>,
        /// Runs before each iteration of the loop. If this evaluates to
        /// `false`, the loop will end. If this is [`None`], the loop
        /// will run forever.
        cond: Option<Expr>,
        /// Runs after each iteration of the loop.
        post: Option<Expr>,
        /// The body of the loop.
        body: Box<Stmt>,
    },
    /// `{ ... }`
    BlockStmt(Vec<Stmt>),
    /// `x;`
    ExprStmt(Expr),
    /// `;`
    EmptyStmt,
    /// `continue;`
    ContinueStmt,
    /// `break;`
    BreakStmt,
    /// `return;` or `return x;`
    ReturnStmt(Option<Expr>),
    /// Any kind of declaration
    Declaration(Declaration),
}

/// Any declaration valid to be present at the top level of a file. May also be
/// used from the [`Stmt::Declaration`] variant.
#[derive(PartialEq, Debug, Clone)]
pub enum Declaration {
    /// A list of [`LetDeclaration`]s.
    DeclarationList(Vec<LetDeclaration>),
    /// A definition for a function
    FunctionDefinition {
        /// The name of the function.
        name: String,
        /// The parameters of the function.
        parameters: Vec<ArgumentDeclaration>,
        /// The return type of the function. If set to [`None`], the function is
        /// void.
        return_type: Option<Type>,
        /// The body of the function.
        body: Vec<Stmt>,
    },
}

impl Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DeclarationList(l) => {
                write!(
                    f,
                    "let {};",
                    l.iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Self::FunctionDefinition {
                name,
                parameters,
                return_type: Some(r),
                body,
            } => write!(
                f,
                "fn {name}({}) -> {r} {}",
                parameters
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(", "),
                body.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            Self::FunctionDefinition {
                name,
                parameters,
                return_type: None,
                body,
            } => write!(
                f,
                "fn {name}({}) {}",
                parameters
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(", "),
                body.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
        }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IfStmt(e, s, None) => write!(f, "if ({e}) {s}"),
            Self::IfStmt(e, s1, Some(s2)) => write!(f, "if ({e}) {s1} else {s2}"),
            Self::WhileStmt(e, s) => write!(f, "while ({e}) {s}"),
            Self::ForStmt {
                init,
                cond,
                post,
                body,
            } => {
                write!(
                    f,
                    "for ({} {}; {}) {body}",
                    init.clone().map_or(String::new(), |x| x.to_string()),
                    cond.clone().map_or(String::new(), |x| x.to_string()),
                    post.clone().map_or(String::new(), |x| x.to_string()),
                )
            }

            Self::BlockStmt(s) => {
                write!(f, "{{")?;
                for stmt in s {
                    write!(f, "{stmt}")?;
                }
                write!(f, "}}")
            }
            Self::ExprStmt(e) => write!(f, "{e};"),
            Self::EmptyStmt => write!(f, ";"),
            Self::ContinueStmt => write!(f, "continue;"),
            Self::BreakStmt => write!(f, "break;"),
            Self::ReturnStmt(Some(e)) => write!(f, "return {e};",),
            Self::ReturnStmt(None) => write!(f, "return;"),
            Self::Declaration(d) => write!(f, "{d}"),
        }
    }
}

/// A special form of [`LetDeclaration`] used for function parameters.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ArgumentDeclaration {
    /// The name of the parameter.
    pub name: String,
    /// The type of the parameter.
    pub ty: Option<Type>,
}

impl Display for ArgumentDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ty {
            None => write!(f, "{}", self.name),
            Some(t) => write!(f, "{}: {}", self.name, t),
        }
    }
}
