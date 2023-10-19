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
pub struct Stmt(pub Spanned<StmtKind>);

/// The enum representing all the different kinds of statements in Zirco
///
/// This enum represents all the different kinds of statements in Zirco. It is
/// used by the parser to represent the AST in the statement position.
#[derive(PartialEq, Debug, Clone)]
#[allow(clippy::module_name_repetitions)]
pub enum StmtKind {
    /// `if (x) y` or `if (x) y else z`
    IfStmt(Expr, Box<Stmt>, Option<Box<Stmt>>),
    /// `while (x) y`
    WhileStmt(Expr, Box<Stmt>),
    /// `for (init; cond; post) body`
    ForStmt {
        /// Runs once before the loop starts.
        // TODO: May also be able to be expressions?
        init: Option<Box<Spanned<Vec<Spanned<LetDeclaration>>>>>,
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
    /// A let declaration
    DeclarationList(Spanned<Vec<Spanned<LetDeclaration>>>),
}

/// A struct or function declaration at the top level of a file
#[derive(PartialEq, Debug, Clone)]
pub enum Declaration {
    /// A declaration of a function
    FunctionDeclaration {
        /// The name of the function.
        name: Spanned<String>,
        /// The parameters of the function.
        parameters: Spanned<ArgumentDeclarationList>,
        /// The return type of the function. If set to [`None`], the function is
        /// void.
        return_type: Option<Type>,
        /// The body of the function. If set to [`None`], this is an extern
        /// declaration.
        body: Option<Spanned<Vec<Stmt>>>,
    },
    /// A named declaration for a `struct`.
    StructDeclaration {
        /// The name of the newtype.
        name: Spanned<String>,
        /// The key-value pairs of the struct. Ordered by declaration order.
        #[allow(clippy::type_complexity)]
        fields: Spanned<Vec<Spanned<(Spanned<String>, super::ty::Type)>>>,
    },
}

/// The list of arguments on a [`Declaration::FunctionDeclaration`]
///
/// May be variadic or not.
#[derive(Debug, Clone, PartialEq)]
pub enum ArgumentDeclarationList {
    Variadic(Vec<Spanned<ArgumentDeclaration>>),
    NonVariadic(Vec<Spanned<ArgumentDeclaration>>),
}
impl ArgumentDeclarationList {
    pub fn empty() -> Self {
        Self::NonVariadic(vec![])
    }
}
impl Display for ArgumentDeclarationList {
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

impl Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FunctionDeclaration {
                name,
                parameters,
                return_type: Some(r),
                body: Some(body),
            } => write!(
                f,
                "fn {}({}) -> {} {{\n{}\n}}",
                name.value(),
                parameters.value(),
                r.0.value(),
                body.value()
                    .iter()
                    .map(|stmt| format!("    {stmt}"))
                    .collect::<Vec<String>>()
                    .join("\n")
            ),
            Self::FunctionDeclaration {
                name,
                parameters,
                return_type: Some(r),
                body: None,
            } => write!(
                f,
                "fn {}({}) -> {};",
                name.value(),
                parameters.value(),
                r.0.value()
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

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.value().fmt(f)
    }
}
impl Display for StmtKind {
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
            Self::DeclarationList(d) => {
                write!(
                    f,
                    "let {};",
                    d.value()
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
pub struct LetDeclaration {
    /// The name of the identifier.
    pub name: Spanned<String>,
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
                None => write!(f, "{}", self.name.value()),
                Some(v) => write!(f, "{} = {}", self.name.value(), v),
            },
            Some(t) => match &self.value {
                None => write!(f, "{}: {}", self.name.value(), t),
                Some(v) => write!(f, "{}: {} = {}", self.name.value(), t, v),
            },
        }
    }
}

/// A special form of [`LetDeclaration`] used for function parameters.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ArgumentDeclaration {
    /// The name of the parameter.
    pub name: Spanned<String>,
    /// The type of the parameter.
    pub ty: Type,
}

impl Display for ArgumentDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name.value(), self.ty)
    }
}
