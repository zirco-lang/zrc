//! Statement representation for the Zirco [TAST](super)

use std::fmt::Display;

use zrc_parser::ast::stmt::ArgumentDeclaration;

use super::{expr::TypedExpr, ty::Type};

/// A declaration created with `let`.
#[derive(Debug, Clone, PartialEq)]
pub struct LetDeclaration {
    /// The name of the identifier.
    pub name: String,
    /// The type of the new symbol. If set to [`None`], the type will be inferred.
    pub ty: Type, // types are definite after inference
    /// The value to associate with the new symbol.
    pub value: Option<TypedExpr>,
}

impl Display for LetDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            Some(v) => write!(f, "{}: {} = {}", self.name, self.ty, v),
            None => write!(f, "{}: {}", self.name, self.ty),
        }
    }
}

/// The enum representing all of the different kinds of statements in Zirco after type checking
#[derive(Debug, Clone, PartialEq)]
#[allow(clippy::module_name_repetitions)]
pub enum TypedStmt {
    // all of the Box<Stmt>s for "possibly blocks" have been desugared into vec[single stmt] here (basically if (x) y has become if (x) {y})
    /// `if (x) y` or `if (x) y else z`
    IfStmt(TypedExpr, Vec<TypedStmt>, Option<Vec<TypedStmt>>),
    /// `while (x) y`
    WhileStmt(TypedExpr, Vec<TypedStmt>),
    /// `for (init; cond; post) body`
    ForStmt {
        /// Runs once before the loop starts.
        init: Option<Box<TypedDeclaration>>,
        /// Runs before each iteration of the loop. If this evaluates to `false`, the loop will end.
        /// If this is [`None`], the loop will run forever.
        cond: Option<TypedExpr>,
        /// Runs after each iteration of the loop.
        post: Option<TypedExpr>,
        /// The body of the loop.
        body: Box<TypedStmt>,
    },
    /// `{ ... }`
    BlockStmt(Vec<TypedStmt>),
    /// `x;`
    ExprStmt(TypedExpr),
    /// `;`
    EmptyStmt,
    /// `continue;`
    ContinueStmt,
    /// `break;`
    BreakStmt,
    /// `return;` or `return x;`
    ReturnStmt(Option<TypedExpr>),
    /// Any kind of declaration
    Declaration(TypedDeclaration),
}

/// Any declaration valid to be present at the top level of a file. May also be used from the [`TypedStmt::Declaration`] variant.
#[derive(Debug, Clone, PartialEq)]
pub enum TypedDeclaration {
    /// A list of [`LetDeclaration`]s.
    DeclarationList(Vec<LetDeclaration>),
    /// A definition for a function
    FunctionDefinition {
        /// The name of the function.
        name: String,
        /// The parameters of the function.
        parameters: Vec<ArgumentDeclaration>,
        /// The return type of the function. If set to [`None`], the function is void.
        return_type: Option<Type>,
        /// The body of the function.
        body: Vec<TypedStmt>,
    },
}

impl Display for TypedDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DeclarationList(l) => write!(
                f,
                "let {};",
                l.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::FunctionDefinition {
                name,
                parameters,
                return_type: Some(return_type),
                body,
            } => write!(
                f,
                "fn {name}({}) -> {return_type} {{ {} }}",
                parameters
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", "),
                body.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Self::FunctionDefinition {
                name,
                parameters,
                return_type: None,
                body,
            } => write!(
                f,
                "fn {name}({}) {{ {} }}",
                parameters
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", "),
                body.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
        }
    }
}

impl Display for TypedStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IfStmt(e, s, None) => write!(
                f,
                "if ({e}) {{ {} }}",
                s.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Self::IfStmt(e, s1, Some(s2)) => write!(
                f,
                "if ({e}) {{ {} }} else {{ {} }}",
                s1.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(" "),
                s2.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Self::WhileStmt(e, s) => write!(
                f,
                "while ({e}) {{ {} }}",
                s.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Self::ForStmt {
                init,
                cond,
                post,
                body,
            } => write!(
                f,
                "for ({} {}; {}) {body}",
                init.clone().map_or(String::new(), |x| x.to_string()),
                cond.clone().map_or(String::new(), |x| x.to_string()),
                post.clone().map_or(String::new(), |x| x.to_string())
            ),
            Self::BlockStmt(s) => write!(
                f,
                "{{ {} }}",
                s.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
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
