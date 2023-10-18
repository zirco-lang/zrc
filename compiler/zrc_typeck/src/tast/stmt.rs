//! Statement representation for the Zirco [TAST](super)

use std::fmt::Display;

use indexmap::IndexMap;

use super::{expr::TypedExpr, ty::Type};

/// A declaration created with `let`.
#[derive(Debug, Clone, PartialEq)]
pub struct LetDeclaration {
    /// The name of the identifier.
    pub name: String,
    /// The type of the new symbol. If set to [`None`], the type will be
    /// inferred.
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

/// The enum representing all of the different kinds of statements in Zirco
/// after type checking
#[derive(Debug, Clone, PartialEq)]
#[allow(clippy::module_name_repetitions)]
pub enum TypedStmt {
    // all of the Box<Stmt>s for "possibly blocks" have been desugared into vec[single stmt] here
    // (basically if (x) y has become if (x) {y})
    /// `if (x) y` or `if (x) y else z`
    IfStmt(TypedExpr, Vec<TypedStmt>, Option<Vec<TypedStmt>>),
    /// `while (x) y`
    WhileStmt(TypedExpr, Vec<TypedStmt>),
    /// `for (init; cond; post) body`
    ForStmt {
        /// Runs once before the loop starts.
        init: Option<Box<Vec<LetDeclaration>>>,
        /// Runs before each iteration of the loop. If this evaluates to
        /// `false`, the loop will end. If this is [`None`], the loop
        /// will run forever.
        cond: Option<TypedExpr>,
        /// Runs after each iteration of the loop.
        post: Option<TypedExpr>,
        /// The body of the loop.
        body: Vec<TypedStmt>,
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
    /// A let declaration
    DeclarationList(Vec<LetDeclaration>),
}

/// A struct or function declaration at the top level of a file
#[derive(Debug, Clone, PartialEq)]
pub enum TypedDeclaration {
    /// A declaration of a function
    FunctionDeclaration {
        /// The name of the function.
        name: String,
        /// The parameters of the function.
        parameters: Vec<ArgumentDeclaration>,
        /// The return type of the function. If set to [`None`], the function is
        /// void.
        return_type: Option<Type>,
        /// The body of the function. If set to [`None`], this is an extern
        /// declaration.
        body: Option<Vec<TypedStmt>>,
    },
    /// A named declaration for a `struct`.
    StructDeclaration {
        /// The name of the newtype.
        name: String,
        /// The key-value pairs of the struct. Ordered by declaration order.
        fields: IndexMap<String, super::ty::Type>,
    },
}

impl Display for TypedDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FunctionDeclaration {
                name,
                parameters,
                return_type: Some(r),
                body: Some(body),
            } => write!(
                f,
                "fn {name}({}) -> {r} {{\n{}\n}}",
                parameters
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(", "),
                body.iter()
                    .map(|stmt| stmt
                        .to_string()
                        .split('\n')
                        .map(|x| format!("    {x}"))
                        .collect::<Vec<_>>()
                        .join("\n"))
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
                "fn {name}({}) -> {r};",
                parameters
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::FunctionDeclaration {
                name,
                parameters,
                return_type: None,
                body: Some(body),
            } => write!(
                f,
                "fn {name}({}) {{\n{}\n}}",
                parameters
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(", "),
                body.iter()
                    .map(|stmt| stmt
                        .to_string()
                        .split('\n')
                        .map(|x| format!("    {x}"))
                        .collect::<Vec<_>>()
                        .join("\n"))
                    .collect::<Vec<String>>()
                    .join("\n")
            ),
            Self::FunctionDeclaration {
                name,
                parameters,
                return_type: None,
                body: None,
            } => write!(
                f,
                "fn {name}({});",
                parameters
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::StructDeclaration { name, fields } => write!(
                f,
                "struct {name} {{\n{}\n}}",
                fields
                    .iter()
                    .map(|(name, ty)| format!("    {name}: {ty}"))
                    .collect::<Vec<_>>()
                    .join(",\n")
            ),
        }
    }
}

impl Display for TypedStmt {
    #[allow(clippy::too_many_lines)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IfStmt(e, s1, Some(s2)) => write!(
                f,
                "if ({e}) {{\n{}\n}} else {{\n{}\n}}",
                s1.iter()
                    .map(|stmt| stmt
                        .to_string()
                        .split('\n')
                        .map(|x| format!("    {x}"))
                        .collect::<Vec<_>>()
                        .join("\n"))
                    .collect::<Vec<_>>()
                    .join("\n"),
                s2.iter()
                    .map(|stmt| stmt
                        .to_string()
                        .split('\n')
                        .map(|x| format!("    {x}"))
                        .collect::<Vec<_>>()
                        .join("\n"))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            Self::IfStmt(e, s1, None) => write!(
                f,
                "if ({e}) {{\n{}\n}}",
                s1.iter()
                    .map(|stmt| stmt
                        .to_string()
                        .split('\n')
                        .map(|x| format!("    {x}"))
                        .collect::<Vec<_>>()
                        .join("\n"))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            Self::WhileStmt(e, s) => write!(
                f,
                "while ({e}) {{\n{}\n}}",
                s.iter()
                    .map(|stmt| stmt
                        .to_string()
                        .split('\n')
                        .map(|x| format!("    {x}"))
                        .collect::<Vec<_>>()
                        .join("\n"))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            Self::ForStmt {
                init,
                cond,
                post,
                body,
            } => write!(
                f,
                "for ({} {}; {}) {{\n{}\n}}",
                init.clone().map_or(String::new(), |x| format!(
                    "let {};",
                    x.iter()
                        .map(std::string::ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(", ")
                )),
                cond.clone().map_or(String::new(), |x| x.to_string()),
                post.clone().map_or(String::new(), |x| x.to_string()),
                body.iter()
                    .map(|stmt| stmt
                        .to_string()
                        .split('\n')
                        .map(|x| format!("    {x}"))
                        .collect::<Vec<_>>()
                        .join("\n"))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            Self::BlockStmt(s) => write!(
                f,
                "{{\n{}\n}}",
                s.iter()
                    .map(|stmt| stmt
                        .to_string()
                        .split('\n')
                        .map(|x| format!("    {x}"))
                        .collect::<Vec<_>>()
                        .join("\n"))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            Self::ExprStmt(e) => write!(f, "{e};"),
            Self::EmptyStmt => write!(f, ";"),
            Self::ContinueStmt => write!(f, "continue;"),
            Self::BreakStmt => write!(f, "break;"),
            Self::ReturnStmt(Some(e)) => write!(f, "return {e};",),
            Self::ReturnStmt(None) => write!(f, "return;"),
            Self::DeclarationList(d) => write!(
                f,
                "let {};",
                d.iter()
                    .map(std::string::ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

/// A special form of [`LetDeclaration`] used for function parameters.
#[derive(PartialEq, Debug, Clone)]
pub struct ArgumentDeclaration {
    /// The name of the parameter.
    pub name: String,
    /// The type of the parameter.
    pub ty: Type,
}
impl Display for ArgumentDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.ty)
    }
}
