//! Statement representation for the Zirco [TAST](super)

use std::{fmt::Display, string::ToString};

use indexmap::IndexMap;

use super::{expr::TypedExpr, ty::Type};

/// A declaration created with `let`.
#[derive(Debug, Clone, PartialEq)]
pub struct LetDeclaration<'input> {
    /// The name of the identifier.
    pub name: &'input str,
    /// The type of the new symbol. If set to [`None`], the type will be
    /// inferred.
    pub ty: Type<'input>, // types are definite after inference
    /// The value to associate with the new symbol.
    pub value: Option<TypedExpr<'input>>,
}

impl<'input> Display for LetDeclaration<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            Some(value) => write!(f, "{}: {} = {}", self.name, self.ty, value),
            None => write!(f, "{}: {}", self.name, self.ty),
        }
    }
}

/// The enum representing all of the different kinds of statements in Zirco
/// after type checking
#[derive(Debug, Clone, PartialEq)]
#[allow(clippy::module_name_repetitions)]
pub enum TypedStmt<'input> {
    // all of the Box<Stmt>s for "possibly blocks" have been desugared into vec[single stmt] here
    // (basically if (x) y has become if (x) {y})
    /// `if (x) y` or `if (x) y else z`
    IfStmt(
        TypedExpr<'input>,
        Vec<TypedStmt<'input>>,
        Option<Vec<TypedStmt<'input>>>,
    ),
    /// `while (x) y`
    WhileStmt(TypedExpr<'input>, Vec<TypedStmt<'input>>),
    /// `for (init; cond; post) body`
    ForStmt {
        /// Runs once before the loop starts.
        init: Option<Box<Vec<LetDeclaration<'input>>>>,
        /// Runs before each iteration of the loop. If this evaluates to
        /// `false`, the loop will end. If this is [`None`], the loop
        /// will run forever.
        cond: Option<TypedExpr<'input>>,
        /// Runs after each iteration of the loop.
        post: Option<TypedExpr<'input>>,
        /// The body of the loop.
        body: Vec<TypedStmt<'input>>,
    },
    /// `{ ... }`
    BlockStmt(Vec<TypedStmt<'input>>),
    /// `x;`
    ExprStmt(TypedExpr<'input>),
    /// `;`
    EmptyStmt,
    /// `continue;`
    ContinueStmt,
    /// `break;`
    BreakStmt,
    /// `return;` or `return x;`
    ReturnStmt(Option<TypedExpr<'input>>),
    /// A let declaration
    DeclarationList(Vec<LetDeclaration<'input>>),
}

/// A struct or function declaration at the top level of a file
#[derive(Debug, Clone, PartialEq)]
pub enum TypedDeclaration<'input> {
    /// A declaration of a function
    FunctionDeclaration {
        /// The name of the function.
        name: &'input str,
        /// The parameters of the function.
        parameters: ArgumentDeclarationList<'input>,
        /// The return type of the function. If set to [`None`], the function is
        /// void.
        return_type: Option<Type<'input>>,
        /// The body of the function. If set to [`None`], this is an extern
        /// declaration.
        body: Option<Vec<TypedStmt<'input>>>,
    },
    /// A named declaration for a `struct`.
    StructDeclaration {
        /// The name of the newtype.
        name: &'input str,
        /// The key-value pairs of the struct. Ordered by declaration order.
        fields: IndexMap<&'input str, super::ty::Type<'input>>,
    },
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
}
impl<'input> Display for ArgumentDeclarationList<'input> {
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

impl<'input> Display for TypedDeclaration<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FunctionDeclaration {
                name,
                parameters,
                return_type: Some(return_ty),
                body: Some(body),
            } => write!(
                f,
                "fn {name}({}) -> {return_ty} {{\n{}\n}}",
                parameters,
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
            } => write!(f, "fn {name}({parameters});"),
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

impl<'input> Display for TypedStmt<'input> {
    #[allow(clippy::too_many_lines)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IfStmt(cond, if_true, Some(if_false)) => write!(
                f,
                "if ({cond}) {{\n{}\n}} else {{\n{}\n}}",
                if_true
                    .iter()
                    .map(|stmt| stmt
                        .to_string()
                        .split('\n')
                        .map(|x| format!("    {x}"))
                        .collect::<Vec<_>>()
                        .join("\n"))
                    .collect::<Vec<_>>()
                    .join("\n"),
                if_false
                    .iter()
                    .map(|stmt| stmt
                        .to_string()
                        .split('\n')
                        .map(|x| format!("    {x}"))
                        .collect::<Vec<_>>()
                        .join("\n"))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            Self::IfStmt(cond, if_true, None) => write!(
                f,
                "if ({cond}) {{\n{}\n}}",
                if_true
                    .iter()
                    .map(|stmt| stmt
                        .to_string()
                        .split('\n')
                        .map(|x| format!("    {x}"))
                        .collect::<Vec<_>>()
                        .join("\n"))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            Self::WhileStmt(cond, body) => write!(
                f,
                "while ({cond}) {{\n{}\n}}",
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
                        .map(ToString::to_string)
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
            Self::BlockStmt(stmts) => write!(
                f,
                "{{\n{}\n}}",
                stmts
                    .iter()
                    .map(|stmt| stmt
                        .to_string()
                        .split('\n')
                        .map(|x| format!("    {x}"))
                        .collect::<Vec<_>>()
                        .join("\n"))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            Self::ExprStmt(expr) => write!(f, "{expr};"),
            Self::EmptyStmt => write!(f, ";"),
            Self::ContinueStmt => write!(f, "continue;"),
            Self::BreakStmt => write!(f, "break;"),
            Self::ReturnStmt(Some(expr)) => write!(f, "return {expr};",),
            Self::ReturnStmt(None) => write!(f, "return;"),
            Self::DeclarationList(list) => write!(
                f,
                "let {};",
                list.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

/// A special form of [`LetDeclaration`] used for function parameters.
#[derive(PartialEq, Debug, Clone)]
pub struct ArgumentDeclaration<'input> {
    /// The name of the parameter.
    pub name: &'input str,
    /// The type of the parameter.
    pub ty: Type<'input>,
}
impl<'input> Display for ArgumentDeclaration<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.ty)
    }
}
