//! Statement representation for the Zirco [TAST](super)

use std::{fmt::Display, string::ToString};

use zrc_utils::span::Spanned;

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

impl<'input> Display for LetDeclaration<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            Some(value) => write!(f, "{}: {} = {}", self.name.value(), self.ty, value),
            None => write!(f, "{}: {}", self.name.value(), self.ty),
        }
    }
}

/// A zirco statement after typeck
#[derive(Debug, Clone, PartialEq)]
#[allow(clippy::module_name_repetitions)]
pub struct TypedStmt<'input>(pub Spanned<TypedStmtKind<'input>>);

/// The enum representing all of the different kinds of statements in Zirco
/// after type checking
#[derive(Debug, Clone, PartialEq)]
#[allow(clippy::module_name_repetitions)]
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
    /// `{ ... }`
    BlockStmt(Vec<TypedStmt<'input>>),
    /// `x;`
    ExprStmt(TypedExpr<'input>),
    /// `continue;`
    ContinueStmt,
    /// `break;`
    BreakStmt,
    /// `return;` or `return x;`
    ReturnStmt(Option<TypedExpr<'input>>),
    /// A let declaration
    DeclarationList(Vec<Spanned<LetDeclaration<'input>>>),
}
impl<'input> Display for TypedStmt<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.value().fmt(f)
    }
}

impl<'input> Display for TypedStmtKind<'input> {
    #[allow(clippy::too_many_lines)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IfStmt(cond, if_true, Some(if_false)) => write!(
                f,
                "if ({cond}) {{\n{}\n}} else {{\n{}\n}}",
                if_true
                    .value()
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
                    .value()
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
                    .value()
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
                body.value()
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
            Self::DoWhileStmt(body, cond) => write!(
                f,
                "do {{\n{}\n}} while ({cond})",
                body.value()
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
            Self::ForStmt {
                init,
                cond,
                post,
                body,
            } => write!(
                f,
                "for ({} {}; {}) {{\n{}\n}}",
                init.as_ref().map_or(String::new(), |x| format!(
                    "let {};",
                    x.iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(", ")
                )),
                cond.as_ref().map_or(String::new(), ToString::to_string),
                post.as_ref().map_or(String::new(), ToString::to_string),
                body.value()
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

/// A struct or function declaration at the top level of a file
#[derive(Debug, Clone, PartialEq)]
pub enum TypedDeclaration<'input> {
    /// A declaration of a function
    FunctionDeclaration {
        /// The name of the function.
        name: Spanned<&'input str>,
        /// The parameters of the function.
        parameters: Spanned<ArgumentDeclarationList<'input>>,
        /// The return type of the function. If set to [`None`], the function is
        /// void.
        return_type: Option<Spanned<Type<'input>>>,
        /// The body of the function. If set to [`None`], this is an extern
        /// declaration.
        body: Option<Spanned<Vec<TypedStmt<'input>>>>,
    },
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
                "fn {name}({parameters}) -> {return_ty} {{\n{}\n}}",
                body.value()
                    .iter()
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
                body.value()
                    .iter()
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
        }
    }
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

/// A special form of [`LetDeclaration`] used for function parameters.
#[derive(PartialEq, Debug, Clone)]
pub struct ArgumentDeclaration<'input> {
    /// The name of the parameter.
    pub name: Spanned<&'input str>,
    /// The type of the parameter.
    pub ty: Spanned<Type<'input>>,
}
impl<'input> Display for ArgumentDeclaration<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.ty)
    }
}
