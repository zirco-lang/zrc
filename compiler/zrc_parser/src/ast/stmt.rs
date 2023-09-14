use std::fmt::Display;

use super::{expr::Expr, ty::Type};

#[derive(Debug, Clone, PartialEq)]
pub struct LetDeclaration {
    pub name: String,
    pub ty: Option<Type>,
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

// FIXME: There should be a separate enum for declarations present at the top-level of a file
#[derive(PartialEq, Debug, Clone)]
pub enum Stmt {
    IfStmt(Expr, Box<Stmt>),
    IfElseStmt(Expr, Box<Stmt>, Box<Stmt>),
    WhileStmt(Expr, Box<Stmt>),
    ForStmt {
        init: Box<Declaration>,
        cond: Expr,
        post: Expr,
        body: Box<Stmt>,
    },
    BlockStmt(Vec<Stmt>),
    ExprStmt(Expr),
    EmptyStmt,
    ContinueStmt,
    BreakStmt,
    ReturnStmt(Option<Expr>),
    Declaration(Declaration),
}

/// Any declaration valid to be present at the top level of a file
#[derive(PartialEq, Debug, Clone)]
pub enum Declaration {
    DeclarationList(Vec<LetDeclaration>),
    FunctionDefinition {
        name: String,
        parameters: Vec<ArgumentDeclaration>,
        return_type: Option<Type>,
        body: Vec<Stmt>,
    },
}

impl Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Declaration::DeclarationList(l) => {
                write!(
                    f,
                    "let {};",
                    l.iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Declaration::FunctionDefinition {
                name,
                parameters,
                return_type: Some(r),
                body,
            } => write!(
                f,
                "fn {name}({}) -> {r} {}",
                parameters
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                body.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            Declaration::FunctionDefinition {
                name,
                parameters,
                return_type: None,
                body,
            } => write!(
                f,
                "fn {name}({}) {}",
                parameters
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                body.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
        }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::IfStmt(e, s) => write!(f, "if ({e}) {s}"),
            Stmt::IfElseStmt(e, s1, s2) => write!(f, "if ({e}) {s1} else {s2}"),
            Stmt::WhileStmt(e, s) => write!(f, "while ({e}) {s}"),
            Stmt::ForStmt {
                init,
                cond,
                post,
                body,
            } => {
                write!(f, "for ({init}; {cond}; {post}) {body}")
            }
            Stmt::BlockStmt(s) => {
                write!(f, "{{")?;
                for stmt in s {
                    write!(f, "{stmt}")?;
                }
                write!(f, "}}")
            }
            Stmt::ExprStmt(e) => write!(f, "{e};"),
            Stmt::EmptyStmt => write!(f, ";"),
            Stmt::ContinueStmt => write!(f, "continue;"),
            Stmt::BreakStmt => write!(f, "break;"),
            Stmt::ReturnStmt(Some(e)) => write!(f, "return {e};",),
            Stmt::ReturnStmt(None) => write!(f, "return;"),
            Stmt::Declaration(d) => write!(f, "{}", d),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct ArgumentDeclaration {
    pub name: String,
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
