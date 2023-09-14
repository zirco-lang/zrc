//! The HIR version of the [zrc_parser::ast::stmt] module.
//!
//! The HIR performs a few desugaring steps. First of all, all statements on their own are automatically placed within blocks.
//! Additionally, for loops are desugared to while loops, and while loops are all desugared to infinite loops with a break statement.
//!
//! Generate with the [super::super::lower_ast::lower_block] function.

use super::expr::Expr;
use super::ty::Type;
use std::fmt::Display;

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

#[derive(PartialEq, Debug, Clone)]
pub enum Stmt {
    IfStmt {
        cond: Expr,
        then: Vec<Stmt>,
        then_else: Option<Vec<Stmt>>,
    },
    InfiniteLoop(Vec<Stmt>),
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
                "fn {name}({}) -> {r} {{ {} }}",
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
                "fn {name}({}) {{ {} }}",
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
            Stmt::IfStmt {
                cond,
                then,
                then_else: None,
            } => write!(
                f,
                "if ({cond}) {{ {} }}",
                then.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Stmt::IfStmt {
                cond,
                then,
                then_else: Some(then_else),
            } => write!(
                f,
                "if ({cond}) {{ {} }} else {{ {} }}",
                then.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" "),
                then_else
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Stmt::InfiniteLoop(body) => write!(
                f,
                "while (true) {{ {} }}",
                body.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
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
