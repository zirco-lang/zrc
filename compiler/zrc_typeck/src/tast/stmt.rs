use std::fmt::Display;

use zrc_parser::ast::stmt::ArgumentDeclaration;

use super::{expr::TypedExpr, ty::Type};

#[derive(Debug, Clone, PartialEq)]
pub struct LetDeclaration {
    pub name: String,
    pub ty: Type, // in contrast to the parser, the type of a declaration is definite by this step
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

#[derive(Debug, Clone, PartialEq)]
pub enum TypedStmt {
    // all of the Box<Stmt>s for "possibly blocks" have been desugared into vec[single stmt] here (basically if (x) y has become if (x) {y})
    IfStmt(TypedExpr, Vec<TypedStmt>),
    IfElseStmt(TypedExpr, Vec<TypedStmt>, Vec<TypedStmt>),
    // For statements are desugared into while statements here
    WhileStmt(TypedExpr, Vec<TypedStmt>),
    BlockStmt(Vec<TypedStmt>),
    ExprStmt(TypedExpr),
    EmptyStmt,
    ContinueStmt,
    BreakStmt,
    ReturnStmt(Option<TypedExpr>),
    Declaration(TypedDeclaration),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedDeclaration {
    DeclarationList(Vec<LetDeclaration>),
    FunctionDefinition {
        name: String,
        parameters: Vec<ArgumentDeclaration>,
        return_type: Option<Type>, // none is void
        body: Vec<TypedStmt>,
    },
}

impl Display for TypedDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypedDeclaration::DeclarationList(l) => write!(
                f,
                "let {};",
                l.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            TypedDeclaration::FunctionDefinition {
                name,
                parameters,
                return_type: Some(return_type),
                body,
            } => write!(
                f,
                "fn {name}({}) -> {return_type} {{ {} }}",
                parameters
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                body.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            TypedDeclaration::FunctionDefinition {
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
                    .collect::<Vec<_>>()
                    .join(", "),
                body.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
        }
    }
}

impl Display for TypedStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypedStmt::IfStmt(e, s) => write!(
                f,
                "if ({e}) {{ {} }}",
                s.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            TypedStmt::IfElseStmt(e, s1, s2) => write!(
                f,
                "if ({e}) {{ {} }} else {{ {} }}",
                s1.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" "),
                s2.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            TypedStmt::WhileStmt(e, s) => write!(
                f,
                "while ({e}) {{ {} }}",
                s.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            TypedStmt::BlockStmt(s) => write!(
                f,
                "{{ {} }}",
                s.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            TypedStmt::ExprStmt(e) => write!(f, "{e};"),
            TypedStmt::EmptyStmt => write!(f, ";"),
            TypedStmt::ContinueStmt => write!(f, "continue;"),
            TypedStmt::BreakStmt => write!(f, "break;"),
            TypedStmt::ReturnStmt(Some(e)) => write!(f, "return {e};",),
            TypedStmt::ReturnStmt(None) => write!(f, "return;"),
            TypedStmt::Declaration(d) => write!(f, "{}", d),
        }
    }
}
