use std::fmt::Display;

#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    Identifier(String),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Identifier(i) => write!(f, "{}", i),
        }
    }
}
