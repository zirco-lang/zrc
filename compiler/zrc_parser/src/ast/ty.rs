use std::{collections::HashMap, fmt::Display};

#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    Identifier(String),
    Ptr(Box<Type>),
    Struct(HashMap<String, Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Identifier(i) => write!(f, "{}", i),
            Type::Ptr(t) => write!(f, "*{}", t),
            Type::Struct(members) => {
                write!(f, "struct {{ ")?;
                for (i, m) in members.iter().enumerate() {
                    write!(f, "{}: {}", m.0, m.1)?;
                    if i < members.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, " }}")
            }
        }
    }
}
