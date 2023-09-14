use std::{collections::HashMap, fmt::Display};

#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    Isize,
    Usize,
    Bool, // TODO: need an "any Int" type that implicitly casts to all int types but becomes i32 when assigned to a value
    Ptr(Box<Type>),
    Fn(Vec<Type>, Box<Type>),
    Struct(HashMap<String, Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.clone() {
            Type::I8 => write!(f, "i8"),
            Type::U8 => write!(f, "u8"),
            Type::I16 => write!(f, "i16"),
            Type::U16 => write!(f, "u16"),
            Type::I32 => write!(f, "i32"),
            Type::U32 => write!(f, "u32"),
            Type::I64 => write!(f, "i64"),
            Type::U64 => write!(f, "u64"),
            Type::Isize => write!(f, "isize"),
            Type::Usize => write!(f, "usize"),
            Type::Bool => write!(f, "bool"),
            Type::Ptr(t) => write!(f, "*({})", t),
            Type::Fn(args, ret) => write!(
                f,
                "(fn({}) -> {})",
                args.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                ret
            ),
            Type::Struct(fields) => write!(
                f,
                "(struct {{ {} }})",
                fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

impl Type {
    pub fn is_integer(&self) -> bool {
        use Type::*;
        match self {
            I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64 | Isize | Usize => true,
            _ => false,
        }
    }

    pub fn is_signed_integer(&self) -> bool {
        use Type::*;
        match self {
            I8 | I16 | I32 | I64 | Isize => true,
            _ => false,
        }
    }
}
