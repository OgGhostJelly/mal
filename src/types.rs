use std::{collections::HashMap, fmt, rc::Rc};

use crate::{
    env::{self, Env},
    printer, Error,
};

pub type MalRet = Result<MalVal, Error>;
pub type MalArgs = Vec<MalVal>;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum MalVal {
    /// NOTE: for performance reasons the order of the Vec is reversed
    ///       so the last item in the list is at index 0 and the first item is at index vec.len()
    List(Rc<Vec<MalVal>>),
    Vector(Rc<Vec<MalVal>>),
    Map(Rc<HashMap<MapKey, MalVal>>),
    Sym(String),
    Str(String),
    Kwd(String),
    Int(i64),
    Bool(bool),
    Func(fn(MalArgs) -> MalRet),
    MalFunc {
        outer: Env,
        binds: Rc<Vec<String>>,
        body: Rc<Vec<MalVal>>,
    },
    Nil,
}

impl MalVal {
    pub const TN_LIST: &'static str = "list";
    pub const TN_VECTOR: &'static str = "vector";
    pub const TN_MAP: &'static str = "map";
    pub const TN_SYMBOL: &'static str = "symbol";
    pub const TN_STRING: &'static str = "string";
    pub const TN_KEYWORD: &'static str = "keyword";
    pub const TN_INT: &'static str = "int";
    pub const TN_BOOL: &'static str = "bool";
    pub const TN_FUNCTION: &'static str = "function";
    pub const TN_NIL: &'static str = "nil";

    pub fn type_name(&self) -> &'static str {
        match self {
            MalVal::List(_) => Self::TN_LIST,
            MalVal::Vector(_) => Self::TN_VECTOR,
            MalVal::Map(_) => Self::TN_MAP,
            MalVal::Sym(_) => Self::TN_SYMBOL,
            MalVal::Str(_) => Self::TN_STRING,
            MalVal::Kwd(_) => Self::TN_KEYWORD,
            MalVal::Int(_) => Self::TN_INT,
            MalVal::Bool(_) => Self::TN_BOOL,
            MalVal::Func(_) | MalVal::MalFunc { .. } => Self::TN_FUNCTION,
            MalVal::Nil => Self::TN_NIL,
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            MalVal::Bool(bool) => *bool,
            MalVal::Nil => false,
            _ => true,
        }
    }

    pub fn to_int(&self) -> Result<&i64, env::Error> {
        match self {
            MalVal::Int(i) => Ok(i),
            _ => Err(env::Error::TypeMismatch(Self::TN_INT, self.type_name())),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum MapKey {
    Str(String),
    Keyword(String),
}

impl fmt::Display for MapKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = match self {
            MapKey::Str(str) => MalVal::Str(str.clone()),
            MapKey::Keyword(str) => MalVal::Kwd(str.clone()),
        };
        printer::write_value(f, &value)
    }
}

impl fmt::Display for MalVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        printer::write_value(f, self)
    }
}
