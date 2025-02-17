use std::{collections::HashMap, fmt};

use crate::printer;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Value {
    List(Vec<Value>),
    Vector(Vec<Value>),
    Map(HashMap<MapKey, Value>),
    Symbol(String),
    Str(String),
    Keyword(String),
    Int(i64),
    Bool(bool),
    Nil,
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum MapKey {
    Str(String),
    Keyword(String),
}

impl fmt::Display for MapKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = match self {
            MapKey::Str(str) => Value::Str(str.clone()),
            MapKey::Keyword(str) => Value::Keyword(str.clone()),
        };
        printer::write_value(f, &value)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        printer::write_value(f, self)
    }
}
