//! Mal to JS Compiler
//! 
//! Converts mal into a linear-style of programming.

use std::{fmt, hash::{DefaultHasher, Hash, Hasher}};

use crate::{printer::escape_str, reader, Error, MalVal};


pub fn compile_str(input: &str) -> Result<JsVal, Error> {
    let Some(ast) = reader::read_str(input)? else {
        return Ok(JsVal::Undefined);
    };
    compile(ast)
}

pub fn compile(ast: MalVal) -> Result<JsVal, Error> {
    match ast {
        MalVal::List(list) => todo!(),
        MalVal::Vector(array) => {
            let mut block = Block();

            for value in array.iter() {
                let expr = block.push_stmt(value.clone());
            }

            todo!()
        },
        MalVal::Map(map) => todo!(),
        MalVal::Sym(sym) => Ok(JsVal::Symbol(sym.try_into()?)),
        MalVal::Str(str) => Ok(JsVal::String(str)),
        MalVal::Kwd(kwd) => Ok(JsVal::String(kwd_to_str(&kwd))),
        MalVal::Int(value) => Ok(JsVal::Int(value)),
        MalVal::Bool(value) => Ok(JsVal::Bool(value)),
        MalVal::Func(_, _) => todo!(),
        MalVal::MalFunc { name, outer, binds, rest_bind, body, is_macro } => todo!(),
        MalVal::Nil => Ok(JsVal::Null),
        MalVal::Atom(ref_cell) => todo!(),
    }
}

pub enum JsVal {
    Symbol(Symbol),
    String(String),
    Bool(bool),
    Int(i64),
    Null,
    Undefined,
}

impl Block {
    pub fn push_stmt(&mut self, value: MalVal) -> JsVal {
        
    }
}

pub struct Symbol(String);

fn kwd_to_str(kwd: &str) -> String {
    let mut hasher = DefaultHasher::new();
    kwd.hash(&mut hasher);
    let hash = hasher.finish();
    format!("{kwd}#{hash}")
}

impl TryFrom<String> for Symbol {
    type Error = Error;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        // TODO: handle special chars properly
        if value.chars().any(|ch| !ch.is_alphabetic()) {
            return Err(Error::Custom(MalVal::Str("js compiler does not support special characters in symbols yet".into())))
        }

        Ok(Self(value))
    }
}

impl fmt::Display for JsVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JsVal::Undefined => write!(f, "undefined"),
            JsVal::Symbol(symbol) => write!(f, "{symbol}"),
            JsVal::String(str) => write!(f, "\"{}\"", escape_str(str)),
            JsVal::Bool(value) => write!(f, "{value}"),
            JsVal::Int(value) => write!(f, "{value}"),
            JsVal::Null => write!(f, "null"),
        }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}