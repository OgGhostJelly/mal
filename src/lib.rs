#![allow(clippy::pedantic)]

use std::collections::HashMap;

use env::Env;
use types::Value;

mod env;
mod printer;
mod reader;
mod types;

type Result<T> = std::result::Result<T, Error>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Reader(#[from] reader::Error),
    #[error("'{0}' not found")]
    NotFound(String),
    #[error("expected symbol, got {0}")]
    ExpectedSymbol(Value),
    #[error("cannot call a non-function")]
    NotCallable,
    #[error("invalid type")]
    TypeMismatch,
}

fn read(inp: &str) -> Result<Value> {
    Ok(reader::read_str(inp)?)
}

fn eval(inp: Value, env: &mut Env) -> Result<Value> {
    match inp {
        Value::Symbol(sym) => Ok(env.get(sym)?),
        Value::List(mut list) => {
            let Some(op) = list.pop_front() else {
                return Ok(Value::List(list));
            };

            let op = match eval(op, env)? {
                Value::Func(_, f) => f,
                _ => return Err(Error::NotCallable),
            };

            let mut args = Vec::new();
            for ele in list {
                args.push(eval(ele, env)?);
            }

            Ok(op(args)?)
        }
        Value::Vector(vec) => {
            let mut new_vec = Vec::with_capacity(vec.len());
            for value in vec {
                new_vec.push(eval(value, env)?);
            }
            Ok(Value::Vector(new_vec))
        }
        Value::Map(map) => {
            let mut new_map = HashMap::with_capacity(map.len());
            for (key, value) in map {
                new_map.insert(key, eval(value, env)?);
            }
            Ok(Value::Map(new_map))
        }
        _ => Ok(inp),
    }
}

fn print(inp: Value) {
    println!("> {inp}")
}

pub fn rep(inp: &str) -> Result<()> {
    print(eval(read(inp)?, &mut Env::new())?);
    Ok(())
}
