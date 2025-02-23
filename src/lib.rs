#![allow(clippy::pedantic)]

use env::Env;
use types::{MalRet, MalVal};

mod core;
pub mod env;
mod printer;
mod reader;
pub mod types;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("syntax error: {0}")]
    Reader(#[from] reader::Error),
    #[error(transparent)]
    Env(#[from] env::Error),
}

pub fn rep(env: Env, inp: &str) -> MalRet {
    let ast = reader::read_str(inp)?;
    env.eval(&ast)
}
