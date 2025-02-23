#![allow(clippy::pedantic)]

pub use env::Env;
pub use types::{MalRet, MalVal};

mod core;
mod env;
mod printer;
mod reader;
mod types;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("syntax error: {0}")]
    Reader(#[from] reader::Error),
    #[error(transparent)]
    Env(#[from] env::Error),
}

pub fn re(env: Env, inp: &str) -> MalRet {
    let ast = reader::read_str(inp)?;
    env.eval(&ast)
}

pub fn rep(env: Env, input: &str) {
    if !input.is_empty() {
        match re(env.clone(), input) {
            Ok(ret) => println!("> {ret:#}"),
            Err(e) => eprintln!("{e}"),
        }
    }
}
