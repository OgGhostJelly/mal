#![allow(clippy::pedantic)]

use env::Env;
use types::MalVal;

mod core;
pub mod env;
mod printer;
mod reader;
mod types;

type Result<T> = std::result::Result<T, Error>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("syntax error: {0}")]
    Reader(#[from] reader::Error),
    #[error(transparent)]
    Env(#[from] env::Error),
}

pub fn rep(env: Env, inp: &str) -> Result<()> {
    let ast = reader::read_str(inp)?;
    let ret = env.eval(&ast)?;
    println!("> {ret:#}");
    Ok(())
}
