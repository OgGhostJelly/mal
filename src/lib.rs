#![allow(clippy::pedantic)]

use std::rc::Rc;

use env::Env;
use types::Value;

pub mod env;
mod printer;
mod reader;
mod types;

type Result<T> = std::result::Result<T, Error>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Reader(#[from] reader::Error),
    #[error(transparent)]
    Env(#[from] env::Error),
}

pub fn rep(env: Rc<Env>, inp: &str) -> Result<()> {
    let ast = reader::read_str(inp)?;
    let ret = env.eval(ast)?;
    println!("> {ret}");
    Ok(())
}
