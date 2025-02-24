#![warn(clippy::pedantic)]
#![allow(
    clippy::similar_names,
    clippy::module_name_repetitions,
    clippy::needless_raw_string_hashes
)]
// TODO: should add docs eventually in the future and remove this allow.
#![allow(missing_docs, clippy::missing_errors_doc)]

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
    #[error("error: {0}")]
    Custom(MalVal),
}

pub fn re(env: &Env, inp: &str) -> MalRet {
    let ast = reader::read_str(inp)?;
    env.eval(&ast)
}

pub fn rep(env: &Env, input: &str) {
    if !input.is_empty() {
        match re(env, input) {
            Ok(ret) => println!("> {ret:#}"),
            Err(Error::Reader(reader::Error::None)) => {}
            Err(e) => eprintln!("{e}"),
        }
    }
}
