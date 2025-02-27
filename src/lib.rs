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
    #[error("runtime syntax error: {0}")]
    RuntimeReader(reader::Error),
    #[error("error: {0}")]
    Env(#[from] env::Error),
    #[error("{0}")]
    Custom(MalVal),
}

pub fn re(env: &Env, inp: &str) -> MalRet {
    let ast = reader::read_str(inp)?.unwrap_or(MalVal::Nil);
    env.eval(&ast)
}

pub fn rep(env: &Env, input: &str) {
    if !input.is_empty() {
        let ret = reader::read_str(input)
            .transpose()
            .map(|ast| env.eval(&ast?));

        if let Some(ret) = ret {
            match ret {
                Ok(ret) => println!("> {ret:#}"),
                Err(e) => match e {
                    Error::Custom(e) => eprintln!("Uncaught exception: {e}\n  in ogj-rust"),
                    e => eprintln!("{e}\n  in ogj-rust"),
                },
            }
        }
    }
}
