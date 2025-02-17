#![allow(clippy::pedantic)]

use types::Value;

mod printer;
mod reader;
mod types;

type Result<T> = std::result::Result<T, reader::Error>;

fn read(inp: &str) -> Result<Value> {
    reader::read_str(inp)
}

fn eval(inp: Value) -> Value {
    inp
}

fn print(inp: Value) {
    println!("> {inp}")
}

pub fn rep(inp: &str) -> Result<()> {
    print(eval(read(inp)?));
    Ok(())
}
