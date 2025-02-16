use std::fmt::Display;

mod reader;
mod printer;

type Result<T> = std::result::Result<T, reader::Error>;

fn read(inp: &str) -> Result<Value> {
    reader::read_str(inp)
}

fn eval(inp: Value)  -> Value {
    inp
}

fn print(inp: Value) {
    println!("> {inp}")
}

pub fn rep(inp: &str) -> Result<()> {
    print(eval(read(&inp)?));
    Ok(())
}

#[derive(PartialEq, Eq, Debug)]
pub enum Value {
    List(Vec<Value>),
    Symbol(String),
    Str(String),
    Keyword(String),
    Int(i64),
    Bool(bool),
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        printer::write_value(f, self)
    }
}