use crate::{types::Value, Error, Result};

pub struct Env {}

impl Env {
    pub fn new() -> Self {
        Self {}
    }

    pub fn get(&self, sym: String) -> Result<Value> {
        match sym.as_ref() {
            "+" => Ok(Value::Func("+", add)),
            "-" => Ok(Value::Func("-", sub)),
            "*" => Ok(Value::Func("*", mul)),
            "/" => Ok(Value::Func("/", div)),
            _ => Err(Error::NotFound(sym)),
        }
    }
}

macro_rules! num_op {
    ( $args:expr, $op:tt ) => {
        let mut args = $args;

        let Some(mut sum) = args.pop() else {
            return Ok(Value::Nil);
        };

        for value in args {
            match (sum, value) {
                (Value::Int(lhs), Value::Int(rhs)) => sum = Value::Int(lhs $op rhs),
                _ => return Err(Error::TypeMismatch)
            };
        }

        return Ok(sum);
    };
}

fn add(args: Vec<Value>) -> Result<Value> {
    num_op!(args, +);
}

fn sub(args: Vec<Value>) -> Result<Value> {
    num_op!(args, -);
}

fn mul(args: Vec<Value>) -> Result<Value> {
    num_op!(args, *);
}

fn div(args: Vec<Value>) -> Result<Value> {
    num_op!(args, /);
}
