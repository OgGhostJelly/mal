use std::{
    cell::RefCell,
    collections::{HashMap, LinkedList},
    rc::Rc,
};

use crate::types::Value;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("'{0}' not found")]
    NotFound(String),
    #[error("expected symbol, got {0}")]
    ExpectedSymbol(Value),
    #[error("cannot call a non-function")]
    NotCallable,
    #[error("invalid type")]
    TypeMismatch,
    #[error("not enough arguments")]
    NotEnoughArgs,
    #[error("the first argument of def! must be a symbol")]
    NotSymbol,
}

pub struct Env {
    outer: Option<Rc<Env>>,
    data: RefCell<HashMap<String, Value>>,
}

impl Env {
    pub fn top_level() -> Rc<Self> {
        Rc::new(Self {
            outer: None,
            data: RefCell::new(HashMap::from([
                ("+".into(), Value::Func("+", add)),
                ("-".into(), Value::Func("-", sub)),
                ("*".into(), Value::Func("*", mul)),
                ("/".into(), Value::Func("/", div)),
            ])),
        })
    }

    pub fn new(env: Rc<Self>) -> Rc<Self> {
        Rc::new(Self {
            outer: Some(env),
            data: RefCell::new(HashMap::new()),
        })
    }
}

impl Env {
    pub fn get(&self, sym: String) -> Result<Value> {
        match self.data.borrow().get(&sym) {
            Some(value) => Ok(value.clone()),
            None => match &self.outer {
                Some(env) => env.get(sym),
                None => Err(Error::NotFound(sym)),
            },
        }
    }

    pub fn set(&self, key: String, value: Value) {
        self.data.borrow_mut().insert(key, value);
    }
}

impl Env {
    pub fn eval(self: Rc<Env>, ast: Value) -> Result<Value> {
        match ast {
            Value::Symbol(sym) => Ok(self.get(sym)?),
            Value::List(mut list) => {
                let Some(op) = list.pop_front() else {
                    return Ok(Value::List(list));
                };

                if let Value::Symbol(ref op) = op {
                    if op == "def!" {
                        return def_(self, list);
                    } else if op == "let*" {
                        return let_(self, list);
                    }
                }

                let op = match self.clone().eval(op)? {
                    Value::Func(_, f) => f,
                    _ => return Err(Error::NotCallable),
                };

                let mut args = Vec::new();
                for ele in list {
                    args.push(self.clone().eval(ele)?);
                }

                Ok(op(args)?)
            }
            Value::Vector(vec) => {
                let mut new_vec = Vec::with_capacity(vec.len());
                for value in vec {
                    new_vec.push(self.clone().eval(value)?);
                }
                Ok(Value::Vector(new_vec))
            }
            Value::Map(map) => {
                let mut new_map = HashMap::with_capacity(map.len());
                for (key, value) in map {
                    new_map.insert(key, self.clone().eval(value)?);
                }
                Ok(Value::Map(new_map))
            }
            _ => Ok(ast),
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

fn def_(env: Rc<Env>, mut args: LinkedList<Value>) -> Result<Value> {
    let Value::Symbol(sym) = args.pop_front().ok_or(Error::NotEnoughArgs)? else {
        return Err(Error::NotSymbol);
    };
    let val = args.pop_front().ok_or(Error::NotEnoughArgs)?;
    let val = env.clone().eval(val)?;
    // TODO: try remove this clone
    env.set(sym, val.clone());
    Ok(val)
}

fn let_(env: Rc<Env>, mut args: LinkedList<Value>) -> Result<Value> {
    let Some(bindings) = args.pop_front() else {
        return Err(Error::NotEnoughArgs);
    };
    // TODO: when `do` is implemented change this to use the rest of the let* body as a do
    let Some(ast) = args.pop_front() else {
        return Err(Error::NotEnoughArgs);
    };
    let Value::Vector(bindings) = bindings else {
        return Err(Error::TypeMismatch);
    };

    let env = Env::new(env);

    let mut key = None;
    for value in bindings {
        match key.take() {
            Some(key) => {
                let Value::Symbol(key) = key else {
                    return Err(Error::TypeMismatch);
                };
                env.set(key, value);
            }
            None => key = Some(value),
        }
    }

    env.eval(ast)
}
