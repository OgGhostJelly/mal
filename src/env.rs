use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::types::{Args, Closure, MacroArgs, Value};

pub type Result = std::result::Result<Value, Error>;

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
    #[error("too many arguments")]
    TooManyArgs,
    #[error("the first argument of def! must be a symbol")]
    NotSymbol,
}

#[derive(PartialEq, Eq, Debug, Clone)]
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
    pub fn get(&self, sym: String) -> Result {
        match self.data.borrow().get(&sym) {
            Some(value) => Ok(value.clone()),
            None => match &self.outer {
                Some(env) => env.get(sym),
                None => Err(Error::NotFound(sym)),
            },
        }
    }

    pub fn set(&self, key: String, value: Value) -> Value {
        let value = Value::Ref(Rc::new(value));
        self.data.borrow_mut().insert(key, value.clone());
        value
    }
}

impl Env {
    pub fn eval(self: &Rc<Env>, ast: Value) -> Result {
        self.clone().eval_inner(ast)
    }

    pub fn eval_inner(self: Rc<Env>, ast: Value) -> Result {
        match ast {
            Value::Symbol(sym) => Ok(self.get(sym)?),
            Value::List(mut list) => {
                let Some(op) = list.pop_front() else {
                    return Ok(Value::List(list));
                };

                if let Value::Symbol(ref op) = op {
                    if op == "def!" {
                        return def_(self, MacroArgs::new(list));
                    } else if op == "let*" {
                        return let_(self, MacroArgs::new(list));
                    } else if op == "do" {
                        return do_(self, MacroArgs::new(list));
                    } else if op == "if" {
                        return if_(self, MacroArgs::new(list));
                    } else if op == "fn*" {
                        return fn_(self, MacroArgs::new(list));
                    }
                }

                let x = self.eval(op)?;

                let op: Box<dyn Fn(Args) -> Result> = match x.traverse_ref() {
                    Value::Func(_, f) => Box::new(f),
                    Value::Closure(closure) => Box::new(move |args| closure.apply(args)),
                    _ => return Err(Error::NotCallable),
                };

                let mut args = Vec::new();
                for ele in list {
                    args.push(self.eval(ele)?);
                }

                Ok(op(Args::new(args))?)
            }
            Value::Vector(vec) => {
                let mut new_vec = Vec::with_capacity(vec.len());
                for value in vec {
                    new_vec.push(self.eval(value)?);
                }
                Ok(Value::Vector(new_vec))
            }
            Value::Map(map) => {
                let mut new_map = HashMap::with_capacity(map.len());
                for (key, value) in map {
                    new_map.insert(key, self.eval(value)?);
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

        let Some(mut sum) = args.next() else {
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

fn add(args: Args) -> Result {
    num_op!(args, +);
}

fn sub(args: Args) -> Result {
    num_op!(args, -);
}

fn mul(args: Args) -> Result {
    num_op!(args, *);
}

fn div(args: Args) -> Result {
    num_op!(args, /);
}

fn def_(env: Rc<Env>, mut args: MacroArgs) -> Result {
    let [Value::Symbol(sym), val] = args.grab().ok_or(Error::NotEnoughArgs)? else {
        return Err(Error::NotSymbol);
    };
    Ok(env.set(sym, env.eval(val)?))
}

fn let_(env: Rc<Env>, mut args: MacroArgs) -> Result {
    let bindings = match args.grab().ok_or(Error::NotEnoughArgs)? {
        [Value::Vector(vec)] => vec,
        [Value::List(list)] => list.into_iter().collect(),
        _ => return Err(Error::TypeMismatch),
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

    do_(env, args)
}

fn do_(env: Rc<Env>, args: MacroArgs) -> Result {
    let mut last = None;
    for ele in args {
        last = Some(env.eval(ele)?);
    }
    Ok(last.unwrap_or(Value::Nil))
}

fn if_(env: Rc<Env>, mut args: MacroArgs) -> Result {
    let [cond, truthy, falsey] = args.grab().ok_or(Error::NotEnoughArgs)?;

    let cond = match env.eval(cond)? {
        Value::Bool(bool) => bool,
        Value::Nil => false,
        _ => true,
    };

    if cond {
        env.eval(truthy)
    } else {
        env.eval(falsey)
    }
}

fn fn_(env: Rc<Env>, mut args: MacroArgs) -> Result {
    let [Value::List(binds_ast)] = args.grab().ok_or(Error::NotEnoughArgs)? else {
        return Err(Error::TypeMismatch);
    };
    let body = Rc::new(Value::List(args.to_ast()));

    let mut binds = Vec::new();
    for value in binds_ast {
        if let Value::Symbol(sym) = value {
            binds.push(sym);
        } else {
            return Err(Error::TypeMismatch);
        }
    }

    Ok(Value::Closure(Closure { env, body, binds }))
}
