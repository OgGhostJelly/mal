use std::{collections::HashMap, rc::Rc};

use crate::types::{MalArgs, MalRet, MalVal};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("'{0}' not found")]
    NotFound(String),
    #[error("type mismatch, expected '{0}' got '{1}'")]
    TypeMismatch(&'static str, &'static str),
    #[error("cannot call non-function type '{0}'")]
    CannotCall(&'static str),
    #[error("missing parameters")]
    MissingParams,
}

pub type Env = Rc<EnvInner>;

pub struct EnvInner {
    data: HashMap<String, MalVal>,
    outer: Option<Env>,
}

impl EnvInner {
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
            outer: None,
        }
    }
}

impl Default for EnvInner {
    fn default() -> Self {
        let mut env = Self::new();

        env.set("+".into(), MalVal::Func(add));
        env.set("-".into(), MalVal::Func(sub));
        env.set("*".into(), MalVal::Func(mul));
        env.set("/".into(), MalVal::Func(div));

        env
    }
}

impl EnvInner {
    pub fn eval(self: &Env, ast: &MalVal) -> MalRet {
        match ast {
            MalVal::List(ast) => {
                if ast.is_empty() {
                    return Ok(MalVal::List(ast.clone()));
                }

                let MalVal::Sym(ref sym) = ast[0] else {
                    return Err(Error::TypeMismatch(MalVal::TN_SYMBOL, ast[0].type_name()).into());
                };

                let op = self.get(sym)?;

                let mut args = Vec::new();
                for value in &ast[1..] {
                    args.push(self.eval(value)?);
                }

                Ok(self.apply(op, args)?)
            }
            
            MalVal::Sym(sym) => Ok(self.get(&sym)?.clone()),

            MalVal::Vector(vec) => {
                let mut new_vec = Vec::with_capacity(vec.len());
                for value in vec.iter() {
                    new_vec.push(self.eval(value)?);
                }
                Ok(MalVal::List(Rc::new(new_vec)))
            }

            MalVal::Map(map) => {
                let mut new_map = HashMap::with_capacity(map.len());
                for (key, value) in map.iter() {
                    new_map.insert(key.clone(), self.eval(value)?);
                }
                Ok(MalVal::Map(Rc::new(new_map)))
            }

            // Types that evaluate to themselves:
            MalVal::Func(_)
            | MalVal::Str(_)
            | MalVal::Kwd(_)
            | MalVal::Int(_)
            | MalVal::Bool(_)
            | MalVal::Nil => Ok(ast.clone()),
        }
    }

    pub fn apply(&self, op: &MalVal, args: Vec<MalVal>) -> MalRet {
        match op {
            MalVal::Sym(sym) => Ok(self.get(sym).cloned()?),
            MalVal::Func(f) => f(args),
            MalVal::List(_)
            | MalVal::Vector(_)
            | MalVal::Map(_)
            | MalVal::Str(_)
            | MalVal::Kwd(_)
            | MalVal::Int(_)
            | MalVal::Bool(_)
            | MalVal::Nil => Err(Error::CannotCall(op.type_name()).into()),
        }
    }
}

impl EnvInner {
    pub fn set(&mut self, key: String, value: MalVal) {
        self.data.insert(key, value);
    }

    pub fn get(&self, key: &str) -> Result<&MalVal> {
        if let Some(value) = self.data.get(key) {
            return Ok(value);
        }

        match self.outer {
            Some(ref outer) => outer.get(key),
            None => Err(Error::NotFound(key.into())),
        }
    }
}

macro_rules! impl_sumop {
    ( $args:expr, $op:tt ) => {
        #[allow(clippy::assign_op_pattern)]
        'block: {
            let args = $args;
            if args.is_empty() {
                break 'block Err(Error::MissingParams.into())
            }
            let mut sum = args[0].to_int()?.clone();
            for i in 1..args.len() {
                sum = sum $op args[i].to_int()?;
            }
            Ok(MalVal::Int(sum))
        }
    };
}

fn add(args: MalArgs) -> MalRet {
    impl_sumop!(args, +)
}

fn sub(args: MalArgs) -> MalRet {
    impl_sumop!(args, -)
}

fn mul(args: MalArgs) -> MalRet {
    impl_sumop!(args, *)
}

fn div(args: MalArgs) -> MalRet {
    impl_sumop!(args, /)
}
