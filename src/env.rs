use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    reader,
    types::{take_atleast_slice, take_fixed_slice, take_fixed_vec, MalArgs, MalRet, MalVal},
};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("'{0}' not found")]
    NotFound(String),
    #[error("type mismatch, expected '{0}' got '{1}'")]
    TypeMismatch(&'static str, &'static str),
    #[error("cannot call non-function type '{0}'")]
    CannotCall(&'static str),
    #[error("expected {0} param(s) got {1}")]
    FixedParamsMismatch(usize, usize),
    #[error("expected atleast {0} param(s) got {1}")]
    AtleastParamsMismatch(usize, usize),
}

pub type Env = Rc<EnvInner>;

#[derive(PartialEq, Eq, Debug)]
pub struct EnvInner {
    data: RefCell<HashMap<String, MalVal>>,
    outer: Option<Env>,
}

impl EnvInner {
    pub fn new(outer: Option<Env>) -> Self {
        Self {
            data: RefCell::new(HashMap::new()),
            outer,
        }
    }
}

impl Default for EnvInner {
    fn default() -> Self {
        let env = Self::new(None);
        for (key, value) in crate::core::ns() {
            env.set(key.to_string(), value);
        }
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

                // handle special forms
                if let MalVal::Sym(ref sym) = ast[0] {
                    let ast = &ast[1..];
                    match sym.as_str() {
                        "def!" => return def(self, ast),
                        "let*" => return r#let(self, ast),
                        "do" => return r#do(self, ast),
                        "if" => return r#if(self, ast),
                        "fn*" => return r#fn(self, ast),
                        _ => {}
                    }
                }

                let op = self.eval(&ast[0])?;

                let mut args = Vec::new();
                for value in &ast[1..] {
                    args.push(self.eval(value)?);
                }

                Ok(self.apply(op, args)?)
            }

            MalVal::Sym(sym) => Ok(self.get(sym)?.clone()),

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
            | MalVal::MalFunc { .. }
            | MalVal::Str(_)
            | MalVal::Kwd(_)
            | MalVal::Int(_)
            | MalVal::Bool(_)
            | MalVal::Nil => Ok(ast.clone()),
        }
    }

    pub fn apply(&self, op: MalVal, args: MalArgs) -> MalRet {
        match op {
            MalVal::Sym(sym) => Ok(self.get(&sym)?),
            MalVal::Func(f) => f(args),
            MalVal::MalFunc { outer, binds, body } => {
                let args = take_fixed_vec(args, binds.len())?;

                let env = Rc::new(EnvInner::new(Some(outer)));

                for (key, value) in binds.iter().zip(args) {
                    env.set(key.clone(), value);
                }

                r#do(&env, &body[..])
            }
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
    pub fn set(&self, key: String, value: MalVal) {
        self.data.borrow_mut().insert(key, value);
    }

    pub fn get(&self, key: &str) -> Result<MalVal> {
        if let Some(value) = self.data.borrow().get(key) {
            return Ok(value.clone());
        }

        match self.outer {
            Some(ref outer) => outer.get(key),
            None => Err(Error::NotFound(key.into())),
        }
    }
}

fn def(env: &Env, ast: &[MalVal]) -> MalRet {
    let ast = take_fixed_slice::<2>(ast)?;

    let MalVal::Sym(key) = &ast[0] else {
        return Err(Error::TypeMismatch(MalVal::TN_SYMBOL, ast[0].type_name()).into());
    };

    let value = env.eval(&ast[1])?;
    env.set(key.into(), value.clone());
    Ok(value)
}

fn r#let(env: &Env, ast: &[MalVal]) -> MalRet {
    let ast = take_atleast_slice(ast, 2)?;

    let (MalVal::List(binds) | MalVal::Vector(binds)) = &ast[0] else {
        return Err(Error::TypeMismatch(MalVal::TN_LIST, ast[0].type_name()).into());
    };

    let env: Env = Rc::new(EnvInner::new(Some(env.clone())));

    let mut key = None;
    for value in binds.iter() {
        match key.take() {
            Some(key) => env.set(key, value.clone()),
            None => match value {
                MalVal::Sym(sym) => key = Some(sym.clone()),
                _ => return Err(Error::TypeMismatch(MalVal::TN_SYMBOL, value.type_name()).into()),
            },
        }
    }
    // If a key was unprocessed then the user must've forget to add a value for a key
    if key.is_some() {
        return Err(reader::Error::MismatchedMapKey.into());
    }

    r#do(&env, &ast[1..])
}

fn r#do(env: &Env, ast: &[MalVal]) -> MalRet {
    let ast = take_atleast_slice(ast, 1)?;
    let mut ret = env.eval(&ast[0])?;
    for value in &ast[1..] {
        ret = env.eval(value)?;
    }
    Ok(ret)
}

fn r#if(env: &Env, ast: &[MalVal]) -> MalRet {
    let ast = take_fixed_slice::<3>(ast)?;

    if env.eval(&ast[0])?.is_truthy() {
        env.eval(&ast[0])
    } else {
        env.eval(&ast[2])
    }
}

fn r#fn(env: &Env, ast: &[MalVal]) -> MalRet {
    let ast = take_atleast_slice(ast, 2)?;

    let (MalVal::List(binds_ast) | MalVal::Vector(binds_ast)) = &ast[0] else {
        return Err(Error::TypeMismatch(MalVal::TN_LIST, ast[0].type_name()).into());
    };

    let mut binds = Vec::with_capacity(binds_ast.len());
    for bind in binds_ast.iter() {
        if let MalVal::Sym(sym) = bind {
            binds.push(sym.clone());
        } else {
            return Err(Error::TypeMismatch(MalVal::TN_SYMBOL, bind.type_name()).into());
        }
    }

    let body = ast[1..].to_vec();

    Ok(MalVal::MalFunc {
        outer: env.clone(),
        binds: Rc::new(binds),
        body: Rc::new(body),
    })
}
