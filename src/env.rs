use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    reader,
    types::{
        take_atleast_slice, take_atleast_vec, take_fixed_slice, take_fixed_vec, MalArgs, MalRet,
        MalVal, RestBind,
    },
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
            MalVal::MalFunc {
                outer,
                binds,
                rest_bind,
                body,
            } => {
                let mut args = match rest_bind.as_ref() {
                    RestBind::None => take_fixed_vec(args, binds.len())?,
                    RestBind::Ignore | RestBind::Bind(_) => take_atleast_vec(args, binds.len())?,
                };

                let env = Rc::new(EnvInner::new(Some(outer)));

                let rest = args.split_off(binds.len());

                for (key, value) in binds.iter().zip(args) {
                    env.set(key.clone(), value);
                }

                if let RestBind::Bind(key) = rest_bind.as_ref() {
                    env.set(key.clone(), MalVal::List(Rc::new(rest)));
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

    let key = ast[0].to_sym()?;

    let value = env.eval(&ast[1])?;
    env.set(key.into(), value.clone());
    Ok(value)
}

fn r#let(env: &Env, ast: &[MalVal]) -> MalRet {
    let ast = take_atleast_slice(ast, 2)?;

    let binds = ast[0].to_seq()?;

    let env: Env = Rc::new(EnvInner::new(Some(env.clone())));

    let mut key = None;
    for value in binds.iter() {
        match key.take() {
            Some(key) => env.set(key, value.clone()),
            None => key = Some(value.to_sym()?.clone()),
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

    let cond = env.eval(&ast[0])?;

    if cond.is_truthy() {
        env.eval(&ast[1])
    } else {
        env.eval(&ast[2])
    }
}

fn r#fn(env: &Env, ast: &[MalVal]) -> MalRet {
    let ast = take_atleast_slice(ast, 2)?;

    let mut binds_ast = ast[0].to_seq()?.iter();

    let mut binds = Vec::with_capacity(binds_ast.len());
    let mut rest_bind = RestBind::None;

    while let Some(val) = binds_ast.next() {
        let key = val.to_sym()?.clone();
        if key != "&" {
            binds.push(key);
            continue;
        }

        rest_bind = match binds_ast.next() {
            Some(sym) => RestBind::Bind(sym.to_sym()?.clone()),
            None => RestBind::Ignore,
        }
    }

    let body = ast[1..].to_vec();

    Ok(MalVal::MalFunc {
        outer: env.clone(),
        binds: Rc::new(binds),
        rest_bind: Rc::new(rest_bind),
        body: Rc::new(body),
    })
}
