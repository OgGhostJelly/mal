use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    list, re, reader, sym,
    types::{
        take_atleast_slice, take_atleast_vec, take_between_slice, take_fixed_slice, take_fixed_vec,
        MalArgs, MalRet, MalVal, RestBind,
    },
};

pub type Result<T> = std::result::Result<T, crate::Error>;

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
    #[error("expected between {0} and {1} param(s) got {2}")]
    BetweenParamsMismatch(usize, usize, usize),
    #[error("cannot put params after the & arg")]
    ParamsAfterRest,
    #[error("io: {0}")]
    Io(#[from] std::io::Error),
    #[error("try* form missing catch*")]
    TryMissingCatch,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Env(Rc<EnvInner>);

#[derive(PartialEq, Eq, Debug)]
struct EnvInner {
    data: RefCell<HashMap<String, MalVal>>,
    outer: Option<Env>,
}

impl Env {
    #[must_use]
    pub fn new(outer: Option<Env>) -> Self {
        Self(
            EnvInner {
                data: RefCell::new(HashMap::new()),
                outer,
            }
            .into(),
        )
    }
}

impl Default for Env {
    fn default() -> Self {
        let env = Self::new(None);
        env.apply_ns(crate::core::ns());
        re(
            &env,
            r#"(do
(def! not (fn* (a) (if a false true)))

(def! load-file (fn* [file] (eval (read-string
    (str "(do" (slurp file) "\n)"))))))"#,
        )
        .expect("builtin scripts should be valid mal");
        re(&env, "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")
        .expect("builtin scripts should be valid mal");
        env
    }
}

impl Env {
    #[inline]
    pub fn eval(self: &Env, ast_val: &MalVal) -> MalRet {
        self.clone().eval_inner(ast_val.clone())
    }

    pub fn eval_inner(mut self: Env, mut ast_val: MalVal) -> MalRet {
        loop {
            match ast_val {
                MalVal::List(ast) => {
                    if ast.is_empty() {
                        break Ok(MalVal::List(ast.clone()));
                    }

                    let ret = if let MalVal::Sym(ref sym) = ast[0] {
                        // handle special forms
                        let ast_in = &ast[1..];
                        match sym.as_str() {
                            "def!" => Ok(TcoRetInner::Ret(def(&self, ast_in)?)),
                            "defmacro!" => Ok(TcoRetInner::Ret(defmacro(&self, ast_in)?)),
                            "fn*" => Ok(TcoRetInner::Ret(r#fn(&self, ast_in)?)),
                            "quote" => Ok(TcoRetInner::Ret(quote(&self, ast_in)?)),
                            "quasiquote" => quasiquote(&self, ast_in),
                            "if" => r#if(&self, ast_in),
                            "do" => r#do(&self, ast_in),
                            "let*" => r#let(&self, ast_in),
                            "try*" => r#try(&self, ast_in),
                            _ => self.eval_list(&ast),
                        }
                    } else {
                        self.eval_list(&ast)
                    }?;

                    match ret {
                        TcoRetInner::Ret(val) => break Ok(val),
                        TcoRetInner::Unevaluated(env, val) => {
                            self = env;
                            ast_val = val;
                        }
                    }
                }

                MalVal::Sym(sym) => break Ok(self.get(&sym)?.clone()),

                MalVal::Vector(vec) => {
                    let mut new_vec = Vec::with_capacity(vec.len());
                    for value in vec.iter() {
                        new_vec.push(self.eval(value)?);
                    }
                    break Ok(MalVal::List(Rc::new(new_vec)));
                }

                MalVal::Map(map) => {
                    let mut new_map = HashMap::with_capacity(map.len());
                    for (key, value) in map.iter() {
                        new_map.insert(key.clone(), self.eval(value)?);
                    }
                    break Ok(MalVal::Map(Rc::new(new_map)));
                }

                // Types that evaluate to themselves:
                MalVal::Func(_, _)
                | MalVal::MalFunc { .. }
                | MalVal::Str(_)
                | MalVal::Kwd(_)
                | MalVal::Int(_)
                | MalVal::Bool(_)
                | MalVal::Atom(_)
                | MalVal::Nil => break Ok(ast_val.clone()),
            }
        }
    }

    fn eval_list(self: &Env, ast: &Rc<Vec<MalVal>>) -> TcoRet {
        let op = self.eval(&ast[0])?;

        if matches!(op, MalVal::MalFunc { is_macro, .. } if is_macro) {
            self.apply(&op, ast[1..].into())
        } else {
            let mut args = Vec::new();
            for value in &ast[1..] {
                args.push(self.eval(value)?);
            }

            self.apply(&op, args)
        }
    }

    pub fn apply(self: &Env, op: &MalVal, args: MalArgs) -> TcoRet {
        match op {
            MalVal::Sym(sym) => Ok(TcoRetInner::Ret(self.get(sym)?)),
            MalVal::Func(_, f) => f(self, args).map(TcoRetInner::Ret),
            MalVal::MalFunc {
                name,
                outer,
                binds,
                rest_bind,
                body,
                is_macro,
            } => {
                _ = (name, is_macro);

                let mut args = match rest_bind.as_ref() {
                    RestBind::None => take_fixed_vec(args, binds.len())?,
                    RestBind::Ignore | RestBind::Bind(_) => take_atleast_vec(args, binds.len())?,
                };

                let env = Rc::new(Env::new(Some(outer.clone())));

                let rest = args.split_off(binds.len());

                for (key, value) in binds.iter().zip(args) {
                    env.set(key.clone(), value);
                }

                if let RestBind::Bind(key) = rest_bind.as_ref() {
                    env.set(key.clone(), MalVal::List(Rc::new(rest)));
                }

                let ret = r#do(&env, &body[..])?;
                if *is_macro {
                    Ok(TcoRetInner::Unevaluated(outer.clone(), ret.tco_eval()?))
                } else {
                    Ok(ret)
                }
            }
            MalVal::List(_)
            | MalVal::Vector(_)
            | MalVal::Map(_)
            | MalVal::Str(_)
            | MalVal::Kwd(_)
            | MalVal::Int(_)
            | MalVal::Bool(_)
            | MalVal::Atom(_)
            | MalVal::Nil => Err(Error::CannotCall(op.type_name()).into()),
        }
    }

    pub fn apply_ns(&self, ns: &[(&'static str, MalVal)]) {
        for (key, value) in ns {
            self.set((*key).to_string(), value.clone());
        }
    }
}

impl Env {
    pub fn set(&self, key: String, mut value: MalVal) {
        if let MalVal::MalFunc { ref mut name, .. } = value {
            *name = Some(key.clone());
        } else if let MalVal::Func(ref mut name, _) = value {
            *name = Some(key.clone());
        }

        self.0.data.borrow_mut().insert(key, value);
    }

    pub fn get(&self, key: &str) -> MalRet {
        if let Some(value) = self.0.data.borrow().get(key) {
            return Ok(value.clone());
        }

        match self.0.outer {
            Some(ref outer) => outer.get(key),
            None => Err(Error::NotFound(key.into()).into()),
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

fn defmacro(env: &Env, ast: &[MalVal]) -> MalRet {
    let ast = take_fixed_slice::<2>(ast)?;

    let key = ast[0].to_sym()?;

    let mut value = env.eval(&ast[1])?;

    let MalVal::MalFunc {
        ref mut is_macro, ..
    } = value
    else {
        return Err(Error::TypeMismatch(MalVal::TN_FUNCTION, value.type_name()).into());
    };
    *is_macro = true;

    env.set(key.into(), value.clone());
    Ok(value)
}

/// Return value that could be used for tail-call optimizations.
pub type TcoRet = std::result::Result<TcoRetInner, crate::Error>;

pub enum TcoRetInner {
    /// Normal mal value returned.
    Ret(MalVal),
    /// Unevaluated mal value that should be tail call optimized.
    Unevaluated(Env, MalVal),
}

impl TcoRetInner {
    pub fn tco_eval(self) -> MalRet {
        match self {
            TcoRetInner::Ret(val) => Ok(val),
            TcoRetInner::Unevaluated(env, val) => env.eval(&val),
        }
    }
}

fn r#let(env: &Env, ast: &[MalVal]) -> TcoRet {
    let ast = take_atleast_slice(ast, 2)?;

    let binds = ast[0].to_seq()?;

    let env = Env::new(Some(env.clone()));

    if binds.len() % 2 != 0 {
        return Err(reader::Error::MismatchedKey.into());
    }

    for chunk in binds.chunks(2) {
        let key = chunk[0].to_sym()?;
        let value = &chunk[1];
        env.set(key.clone(), value.clone());
    }

    r#do(&env, &ast[1..])
}

fn r#do(env: &Env, ast: &[MalVal]) -> TcoRet {
    let ast = take_atleast_slice(ast, 1)?;
    let (ast, last) = ast.split_at(ast.len() - 1);
    for value in ast {
        env.eval(value)?;
    }
    Ok(TcoRetInner::Unevaluated(env.clone(), last[0].clone()))
}

fn r#if(env: &Env, ast: &[MalVal]) -> TcoRet {
    let ast = take_between_slice(ast, 2, 3)?;

    let cond = env.eval(&ast[0])?;

    if cond.is_truthy() {
        Ok(TcoRetInner::Unevaluated(env.clone(), ast[1].clone()))
    } else if let Some(r#else) = ast.get(2) {
        Ok(TcoRetInner::Unevaluated(env.clone(), r#else.clone()))
    } else {
        Ok(TcoRetInner::Ret(MalVal::Nil))
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
        };

        if binds_ast.next().is_some() {
            return Err(Error::ParamsAfterRest.into());
        }
    }

    let body = ast[1..].to_vec();

    Ok(MalVal::MalFunc {
        name: None,
        outer: env.clone(),
        binds: Rc::new(binds),
        rest_bind: Rc::new(rest_bind),
        body: Rc::new(body),
        is_macro: false,
    })
}

fn quote(_env: &Env, ast: &[MalVal]) -> MalRet {
    Ok(take_fixed_slice::<1>(ast)?[0].clone())
}

fn quasiquote(env: &Env, ast: &[MalVal]) -> TcoRet {
    let ast = &take_fixed_slice::<1>(ast)?[0];
    Ok(TcoRetInner::Unevaluated(
        env.clone(),
        quasiquote_inner(ast)?,
    ))
}

fn quasiquote_inner(ast: &MalVal) -> MalRet {
    match ast {
        MalVal::List(list) | MalVal::Vector(list) => {
            if matches!(list.first(), Some(MalVal::Sym(sym)) if sym == "unquote") {
                let val = &take_fixed_slice::<1>(&list[1..])?[0];
                return Ok(val.clone());
            }

            let mut new_list = list![];

            for elt in list.iter().rev() {
                if let MalVal::List(list) = elt {
                    if matches!(list.first(), Some(MalVal::Sym(sym)) if sym == "splice-unquote") {
                        let val = &take_fixed_slice::<1>(&list[1..])?[0];
                        new_list = list![sym!("concat"), val.clone(), new_list];
                        continue;
                    }
                }

                new_list = list![sym!("cons"), quasiquote_inner(elt)?, new_list];
            }

            if let MalVal::Vector(_) = ast {
                new_list = list![sym!("vec"), new_list];
            }

            Ok(new_list)
        }
        MalVal::Map(_) | MalVal::Sym(_) => Ok(list![sym!("quote"), ast.clone()]),
        _ => Ok(ast.clone()),
    }
}

fn r#try(env: &Env, ast: &[MalVal]) -> TcoRet {
    let ast = take_fixed_slice::<2>(ast)?;
    let (bind, body) = r#catch(&ast[1])?;

    match env.eval(&ast[0]) {
        Ok(val) => Ok(TcoRetInner::Ret(val)),
        Err(err) => {
            let env = Env::new(Some(env.clone()));
            env.set(bind.clone(), MalVal::Str(err.to_string()));
            r#do(&env, body)
        }
    }
}

fn r#catch(ast: &MalVal) -> Result<(&String, &[MalVal])> {
    let ast = ast.to_seq()?;
    let ast = take_atleast_slice(&ast[..], 3)?;
    if matches!(&ast[0], MalVal::Sym(sym) if sym != "catch*") {
        return Err(Error::TryMissingCatch.into());
    }

    Ok((ast[1].to_sym()?, &ast[2..]))
}
