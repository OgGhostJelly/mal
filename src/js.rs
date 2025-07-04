use std::{
    collections::HashMap,
    fmt::{self, Display},
    hash::{DefaultHasher, Hash, Hasher},
    rc::Rc,
};

use crate::{
    env, func, list,
    printer::escape_str,
    reader, str, sym,
    types::{take_atleast_slice, take_atleast_vec, take_fixed_slice, MalArgs, MapKey as MalMapKey},
    Env, Error, MalRet, MalVal,
};

pub const fn ns() -> &'static [(&'static str, MalVal)] {
    &[("mal->js", func!(mal2js))]
}

fn mal2js(_: &Env, args: MalArgs) -> MalRet {
    let ast = take_atleast_vec(args, 1)?;

    if ast.len() == 1 {
        let js = format!("{}", compile(ast[0].clone())?);
        Ok(str!(js))
    } else {
        let ast = list!(sym!("do"), MalVal::List(Rc::new(ast)));
        let js = format!("{}", compile(ast)?);
        Ok(str!(js))
    }
}

pub fn compile_str(input: &str) -> Result<JsVal, Error> {
    let Some(ast) = reader::read_str(input)? else {
        return Ok(JsExpr::Undefined.into());
    };
    compile(ast)
}

pub fn compile(ast: MalVal) -> Result<JsVal, Error> {
    let mut block = Block::empty();
    let value = compile_(&mut block, ast)?;
    if block.0.is_empty() {
        Ok(value)
    } else {
        let value = block.push(value);
        block.0.push(JsStmt::Return(Some(value)).into());
        Ok(JsStmt::Block(block).into())
    }
}

fn compile_(block: &mut Block, ast: MalVal) -> Result<JsVal, Error> {
    match ast {
        MalVal::List(ls) => {
            if ls.is_empty() {
                return Ok(JsExpr::Array(vec![]).into());
            }

            let op = &ls[0];
            let args = &ls[1..];

            if let MalVal::Sym(sym) = op {
                match sym.as_str() {
                    "+" => {
                        return mal_seq_to_js(block, args.iter(), |args| {
                            JsExpr::Math(MathOp::Add, args).into()
                        })
                    }
                    "-" => {
                        return mal_seq_to_js(block, args.iter(), |args| {
                            JsExpr::Math(MathOp::Sub, args).into()
                        })
                    }
                    "/" => {
                        return mal_seq_to_js(block, args.iter(), |args| {
                            JsExpr::Math(MathOp::Div, args).into()
                        })
                    }
                    "*" => {
                        return mal_seq_to_js(block, args.iter(), |args| {
                            JsExpr::Math(MathOp::Mul, args).into()
                        })
                    }

                    "fn*" => return args_to_func(args).map(Into::into),
                    "def!" => {
                        let args = take_fixed_slice::<2>(args)?;
                        let bind = str_to_sym(args[0].to_sym()?);
                        let value = compile_(block, args[1].clone())?;
                        let value = block.push(value);

                        return Ok(match value {
                            JsExpr::Function {
                                name: None,
                                params,
                                rest,
                                body,
                            } => JsExpr::Function {
                                name: Some(bind),
                                params,
                                rest,
                                body,
                            }
                            .into(),
                            value => JsStmt::Let(bind, value).into(),
                        });
                    }
                    "do" => {
                        if args.is_empty() {
                            return Ok(JsExpr::Undefined.into());
                        }

                        let (args, last) = args.split_at(args.len() - 1);

                        for value in args {
                            let value = compile_(block, value.clone())?;
                            block.0.push(value);
                        }

                        return compile_(block, last[0].clone());
                    }
                    _ => {}
                }
            }

            ls_to_call(block, &ls)
        }
        MalVal::Vector(vals) => {
            mal_seq_to_js(block, vals.iter(), |vals| JsExpr::Array(vals).into())
        }
        MalVal::Map(map) => Ok(JsExpr::Object(mal_map_to_js(block, &map)?).into()),
        MalVal::Sym(sym) => Ok(JsExpr::Symbol(str_to_sym(sym)).into()),
        MalVal::Str(str) => Ok(JsExpr::String(str).into()),
        MalVal::Kwd(kwd) => Ok(JsExpr::String(kwd_to_js(&kwd)).into()),
        MalVal::Int(value) => Ok(JsExpr::Int(value).into()),
        MalVal::Bool(value) => Ok(JsExpr::Bool(value).into()),
        MalVal::Func(_, _) | MalVal::MalFunc { .. } => {
            unreachable!("ast shouldn't contain any function types")
        }
        MalVal::Nil => Ok(JsExpr::Null.into()),
        MalVal::Atom(_) => unimplemented!("js compiler doesn't support atom types yet"),
    }
}

#[derive(Debug)]
pub enum JsVal {
    Expr(JsExpr),
    Stmt(JsStmt),
}

impl From<JsExpr> for JsVal {
    fn from(value: JsExpr) -> Self {
        Self::Expr(value)
    }
}

impl From<JsStmt> for JsVal {
    fn from(value: JsStmt) -> Self {
        Self::Stmt(value)
    }
}

#[derive(Debug)]
pub enum JsExpr {
    Math(MathOp, Vec<JsExpr>),
    Array(Vec<JsExpr>),
    Object(HashMap<String, JsExpr>),
    Bool(bool),
    Int(i64),
    String(String),
    Symbol(Symbol),
    Undefined,
    Function {
        name: Option<Symbol>,
        params: Vec<Symbol>,
        rest: Option<Symbol>,
        body: Block,
    },
    Call {
        bind: Symbol,
        args: Vec<JsExpr>,
    },
    Null,
}

#[derive(Debug)]
pub enum JsStmt {
    Let(Symbol, JsExpr),
    Return(Option<JsExpr>),
    Block(Block),
}

#[derive(Debug)]
pub enum MathOp {
    Add,
    Sub,
    Div,
    Mul,
}

#[derive(Debug, Clone)]
pub struct Symbol(String);

#[derive(Debug)]
pub struct Block(Vec<JsVal>);

impl Block {
    pub fn set(&mut self, key: Symbol, value: JsExpr) -> Symbol {
        self.0.push(JsStmt::Let(key.clone(), value).into());
        key
    }

    pub fn push(&mut self, value: JsVal) -> JsExpr {
        match value {
            JsVal::Expr(value) => value,
            #[expect(
                clippy::match_wildcard_for_single_variants,
                reason = "this is suppose to match all future variants of the enum"
            )]
            JsVal::Stmt(mut value) => match value {
                JsStmt::Let(symbol, expr) => {
                    let symbol = self.set(symbol, expr);
                    JsExpr::Symbol(symbol)
                }
                JsStmt::Block(ref mut other_block) => self.append(other_block),
                value => self.push(JsVal::Stmt(value)),
            },
        }
    }

    pub fn append(&mut self, value: &mut Block) -> JsExpr {
        let Some(last) = value.0.pop() else {
            return JsExpr::Undefined;
        };
        self.0.append(&mut value.0);
        self.push(last)
    }

    #[must_use]
    pub fn empty() -> Self {
        Self(vec![])
    }
}

fn mal_seq_to_js<'a, I>(
    block: &mut Block,
    seq: I,
    map: impl Fn(Vec<JsExpr>) -> JsVal,
) -> Result<JsVal, Error>
where
    I: Iterator<Item = &'a MalVal> + ExactSizeIterator,
{
    let mut exprs = vec![];
    for value in seq {
        let value = compile_(block, value.clone())?;
        exprs.push(block.push(value));
    }

    Ok(map(exprs))
}

fn mal_map_to_js(
    block: &mut Block,
    map: &Rc<HashMap<MalMapKey, MalVal>>,
) -> Result<HashMap<String, JsExpr>, Error> {
    let mut new_map = HashMap::with_capacity(map.len());
    for (key, value) in map.iter() {
        let key = match key.clone() {
            MalMapKey::Str(str) => str,
            MalMapKey::Kwd(kwd) => kwd_to_js(&kwd),
        };

        let value = compile_(block, value.clone())?;
        new_map.insert(key, block.push(value));
    }
    Ok(new_map)
}

fn kwd_to_js(kwd: &str) -> String {
    let mut hasher = DefaultHasher::new();
    kwd.hash(&mut hasher);
    let hash = hasher.finish();
    format!("{kwd}#{hash}")
}

fn str_to_sym<T: Into<String>>(s: T) -> Symbol {
    let s = s.into();

    if !s
        .chars()
        .all(|ch| ch.is_alphanumeric() || ch == '_' || ch == '$' || ch == '.')
    {
        unimplemented!("js doesn't support those characters")
    }

    Symbol(s)
}

fn ls_to_call(block: &mut Block, ls: &Rc<Vec<MalVal>>) -> Result<JsVal, Error> {
    let ls = take_atleast_slice(ls.as_slice(), 1)?;

    let bind = str_to_sym(ls[0].to_sym()?);

    let args = &ls[1..];
    mal_seq_to_js(block, args.iter(), move |args| {
        JsExpr::Call {
            bind: bind.clone(),
            args,
        }
        .into()
    })
}

fn args_to_func(args: &[MalVal]) -> Result<JsExpr, Error> {
    let args = take_atleast_slice(args, 1)?;

    let params_ast = args[0].to_seq()?;
    let mut params_iter = params_ast.iter();
    let mut params = Vec::with_capacity(params_ast.len());
    let mut rest = None;

    while let Some(value) = params_iter.next() {
        let value = value.to_sym()?;

        if value == "&" {
            if let Some(value) = params_iter.next() {
                rest = Some(str_to_sym(value.to_sym()?));
            }
            break;
        }

        params.push(str_to_sym(value));
    }

    if params_iter.next().is_some() {
        return Err(env::Error::ParamsAfterRest.into());
    }

    let mut body = Block(Vec::with_capacity(args.len()));
    let args = &args[1..];
    for value in args {
        let ret = compile_(&mut body, value.clone())?;
        body.push(ret);
    }

    Ok(JsExpr::Function {
        name: None,
        params,
        rest,
        body,
    })
}

impl fmt::Display for JsVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JsVal::Expr(expr) => expr.fmt(f),
            JsVal::Stmt(stmt) => stmt.fmt(f),
        }
    }
}

impl fmt::Display for JsExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JsExpr::Math(op, exprs) => join(f, "(", ")", exprs.iter(), op),
            JsExpr::Array(exprs) => join(f, "[", "]", exprs.iter(), ","),
            JsExpr::Object(exprs) => {
                struct MapPair<'a>((&'a String, &'a JsExpr));

                impl fmt::Display for MapPair<'_> {
                    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                        write!(f, "\"{}\":{}", escape_str(self.0 .0), self.0 .1)
                    }
                }

                join(f, "{", "}", exprs.iter().map(MapPair), ",")
            }
            JsExpr::Bool(value) => write!(f, "{value}"),
            JsExpr::Int(value) => write!(f, "{value}"),
            JsExpr::String(str) => write!(f, "\"{}\"", escape_str(str)),
            JsExpr::Symbol(symbol) => write!(f, "{symbol}"),
            JsExpr::Undefined => write!(f, "undefined"),
            JsExpr::Function {
                name,
                params,
                rest,
                body,
            } => {
                if let Some(name) = name {
                    write!(f, "function {name}")?;
                }

                join(f, "(", "", params.iter(), ",")?;
                if let Some(rest) = rest {
                    if params.len() > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "...{rest}")?;
                }
                write!(f, ")")?;

                if name.is_none() {
                    write!(f, "=>")?;
                }

                join(f, "{", "}", body.0.iter(), ";")
            }
            JsExpr::Call { bind, args } => {
                write!(f, "{bind}")?;
                join(f, "(", ")", args.iter(), ",")
            }
            JsExpr::Null => write!(f, "null"),
        }
    }
}

impl fmt::Display for JsStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JsStmt::Let(symbol, expr) => write!(f, "let {symbol}={expr}"),
            JsStmt::Return(symbol) => match symbol {
                Some(symbol) => write!(f, "return {symbol}"),
                None => write!(f, "return;"),
            },
            JsStmt::Block(block) => {
                for value in &block.0 {
                    write!(f, "{value};")?;
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl fmt::Display for MathOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MathOp::Add => write!(f, "+"),
            MathOp::Sub => write!(f, "-"),
            MathOp::Div => write!(f, "/"),
            MathOp::Mul => write!(f, "*"),
        }
    }
}

fn join(
    f: &mut fmt::Formatter<'_>,
    start: &str,
    end: &str,
    mut iter: impl Iterator<Item = impl Display>,
    sep: impl Display,
) -> fmt::Result {
    write!(f, "{start}")?;

    if let Some(value) = iter.next() {
        write!(f, "{value}")?;
    }

    for value in iter {
        write!(f, "{sep}{value}")?;
    }

    write!(f, "{end}")?;

    Ok(())
}
