use std::{cell::RefCell, cmp::Ordering, collections::HashMap, fmt, rc::Rc};

use crate::{
    env::{self, Env},
    printer, Error,
};

pub type MalRet = Result<MalVal, Error>;
pub type MalArgs = Vec<MalVal>;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum MalVal {
    List(Rc<Vec<MalVal>>),
    Vector(Rc<Vec<MalVal>>),
    Map(Rc<HashMap<MapKey, MalVal>>),
    Sym(String),
    Str(String),
    Kwd(String),
    Int(i64),
    Bool(bool),
    Func(Option<String>, fn(&Env, MalArgs) -> MalRet),
    MalFunc {
        name: Option<String>,
        outer: Env,
        binds: Rc<Vec<String>>,
        rest_bind: Rc<RestBind>,
        body: Rc<Vec<MalVal>>,
        is_macro: bool,
    },
    Nil,
    Atom(Rc<RefCell<MalVal>>),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum RestBind {
    /// Function without variadic arguments.
    None,
    /// Function with variadic arguments but the arguments are ignored.
    Ignore,
    /// Function with variadic arguments that get binded to the contained symbol.
    Bind(String),
}

impl MalVal {
    pub const TN_LIST: &'static str = "list";
    pub const TN_VECTOR: &'static str = "vector";
    pub const TN_MAP: &'static str = "map";
    pub const TN_SYMBOL: &'static str = "symbol";
    pub const TN_STRING: &'static str = "string";
    pub const TN_KEYWORD: &'static str = "keyword";
    pub const TN_INT: &'static str = "int";
    pub const TN_BOOL: &'static str = "bool";
    pub const TN_FUNCTION: &'static str = "function";
    pub const TN_NIL: &'static str = "nil";
    pub const TN_SEQ: &'static str = "seq";
    pub const TN_ATOM: &'static str = "atom";
    pub const TN_MAP_KEY: &'static str = "map-key";

    #[must_use]
    pub fn type_name(&self) -> &'static str {
        match self {
            MalVal::List(_) => Self::TN_LIST,
            MalVal::Vector(_) => Self::TN_VECTOR,
            MalVal::Map(_) => Self::TN_MAP,
            MalVal::Sym(_) => Self::TN_SYMBOL,
            MalVal::Str(_) => Self::TN_STRING,
            MalVal::Kwd(_) => Self::TN_KEYWORD,
            MalVal::Int(_) => Self::TN_INT,
            MalVal::Bool(_) => Self::TN_BOOL,
            MalVal::Func(_, _) | MalVal::MalFunc { .. } => Self::TN_FUNCTION,
            MalVal::Nil => Self::TN_NIL,
            MalVal::Atom(_) => Self::TN_ATOM,
        }
    }

    #[must_use]
    pub fn is_truthy(&self) -> bool {
        match self {
            MalVal::Bool(bool) => *bool,
            MalVal::Nil => false,
            _ => true,
        }
    }

    #[must_use]
    #[inline]
    pub fn is_falsey(&self) -> bool {
        !self.is_truthy()
    }

    #[must_use]
    pub fn is_nil(&self) -> bool {
        matches!(self, MalVal::Nil)
    }

    pub fn to_int(&self) -> Result<&i64, env::Error> {
        match self {
            MalVal::Int(i) => Ok(i),
            _ => Err(env::Error::TypeMismatch(Self::TN_INT, self.type_name())),
        }
    }

    pub fn to_sym(&self) -> Result<&String, env::Error> {
        match self {
            MalVal::Sym(sym) => Ok(sym),
            _ => Err(env::Error::TypeMismatch(Self::TN_SYMBOL, self.type_name())),
        }
    }

    pub fn to_map_key(&self) -> Result<MapKey, env::Error> {
        match self {
            MalVal::Str(str) => Ok(MapKey::Str(str.into())),
            MalVal::Kwd(str) => Ok(MapKey::Kwd(str.into())),
            _ => Err(env::Error::TypeMismatch(Self::TN_MAP_KEY, self.type_name())),
        }
    }

    pub fn to_seq(&self) -> Result<&Rc<Vec<MalVal>>, env::Error> {
        match self {
            MalVal::List(seq) | MalVal::Vector(seq) => Ok(seq),
            _ => Err(env::Error::TypeMismatch(Self::TN_SEQ, self.type_name())),
        }
    }

    pub fn to_list(&self) -> Result<&Rc<Vec<MalVal>>, env::Error> {
        match self {
            MalVal::List(seq) => Ok(seq),
            _ => Err(env::Error::TypeMismatch(Self::TN_SEQ, self.type_name())),
        }
    }

    pub fn to_map(&self) -> Result<&Rc<HashMap<MapKey, MalVal>>, env::Error> {
        match self {
            MalVal::Map(map) => Ok(map),
            _ => Err(env::Error::TypeMismatch(Self::TN_MAP, self.type_name())),
        }
    }

    pub fn to_str(&self) -> Result<&str, env::Error> {
        match self {
            MalVal::Str(str) => Ok(str),
            _ => Err(env::Error::TypeMismatch(Self::TN_STRING, self.type_name())),
        }
    }

    pub fn to_atom(&self) -> Result<&Rc<RefCell<MalVal>>, env::Error> {
        match self {
            MalVal::Atom(atom) => Ok(atom),
            _ => Err(env::Error::TypeMismatch(Self::TN_STRING, self.type_name())),
        }
    }

    pub fn to_func(&self) -> Result<&Self, env::Error> {
        match self {
            MalVal::Func(_, _) | MalVal::MalFunc { .. } => Ok(self),
            _ => Err(env::Error::TypeMismatch(
                Self::TN_FUNCTION,
                self.type_name(),
            )),
        }
    }
}

impl From<i64> for MalVal {
    fn from(value: i64) -> Self {
        Self::Int(value)
    }
}

impl From<bool> for MalVal {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl<T> From<Option<T>> for MalVal
where
    T: Into<MalVal>,
{
    fn from(value: Option<T>) -> Self {
        match value {
            Some(value) => value.into(),
            None => MalVal::Nil,
        }
    }
}

impl From<HashMap<MapKey, MalVal>> for MalVal {
    fn from(value: HashMap<MapKey, MalVal>) -> Self {
        Self::Map(value.into())
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum MapKey {
    Str(String),
    Kwd(String),
}

impl From<MapKey> for MalVal {
    fn from(value: MapKey) -> Self {
        match value {
            MapKey::Str(str) => Self::Str(str),
            MapKey::Kwd(kwd) => Self::Kwd(kwd),
        }
    }
}

impl fmt::Display for MapKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = match self {
            MapKey::Str(str) => MalVal::Str(str.clone()),
            MapKey::Kwd(str) => MalVal::Kwd(str.clone()),
        };
        printer::write_value(f, &value, f.alternate())
    }
}

impl fmt::Display for MalVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        printer::write_value(f, self, f.alternate())
    }
}

pub fn take_atleast_vec(value: MalArgs, at_least: usize) -> Result<MalArgs, Error> {
    match value.len().cmp(&at_least) {
        Ordering::Less => Err(env::Error::AtleastParamsMismatch(at_least, value.len()).into()),
        Ordering::Equal | Ordering::Greater => Ok(value),
    }
}

pub fn take_fixed_vec(value: MalArgs, len: usize) -> Result<MalArgs, Error> {
    match value.len().cmp(&len) {
        Ordering::Less | Ordering::Greater => {
            Err(env::Error::FixedParamsMismatch(len, value.len()).into())
        }
        Ordering::Equal => Ok(value),
    }
}

pub fn take_between_slice(value: &[MalVal], from: usize, to: usize) -> Result<&[MalVal], Error> {
    if value.len() < from || value.len() > to {
        return Err(env::Error::BetweenParamsMismatch(from, to, value.len()).into());
    }

    Ok(value)
}

pub fn take_atleast_slice(value: &[MalVal], at_least: usize) -> Result<&[MalVal], Error> {
    match value.len().cmp(&at_least) {
        Ordering::Less => Err(env::Error::AtleastParamsMismatch(at_least, value.len()).into()),
        Ordering::Equal | Ordering::Greater => Ok(value),
    }
}

pub fn take_fixed_slice<const N: usize>(value: &[MalVal]) -> Result<&[MalVal; N], Error> {
    match value.try_into() {
        Ok(value) => Ok(value),
        Err(_) => Err(env::Error::FixedParamsMismatch(N, value.len()).into()),
    }
}

#[macro_export]
macro_rules! list {
    () => (
        $crate::MalVal::List(std::rc::Rc::new(vec![]))
    );
    ($elem:expr; $n:expr) => (
        $crate::MalVal::List(std::rc::Rc::new(vec![$elem.into();$n]))
    );
    ($($x:expr),+ $(,)?) => {
        $crate::MalVal::List(std::rc::Rc::new(vec![$($x.into()),+]))
    };
}

#[macro_export]
macro_rules! mvec {
    ($($x:expr),+ $(,)?) => {
        $crate::MalVal::Vector(std::rc::Rc::new(vec![$($x.into()),+]))
    };
}

#[macro_export]
macro_rules! str {
    ( $x:expr ) => {
        $crate::MalVal::Str($x.into())
    };
}

#[macro_export]
macro_rules! sym {
    ( $x:expr ) => {
        $crate::MalVal::Sym($x.into())
    };
}

#[macro_export]
macro_rules! kwd {
    ( $x:expr ) => {
        $crate::MalVal::Kwd($x.into())
    };
}

#[macro_export]
macro_rules! atom {
    ( $x:expr ) => {
        $crate::MalVal::Atom(std::rc::Rc::new(std::cell::RefCell::new($x.into())))
    };
}

#[macro_export]
macro_rules! func {
    ( $x:expr ) => {
        $crate::MalVal::Func(None, $x)
    };
}
