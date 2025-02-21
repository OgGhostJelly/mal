use std::{
    collections::{HashMap, LinkedList},
    fmt,
    rc::Rc,
};

use crate::{
    env::{Env, Error, Result},
    printer,
};

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Value {
    List(LinkedList<Value>),
    Vector(Vec<Value>),
    Map(HashMap<MapKey, Value>),
    Symbol(String),
    Str(String),
    Keyword(String),
    Int(i64),
    Bool(bool),
    Func(&'static str, fn(Args) -> Result),
    Closure(Closure),
    Nil,
    Ref(Rc<Value>),
}

impl Value {
    pub fn traverse_ref(&self) -> &Value {
        match self {
            Value::Ref(value) => value.as_ref(),
            _ => self,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Closure {
    pub env: Rc<Env>,
    pub binds: Vec<String>,
    pub body: Rc<Value>,
}

impl Closure {
    pub fn apply(&self, args: Args) -> Result {
        let env = Env::new(self.env.clone());

        if args.args.len() < self.binds.len() {
            return Err(Error::NotEnoughArgs);
        } else if args.args.len() > self.binds.len() {
            return Err(Error::TooManyArgs);
        }

        for (bind, expr) in self.binds.iter().zip(args) {
            env.set(bind.clone(), expr);
        }

        Ok(env.eval(Value::Ref(self.body.clone()))?)
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum MapKey {
    Str(String),
    Keyword(String),
}

impl fmt::Display for MapKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = match self {
            MapKey::Str(str) => Value::Str(str.clone()),
            MapKey::Keyword(str) => Value::Keyword(str.clone()),
        };
        printer::write_value(f, &value)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        printer::write_value(f, self)
    }
}

pub struct Args {
    args: Vec<Value>,
}

impl Args {
    pub fn new(args: Vec<Value>) -> Self {
        Self {
            args: args.into_iter().rev().collect(),
        }
    }

    pub fn next(&mut self) -> Option<Value> {
        self.args.pop()
    }

    pub fn take<const N: usize>(mut self) -> Option<[Value; N]> {
        if self.args.len() < N {
            return None;
        }

        let mut arr = [const { None }; N];

        for i in 0..N {
            arr[i] = self.args.pop();
        }

        Some(arr.map(|value| value.expect("all array values should be filled")))
    }
}

impl Iterator for Args {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        self.next()
    }
}

pub struct MacroArgs {
    args: LinkedList<Value>,
}

impl MacroArgs {
    pub fn new(args: LinkedList<Value>) -> Self {
        Self { args }
    }

    pub fn to_ast(self) -> LinkedList<Value> {
        self.args
    }

    pub fn next(&mut self) -> Option<Value> {
        self.args.pop_front()
    }

    pub fn grab<const N: usize>(&mut self) -> Option<[Value; N]> {
        if self.args.len() < N {
            return None;
        }

        let mut arr = [const { None }; N];

        for i in 0..N {
            arr[i] = self.args.pop_front();
        }

        Some(arr.map(|value| value.expect("all array values should be filled")))
    }
}

impl Iterator for MacroArgs {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        self.next()
    }
}
