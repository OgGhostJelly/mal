use std::{cell::RefCell, rc::Rc};

use crate::{
    env::{Env, Error, TcoRetInner},
    func, list, reader,
    types::{take_atleast_vec, take_fixed_vec, MalArgs, MalRet, MalVal},
};

pub const fn ns() -> &'static [(&'static str, MalVal)] {
    &[
        // Numeric operations
        ("+", func!(add)),
        ("-", func!(sub)),
        ("*", func!(mul)),
        ("/", func!(div)),
        // Comparative operations
        ("=", func!(eq)),
        ("<", func!(lt)),
        ("<=", func!(le)),
        (">", func!(gt)),
        (">=", func!(ge)),
        // Printing
        ("pr-str", func!(pr_str)),
        ("str", func!(str)),
        ("print", func!(print)),
        ("println", func!(println)),
        // Strings
        ("read-string", func!(read_string)),
        ("slurp", func!(slurp)),
        // List
        ("list", func!(list)),
        ("list?", func!(is_list)),
        ("cons", func!(cons)),
        ("concat", func!(concat)),
        ("vec", func!(vec)),
        ("nth", func!(nth)),
        ("first", func!(first)),
        ("rest", func!(rest)),
        // Misc
        ("eval", func!(eval)),
        ("empty?", func!(is_empty)),
        ("count", func!(count)),
        // Atom
        ("atom", func!(atom)),
        ("atom?", func!(is_atom)),
        ("deref", func!(deref)),
        ("reset!", func!(reset)),
        ("swap!", func!(swap)),
    ]
}

// Numeric operations

macro_rules! impl_numop {
    ( $args:expr, $op:tt ) => {
        #[allow(clippy::assign_op_pattern)]
        {
            let args = take_atleast_vec($args, 1)?;
            let mut sum = args[0].to_int()?.clone();
            for i in 1..args.len() {
                sum = sum $op args[i].to_int()?;
            }
            Ok(MalVal::Int(sum))
        }
    };
}

fn add(_env: &Env, args: MalArgs) -> MalRet {
    impl_numop!(args, +)
}

fn sub(_env: &Env, args: MalArgs) -> MalRet {
    impl_numop!(args, -)
}

fn mul(_env: &Env, args: MalArgs) -> MalRet {
    impl_numop!(args, *)
}

fn div(_env: &Env, args: MalArgs) -> MalRet {
    impl_numop!(args, /)
}

// Comparative operations

macro_rules! impl_compop {
    ( $args:expr, $op:tt ) => {
        {
            let args = take_atleast_vec($args, 1)?;
            let mut lhs = args[0].to_int()?.clone();
            for i in 1..args.len() {
                let rhs = *args[i].to_int()?;
                if !(lhs $op rhs) {
                    return Ok(MalVal::Bool(false))
                }
                lhs = rhs;
            }
            Ok(MalVal::Bool(true))
        }
    }
}

fn eq(_env: &Env, args: MalArgs) -> MalRet {
    impl_compop!(args, ==)
}

fn lt(_env: &Env, args: MalArgs) -> MalRet {
    impl_compop!(args, <)
}

fn le(_env: &Env, args: MalArgs) -> MalRet {
    impl_compop!(args, <=)
}

fn gt(_env: &Env, args: MalArgs) -> MalRet {
    impl_compop!(args, >)
}

fn ge(_env: &Env, args: MalArgs) -> MalRet {
    impl_compop!(args, >=)
}

// Printing

fn pr_str(_env: &Env, args: MalArgs) -> MalRet {
    Ok(MalVal::Str(
        args.into_iter()
            .map(|x| format!("{x:#}"))
            .collect::<Vec<_>>()
            .join(" "),
    ))
}

fn str(_env: &Env, args: MalArgs) -> MalRet {
    Ok(MalVal::Str(
        args.into_iter()
            .map(|x| format!("{x}"))
            .collect::<Vec<_>>()
            .join(" "),
    ))
}

fn print(_env: &Env, args: MalArgs) -> MalRet {
    let string = args
        .into_iter()
        .map(|x| format!("{x:#}"))
        .collect::<Vec<_>>()
        .join(" ");
    println!("{string}");
    Ok(MalVal::Str(string))
}

fn println(_env: &Env, args: MalArgs) -> MalRet {
    let string = args
        .into_iter()
        .map(|x| format!("{x}"))
        .collect::<Vec<_>>()
        .join(" ");
    println!("{string}");
    Ok(MalVal::Str(string))
}

// Strings

fn read_string(_env: &Env, args: MalArgs) -> MalRet {
    let args = take_fixed_vec(args, 1)?;
    let str = args[0].to_str()?;
    Ok(reader::read_str(str)?)
}

fn slurp(_env: &Env, args: MalArgs) -> MalRet {
    let args = take_fixed_vec(args, 1)?;
    let file = args[0].to_str()?;
    let contents = match std::fs::read_to_string(file) {
        Ok(x) => x,
        Err(e) => return Err(Error::Io(e).into()),
    };
    Ok(MalVal::Str(contents))
}

// List

fn list(_env: &Env, args: MalArgs) -> MalRet {
    Ok(MalVal::List(Rc::new(args)))
}

fn is_list(_env: &Env, args: MalArgs) -> MalRet {
    for value in args {
        if !matches!(value, MalVal::List(_)) {
            return Ok(MalVal::Bool(false));
        }
    }
    Ok(MalVal::Bool(true))
}

pub fn cons(_env: &Env, args: MalArgs) -> MalRet {
    let args = take_atleast_vec(args, 2)?;
    let (elems, to) = args.split_at(args.len() - 1);
    let to = &to[0];

    let seq = to.to_seq()?;

    let mut vec = Vec::with_capacity(elems.len() + seq.len());

    for value in elems {
        vec.push(value.clone());
    }

    for value in seq.iter() {
        vec.push(value.clone());
    }

    Ok(MalVal::List(vec.into()))
}

pub fn concat(_env: &Env, args: MalArgs) -> MalRet {
    let mut vec = Vec::new();
    for value in args {
        for value in value.to_seq()?.iter() {
            vec.push(value.clone());
        }
    }
    Ok(MalVal::List(vec.into()))
}

pub fn vec(_env: &Env, args: MalArgs) -> MalRet {
    let mut vec = Vec::new();
    for value in args {
        for value in value.to_seq()?.iter() {
            vec.push(value.clone());
        }
    }
    Ok(MalVal::Vector(Rc::new(vec)))
}

pub fn nth(_env: &Env, args: MalArgs) -> MalRet {
    let args = take_fixed_vec(args, 2)?;
    let seq = args[0].to_seq()?;
    let index = *args[1].to_int()? as usize;
    Ok(seq.get(index).cloned().into())
}

pub fn first(_env: &Env, args: MalArgs) -> MalRet {
    let args = take_fixed_vec(args, 1)?;
    let seq = args[0].to_seq()?;
    Ok(seq.first().cloned().into())
}

pub fn rest(_env: &Env, args: MalArgs) -> MalRet {
    let seq = &take_fixed_vec(args, 1)?[0];
    if seq.is_nil() {
        return Ok(list![]);
    }

    let seq = seq.to_seq()?;
    if seq.is_empty() {
        return Ok(list![]);
    }

    let mut new_vec = Vec::with_capacity(seq.len() - 1);
    for ele in &seq[1..] {
        new_vec.push(ele.clone());
    }
    Ok(MalVal::List(new_vec.into()))
}

// Misc

fn eval(env: &Env, args: MalArgs) -> MalRet {
    let args = take_atleast_vec(args, 1)?;
    let (args, last) = args.split_at(args.len() - 1);
    for value in args {
        env.eval(value)?;
    }
    env.eval(&last[0])
}

fn is_empty(_env: &Env, args: MalArgs) -> MalRet {
    let args = take_fixed_vec(args, 1)?;
    match &args[0] {
        MalVal::List(list) => Ok(MalVal::Bool(list.is_empty())),
        MalVal::Vector(vec) => Ok(MalVal::Bool(vec.is_empty())),
        MalVal::Map(map) => Ok(MalVal::Bool(map.is_empty())),
        _ => Err(Error::TypeMismatch(MalVal::TN_SEQ, args[0].type_name()).into()),
    }
}

fn count(_env: &Env, args: MalArgs) -> MalRet {
    let args = take_fixed_vec(args, 1)?;
    match &args[0] {
        MalVal::List(list) => Ok(MalVal::Int(list.len() as i64)),
        MalVal::Vector(vec) => Ok(MalVal::Int(vec.len() as i64)),
        MalVal::Map(map) => Ok(MalVal::Int(map.len() as i64)),
        _ => Err(Error::TypeMismatch(MalVal::TN_SEQ, args[0].type_name()).into()),
    }
}

// Atom

fn atom(_env: &Env, args: MalArgs) -> MalRet {
    let mut args = take_fixed_vec(args, 1)?;
    let value = args.swap_remove(0);
    Ok(MalVal::Atom(Rc::new(RefCell::new(value))))
}

fn is_atom(_env: &Env, args: MalArgs) -> MalRet {
    for value in args {
        if !matches!(value, MalVal::Atom(_)) {
            return Ok(MalVal::Bool(false));
        }
    }
    Ok(MalVal::Bool(true))
}

fn deref(_env: &Env, args: MalArgs) -> MalRet {
    let args = take_fixed_vec(args, 1)?;
    let value = args[0].to_atom()?;
    let value = value.as_ref().borrow().clone();
    Ok(value)
}

fn reset(_env: &Env, args: MalArgs) -> MalRet {
    let args = take_fixed_vec(args, 2)?;
    let (atom, val) = (args[0].to_atom()?, &args[1]);
    atom.replace(val.clone());
    Ok(val.clone())
}

fn swap(env: &Env, args: MalArgs) -> MalRet {
    let mut args = take_atleast_vec(args, 2)?;
    let mut rest = args.split_off(2);

    let (atom, fun) = (args[0].to_atom()?, args[1].to_func()?);

    rest.insert(0, atom.borrow().clone());
    let ret = match env.apply(fun, rest)? {
        TcoRetInner::Ret(val) => val,
        TcoRetInner::Unevaluated(env, val) => env.eval(&val)?,
    };

    atom.replace(ret.clone());
    Ok(ret)
}
