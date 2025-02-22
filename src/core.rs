use std::rc::Rc;

use crate::{
    env::{Env, Error},
    reader,
    types::{take_atleast_vec, take_fixed_vec, MalArgs, MalRet, MalVal},
};

pub const fn ns() -> [(&'static str, MalVal); 20] {
    [
        // Numeric operations
        ("+", MalVal::Func(add)),
        ("-", MalVal::Func(sub)),
        ("*", MalVal::Func(mul)),
        ("/", MalVal::Func(div)),
        // Comparative operations
        ("=", MalVal::Func(eq)),
        ("<", MalVal::Func(lt)),
        ("<=", MalVal::Func(le)),
        (">", MalVal::Func(gt)),
        (">=", MalVal::Func(ge)),
        // Printing
        ("pr-str", MalVal::Func(pr_str)),
        ("str", MalVal::Func(str)),
        ("print", MalVal::Func(print)),
        ("println", MalVal::Func(println)),
        // Strings
        ("read-string", MalVal::Func(read_string)),
        ("slurp", MalVal::Func(slurp)),
        // Misc
        ("eval", MalVal::Func(eval)),
        ("list", MalVal::Func(list)),
        ("list?", MalVal::Func(is_list)),
        ("empty?", MalVal::Func(is_empty)),
        ("count", MalVal::Func(count)),
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
        Err(e) => todo!("io error handling: {e}"),
    };
    Ok(MalVal::Str(contents))
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
