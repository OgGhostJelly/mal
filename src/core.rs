use std::rc::Rc;

use crate::{
    env::Error,
    types::{take_atleast_vec, take_fixed_vec, MalArgs, MalRet, MalVal},
};

pub const fn ns() -> [(&'static str, MalVal); 17] {
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
        // Strings and printing
        ("pr-str", MalVal::Func(pr_str)),
        ("str", MalVal::Func(str)),
        ("print", MalVal::Func(print)),
        ("println", MalVal::Func(println)),
        // Misc
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

fn add(args: MalArgs) -> MalRet {
    impl_numop!(args, +)
}

fn sub(args: MalArgs) -> MalRet {
    impl_numop!(args, -)
}

fn mul(args: MalArgs) -> MalRet {
    impl_numop!(args, *)
}

fn div(args: MalArgs) -> MalRet {
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

fn eq(args: MalArgs) -> MalRet {
    impl_compop!(args, ==)
}

fn lt(args: MalArgs) -> MalRet {
    impl_compop!(args, <)
}

fn le(args: MalArgs) -> MalRet {
    impl_compop!(args, <=)
}

fn gt(args: MalArgs) -> MalRet {
    impl_compop!(args, >)
}

fn ge(args: MalArgs) -> MalRet {
    impl_compop!(args, >=)
}

// Strings and printing

fn pr_str(args: MalArgs) -> MalRet {
    Ok(MalVal::Str(
        args.into_iter()
            .map(|x| format!("{x:#}"))
            .collect::<Vec<_>>()
            .join(" "),
    ))
}

fn str(args: MalArgs) -> MalRet {
    Ok(MalVal::Str(
        args.into_iter()
            .map(|x| format!("{x}"))
            .collect::<Vec<_>>()
            .join(" "),
    ))
}

fn print(args: MalArgs) -> MalRet {
    let string = args
        .into_iter()
        .map(|x| format!("{x:#}"))
        .collect::<Vec<_>>()
        .join(" ");
    println!("{string}");
    Ok(MalVal::Str(string))
}

fn println(args: MalArgs) -> MalRet {
    let string = args
        .into_iter()
        .map(|x| format!("{x}"))
        .collect::<Vec<_>>()
        .join(" ");
    println!("{string}");
    Ok(MalVal::Str(string))
}

// Misc

fn list(args: MalArgs) -> MalRet {
    Ok(MalVal::List(Rc::new(args)))
}

fn is_list(args: MalArgs) -> MalRet {
    for value in args {
        if !matches!(value, MalVal::List(_)) {
            return Ok(MalVal::Bool(false));
        }
    }
    Ok(MalVal::Bool(true))
}

fn is_empty(args: MalArgs) -> MalRet {
    let args = take_fixed_vec(args, 1)?;
    match &args[0] {
        MalVal::List(list) => Ok(MalVal::Bool(list.is_empty())),
        MalVal::Vector(vec) => Ok(MalVal::Bool(vec.is_empty())),
        MalVal::Map(map) => Ok(MalVal::Bool(map.is_empty())),
        _ => Err(Error::TypeMismatch(MalVal::TN_SEQ, args[0].type_name()).into()),
    }
}

fn count(args: MalArgs) -> MalRet {
    let args = take_fixed_vec(args, 1)?;
    match &args[0] {
        MalVal::List(list) => Ok(MalVal::Int(list.len() as i64)),
        MalVal::Vector(vec) => Ok(MalVal::Int(vec.len() as i64)),
        MalVal::Map(map) => Ok(MalVal::Int(map.len() as i64)),
        _ => Err(Error::TypeMismatch(MalVal::TN_SEQ, args[0].type_name()).into()),
    }
}
