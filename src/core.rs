#![allow(
    clippy::unnecessary_wraps,
    reason = r#"Most functions in core.rs need to return `MalRet` to interface with mal
this falsely activates the clippy::unnecessary_wraps lint"#
)]

use crate::{
    env::Env,
    func,
    types::{take_fixed_vec, MalArgs, MalRet, MalVal},
};

pub const fn ns() -> &'static [(&'static str, MalVal)] {
    &[
        // math
        ("+", func!(math::add)),
        ("-", func!(math::sub)),
        ("*", func!(math::mul)),
        ("/", func!(math::div)),
        // cmp
        ("=", func!(cmp::eq)),
        ("<", func!(cmp::lt)),
        ("<=", func!(cmp::le)),
        (">", func!(cmp::gt)),
        (">=", func!(cmp::ge)),
        // string
        ("pr-str", func!(string::pr_str)),
        ("str", func!(string::str)),
        ("prn", func!(string::prn)),
        ("println", func!(string::println)),
        ("readline", func!(string::readline)),
        // meta
        ("read-string", func!(meta::read_string)),
        ("slurp", func!(meta::slurp)),
        ("eval", func!(meta::eval)),
        ("apply", func!(meta::apply)),
        // collection
        ("cons", func!(collection::cons)),
        ("concat", func!(collection::concat)),
        ("vec", func!(collection::vec)),
        ("nth", func!(collection::nth)),
        ("first", func!(collection::first)),
        ("rest", func!(collection::rest)),
        ("empty?", func!(collection::is_empty)),
        ("count", func!(collection::count)),
        ("map", func!(collection::map)),
        ("assoc", func!(collection::assoc)),
        ("dissoc", func!(collection::dissoc)),
        ("get", func!(collection::get)),
        ("contains?", func!(collection::contains)),
        ("keys", func!(collection::keys)),
        ("vals", func!(collection::vals)),
        // atom
        ("atom", func!(atom::atom)),
        ("atom?", func!(atom::is_atom)),
        ("deref", func!(atom::deref)),
        ("reset!", func!(atom::reset)),
        ("swap!", func!(atom::swap)),
        // cons
        ("list", func!(cons::list)),
        ("vector", func!(cons::vector)),
        ("symbol", func!(cons::symbol)),
        ("keyword", func!(cons::keyword)),
        ("hash-map", func!(cons::hashmap)),
        // predicate
        ("nil?", func!(predicate::nil)),
        ("true?", func!(predicate::r#true)),
        ("false?", func!(predicate::r#false)),
        ("symbol?", func!(predicate::symbol)),
        ("list?", func!(predicate::list)),
        ("vector?", func!(predicate::vector)),
        ("keyword?", func!(predicate::keyword)),
        ("sequential?", func!(predicate::sequential)),
        ("map?", func!(predicate::map)),
        ("fn?", func!(predicate::is_fn)),
        ("macro?", func!(predicate::is_macro)),
        // other
        ("throw", func!(throw)),
    ]
}

mod math {
    use crate::{
        env::Env,
        types::{take_atleast_vec, MalArgs, MalRet, MalVal},
    };

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

    pub fn add(_env: &Env, args: MalArgs) -> MalRet {
        impl_numop!(args, +)
    }

    pub fn sub(_env: &Env, args: MalArgs) -> MalRet {
        impl_numop!(args, -)
    }

    pub fn mul(_env: &Env, args: MalArgs) -> MalRet {
        impl_numop!(args, *)
    }

    pub fn div(_env: &Env, args: MalArgs) -> MalRet {
        impl_numop!(args, /)
    }
}

mod cmp {
    use crate::{
        env::Env,
        types::{take_atleast_vec, MalArgs, MalRet, MalVal},
    };

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

    pub fn eq(_env: &Env, args: MalArgs) -> MalRet {
        let args = take_atleast_vec(args, 1)?;
        let mut lhs = &args[0];
        for rhs in args.iter().skip(1) {
            if lhs != rhs {
                return Ok(MalVal::Bool(false));
            }
            lhs = rhs;
        }
        Ok(MalVal::Bool(true))
    }

    pub fn lt(_env: &Env, args: MalArgs) -> MalRet {
        impl_compop!(args, <)
    }

    pub fn le(_env: &Env, args: MalArgs) -> MalRet {
        impl_compop!(args, <=)
    }

    pub fn gt(_env: &Env, args: MalArgs) -> MalRet {
        impl_compop!(args, >)
    }

    pub fn ge(_env: &Env, args: MalArgs) -> MalRet {
        impl_compop!(args, >=)
    }
}

mod string {
    use crate::{
        env::Env,
        str, throw,
        types::{take_fixed_vec, MalArgs, MalRet, MalVal},
    };

    pub fn pr_str(_env: &Env, args: MalArgs) -> MalRet {
        Ok(MalVal::Str(
            args.into_iter()
                .map(|x| format!("{x:#}"))
                .collect::<Vec<_>>()
                .join(" "),
        ))
    }

    pub fn str(_env: &Env, args: MalArgs) -> MalRet {
        Ok(MalVal::Str(
            args.into_iter()
                .map(|x| format!("{x}"))
                .collect::<Vec<_>>()
                .join(" "),
        ))
    }

    pub fn prn(_env: &Env, args: MalArgs) -> MalRet {
        let string = args
            .into_iter()
            .map(|x| format!("{x:#}"))
            .collect::<Vec<_>>()
            .join(" ");
        println!("{string}");
        Ok(MalVal::Str(string))
    }

    pub fn println(_env: &Env, args: MalArgs) -> MalRet {
        let string = args
            .into_iter()
            .map(|x| format!("{x}"))
            .collect::<Vec<_>>()
            .join(" ");
        println!("{string}");
        Ok(MalVal::Str(string))
    }

    pub fn readline(_env: &Env, args: MalArgs) -> MalRet {
        let args = take_fixed_vec(args, 1)?;
        let prompt = args[0].to_str()?;

        let mut rl = rustyline::Editor::<(), rustyline::history::DefaultHistory>::new().unwrap();
        match rl.load_history(".mal-history") {
            Ok(()) => {}
            Err(e) => throw!(str!(e.to_string())),
        }

        match rl.readline(prompt) {
            Ok(input) => {
                let _ = rl.add_history_entry(&input);
                Ok(str!(input))
            }
            Err(
                rustyline::error::ReadlineError::Interrupted | rustyline::error::ReadlineError::Eof,
            ) => Ok(MalVal::Nil),
            Err(err) => throw!(str!(err.to_string())),
        }
    }
}

mod meta {
    use crate::{
        env::{Env, Error},
        reader,
        types::{take_atleast_vec, take_fixed_vec, MalArgs, MalRet, MalVal},
    };

    pub fn read_string(_env: &Env, args: MalArgs) -> MalRet {
        let args = take_fixed_vec(args, 1)?;
        let str = args[0].to_str()?;
        // TODO: better error handling
        let ret = match reader::read_str(str) {
            Err(reader::Error::None) => Ok(MalVal::Nil),
            Err(e) => Err(crate::Error::Custom(MalVal::Str(e.to_string())))?,
            ret => ret,
        };
        Ok(ret?)
    }

    pub fn slurp(_env: &Env, args: MalArgs) -> MalRet {
        let args = take_fixed_vec(args, 1)?;
        let file = args[0].to_str()?;
        let contents = match std::fs::read_to_string(file) {
            Ok(x) => x,
            Err(e) => return Err(Error::Io(e).into()),
        };
        Ok(MalVal::Str(contents))
    }

    pub fn eval(env: &Env, args: MalArgs) -> MalRet {
        let args = take_atleast_vec(args, 1)?;
        let (args, last) = args.split_at(args.len() - 1);

        let env = env.find_repl();

        for value in args {
            env.eval(value)?;
        }

        env.eval(&last[0])
    }

    pub fn apply(env: &Env, args: MalArgs) -> MalRet {
        let args = take_atleast_vec(args, 2)?;
        let fun = args[0].to_func()?;

        let mut new_args = Vec::with_capacity(args.len());

        for arg in &args[1..] {
            match arg {
                MalVal::List(list) => {
                    for arg in list.iter() {
                        new_args.push(arg.clone());
                    }
                }
                MalVal::Vector(vec) => {
                    for arg in vec.iter() {
                        new_args.push(arg.clone());
                    }
                }
                _ => new_args.push(arg.clone()),
            }
        }

        env.apply(fun, new_args)?.tco_eval()
    }
}

mod collection {
    use std::{collections::HashMap, rc::Rc};

    use saturating_cast::SaturatingCast as _;

    use crate::{
        env::{Env, Error},
        list,
        types::{take_atleast_vec, take_fixed_vec, MalArgs, MalRet, MalVal},
    };

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

        let index = (*args[1].to_int()?).saturating_cast::<usize>();
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

    pub fn is_empty(_env: &Env, args: MalArgs) -> MalRet {
        let args = take_fixed_vec(args, 1)?;
        match &args[0] {
            MalVal::List(list) => Ok(MalVal::Bool(list.is_empty())),
            MalVal::Vector(vec) => Ok(MalVal::Bool(vec.is_empty())),
            MalVal::Map(map) => Ok(MalVal::Bool(map.is_empty())),
            _ => Err(Error::TypeMismatch(MalVal::TN_SEQ, args[0].type_name()).into()),
        }
    }

    pub fn count(_env: &Env, args: MalArgs) -> MalRet {
        let args = take_fixed_vec(args, 1)?;

        match &args[0] {
            MalVal::List(list) => Ok(MalVal::Int(list.len().saturating_cast::<i64>())),
            MalVal::Vector(vec) => Ok(MalVal::Int(vec.len().saturating_cast::<i64>())),
            MalVal::Map(map) => Ok(MalVal::Int(map.len().saturating_cast::<i64>())),
            _ => Err(Error::TypeMismatch(MalVal::TN_SEQ, args[0].type_name()).into()),
        }
    }

    pub fn map(env: &Env, args: MalArgs) -> MalRet {
        let args = take_fixed_vec(args, 2)?;
        let fun = args[0].to_func()?;
        let seq = args[1].to_seq()?;

        let mut new_seq = Vec::with_capacity(seq.len());

        for value in seq.iter() {
            let ret = env.apply(fun, vec![value.clone()])?.tco_eval()?;
            new_seq.push(ret);
        }

        Ok(MalVal::List(new_seq.into()))
    }

    pub fn assoc(_env: &Env, args: MalArgs) -> MalRet {
        let args = take_atleast_vec(args, 1)?;
        let map = args[0].to_map()?;
        let args = &args[1..];

        if args.len() % 2 != 0 {
            return Err(Error::MismatchedMapKey.into());
        }

        let mut new_map = HashMap::with_capacity(map.len() + args.len() / 2);

        for (key, value) in map.iter() {
            new_map.insert(key.clone(), value.clone());
        }

        for chunk in args.chunks(2) {
            let key = chunk[0].to_map_key()?;
            let value = chunk[1].clone();
            new_map.insert(key, value);
        }

        Ok(new_map.into())
    }

    pub fn dissoc(_env: &Env, args: MalArgs) -> MalRet {
        let args = take_atleast_vec(args, 1)?;
        let map = args[0].to_map()?;
        let args = &args[1..];

        let mut new_map = HashMap::with_capacity(map.len().saturating_sub(args.len()));

        for (key, value) in map.iter() {
            new_map.insert(key.clone(), value.clone());
        }

        for key in args {
            new_map.remove(&key.to_map_key()?);
        }

        Ok(new_map.into())
    }

    pub fn get(_env: &Env, args: MalArgs) -> MalRet {
        let args = take_fixed_vec(args, 2)?;
        let map = args[0].to_map()?;
        let key = args[1].to_map_key()?;
        Ok(map.get(&key).cloned().into())
    }

    pub fn contains(_env: &Env, args: MalArgs) -> MalRet {
        let args = take_fixed_vec(args, 2)?;
        let map = args[0].to_map()?;
        let key = args[1].to_map_key()?;
        Ok(map.contains_key(&key).into())
    }

    pub fn keys(_env: &Env, args: MalArgs) -> MalRet {
        let args = take_fixed_vec(args, 1)?;
        let map = args[0].to_map()?;

        let keys = map.keys().cloned().map(Into::into).collect();

        Ok(MalVal::List(Rc::new(keys)))
    }

    pub fn vals(_env: &Env, args: MalArgs) -> MalRet {
        let args = take_fixed_vec(args, 1)?;
        let map = args[0].to_map()?;

        let keys = map.values().cloned().map(Into::into).collect();

        Ok(MalVal::List(Rc::new(keys)))
    }
}

mod atom {
    use std::{cell::RefCell, rc::Rc};

    use crate::{
        env::Env,
        types::{take_atleast_vec, take_fixed_vec, MalArgs, MalRet, MalVal},
    };

    pub fn atom(_env: &Env, args: MalArgs) -> MalRet {
        let mut args = take_fixed_vec(args, 1)?;
        let value = args.swap_remove(0);
        Ok(MalVal::Atom(Rc::new(RefCell::new(value))))
    }

    pub fn is_atom(_env: &Env, args: MalArgs) -> MalRet {
        for value in args {
            if !matches!(value, MalVal::Atom(_)) {
                return Ok(MalVal::Bool(false));
            }
        }
        Ok(MalVal::Bool(true))
    }

    pub fn deref(_env: &Env, args: MalArgs) -> MalRet {
        let args = take_fixed_vec(args, 1)?;
        let MalVal::Atom(ref value) = args[0] else {
            return Ok(args[0].clone());
        };
        let value = value.as_ref().borrow().clone();
        Ok(value)
    }

    pub fn reset(_env: &Env, args: MalArgs) -> MalRet {
        let args = take_fixed_vec(args, 2)?;
        let (atom, val) = (args[0].to_atom()?, &args[1]);
        atom.replace(val.clone());
        Ok(val.clone())
    }

    pub fn swap(env: &Env, args: MalArgs) -> MalRet {
        let mut args = take_atleast_vec(args, 2)?;
        let mut rest = args.split_off(2);

        let (atom, fun) = (args[0].to_atom()?, args[1].to_func()?);

        rest.insert(0, atom.borrow().clone());
        let ret = env.apply(fun, rest)?.tco_eval()?;

        atom.replace(ret.clone());
        Ok(ret)
    }
}

mod cons {
    use std::{collections::HashMap, rc::Rc};

    use crate::{
        env::{Env, Error},
        types::{take_fixed_vec, MalArgs, MalRet, MalVal},
    };

    pub fn list(_env: &Env, args: MalArgs) -> MalRet {
        Ok(MalVal::List(Rc::new(args)))
    }
    pub fn vector(_env: &Env, args: MalArgs) -> MalRet {
        Ok(MalVal::Vector(Rc::new(args)))
    }
    pub fn symbol(_env: &Env, args: MalArgs) -> MalRet {
        let args = take_fixed_vec(args, 1)?;
        let sym = args[0].to_str()?;
        Ok(MalVal::Sym(sym.to_string()))
    }
    pub fn keyword(_env: &Env, args: MalArgs) -> MalRet {
        let args = take_fixed_vec(args, 1)?;
        let sym = args[0].to_str()?;
        Ok(MalVal::Kwd(sym.to_string()))
    }
    #[allow(
        clippy::needless_pass_by_value,
        reason = "Needs to pass by value because the function has to have this signature"
    )]
    pub fn hashmap(_env: &Env, args: MalArgs) -> MalRet {
        if args.len() % 2 != 0 {
            return Err(Error::MismatchedMapKey.into());
        }

        let mut map = HashMap::with_capacity(args.len() / 2);

        for chunk in args.chunks(2) {
            let key = chunk[0].to_map_key()?;
            let value = chunk[1].clone();
            map.insert(key, value);
        }

        Ok(map.into())
    }
}

mod predicate {
    use crate::{
        env::Env,
        types::{MalArgs, MalRet},
        MalVal,
    };

    fn is_all(_env: &Env, args: MalArgs, pred: impl Fn(MalVal) -> bool) -> MalRet {
        for arg in args {
            if !pred(arg) {
                return Ok(false.into());
            }
        }
        Ok(true.into())
    }

    pub fn nil(env: &Env, args: MalArgs) -> MalRet {
        is_all(env, args, |val| val.is_nil())
    }
    pub fn r#true(env: &Env, args: MalArgs) -> MalRet {
        is_all(env, args, |val| val.is_truthy())
    }
    pub fn r#false(env: &Env, args: MalArgs) -> MalRet {
        is_all(env, args, |val| val.is_falsey())
    }
    pub fn symbol(env: &Env, args: MalArgs) -> MalRet {
        is_all(env, args, |val| matches!(val, MalVal::Sym(_)))
    }
    pub fn list(env: &Env, args: MalArgs) -> MalRet {
        is_all(env, args, |val| matches!(val, MalVal::List(_)))
    }
    pub fn vector(env: &Env, args: MalArgs) -> MalRet {
        is_all(env, args, |val| matches!(val, MalVal::Vector(_)))
    }
    pub fn keyword(env: &Env, args: MalArgs) -> MalRet {
        is_all(env, args, |val| matches!(val, MalVal::Kwd(_)))
    }
    pub fn sequential(env: &Env, args: MalArgs) -> MalRet {
        is_all(env, args, |val| {
            matches!(val, MalVal::List(_) | MalVal::Vector(_))
        })
    }
    pub fn map(env: &Env, args: MalArgs) -> MalRet {
        is_all(env, args, |val| matches!(val, MalVal::Map(_)))
    }
    pub fn is_fn(env: &Env, args: MalArgs) -> MalRet {
        is_all(env, args, |val| {
            matches!(val, MalVal::Func(..) | MalVal::MalFunc { .. })
        })
    }
    pub fn is_macro(env: &Env, args: MalArgs) -> MalRet {
        is_all(env, args, |val| match val {
            MalVal::MalFunc { is_macro, .. } => is_macro,
            _ => false,
        })
    }
}

fn throw(_env: &Env, args: MalArgs) -> MalRet {
    let args = &take_fixed_vec(args, 1)?[0];
    Err(crate::Error::Custom(args.clone()))
}
