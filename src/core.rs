// Most functions in core.rs need to return `MalRet` to interface with mal
// this falsely activates the clippy::unnecessary_wraps lint
#![allow(clippy::unnecessary_wraps)]

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
        ("print", func!(string::print)),
        ("println", func!(string::println)),
        // meta
        ("read-string", func!(meta::read_string)),
        ("slurp", func!(meta::slurp)),
        ("eval", func!(meta::eval)),
        // collection
        ("list", func!(collection::list)),
        ("list?", func!(collection::is_list)),
        ("cons", func!(collection::cons)),
        ("concat", func!(collection::concat)),
        ("vec", func!(collection::vec)),
        ("nth", func!(collection::nth)),
        ("first", func!(collection::first)),
        ("rest", func!(collection::rest)),
        ("empty?", func!(collection::is_empty)),
        ("count", func!(collection::count)),
        // atom
        ("atom", func!(atom::atom)),
        ("atom?", func!(atom::is_atom)),
        ("deref", func!(atom::deref)),
        ("reset!", func!(atom::reset)),
        ("swap!", func!(atom::swap)),
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
        impl_compop!(args, ==)
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
        types::{MalArgs, MalRet, MalVal},
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

    pub fn print(_env: &Env, args: MalArgs) -> MalRet {
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
        Ok(reader::read_str(str)?)
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
        for value in args {
            env.eval(value)?;
        }
        env.eval(&last[0])
    }
}

mod collection {
    use std::rc::Rc;

    use saturating_cast::SaturatingCast as _;

    use crate::{
        env::{Env, Error},
        list,
        types::{take_atleast_vec, take_fixed_vec, MalArgs, MalRet, MalVal},
    };

    pub fn list(_env: &Env, args: MalArgs) -> MalRet {
        Ok(MalVal::List(Rc::new(args)))
    }

    pub fn is_list(_env: &Env, args: MalArgs) -> MalRet {
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
}

mod atom {
    use std::{cell::RefCell, rc::Rc};

    use crate::{
        env::{Env, TcoRetInner},
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
        let value = args[0].to_atom()?;
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
        let ret = match env.apply(fun, rest)? {
            TcoRetInner::Ret(val) => val,
            TcoRetInner::Unevaluated(env, val) => env.eval(&val)?,
        };

        atom.replace(ret.clone());
        Ok(ret)
    }
}

fn throw(_env: &Env, args: MalArgs) -> MalRet {
    let args = &take_fixed_vec(args, 1)?[0];
    Err(crate::Error::Custom(args.clone()))
}
