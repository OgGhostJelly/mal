use crate::env::Error;
use crate::types::{MalArgs, MalRet, MalVal};

pub const NS: &[(&'static str, MalVal)] = &[
    // Numeric operations
    ("+", MalVal::Func(add)),
    ("-", MalVal::Func(sub)),
    ("*", MalVal::Func(mul)),
    ("/", MalVal::Func(div)),
    //
    ("prn", MalVal::Func(prn)),
];

fn prn(args: MalArgs) -> MalRet {
    if args.is_empty() {
        return Err(Error::MissingParams.into());
    }
    println!("{:#}", args[0]);
    Ok(MalVal::Nil)
}

macro_rules! impl_numop {
    ( $args:expr, $op:tt ) => {
        #[allow(clippy::assign_op_pattern)]
        'block: {
            let args = $args;
            if args.is_empty() {
                break 'block Err(Error::MissingParams.into())
            }
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
