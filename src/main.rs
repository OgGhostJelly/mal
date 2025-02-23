#![allow(clippy::pedantic)]

use std::{env, rc::Rc};

use rustyline::{error::ReadlineError, Editor};
use shtml::{list, rep, str, sym, Env, MalVal};

// TODO: error when unparsed tokens

fn main() {
    let env = Env::default();

    let mut args = env::args().skip(1);
    if let Some(file) = args.next() {
        let args: Vec<MalVal> = args.map(MalVal::Str).collect();
        let args = MalVal::List(Rc::new(args));
        env.set("*ARGV*".into(), args);

        let ret = env.eval(&list!(sym!("load-file"), str!(file)));
        return match ret {
            Ok(_) => {}
            Err(e) => eprintln!("{e}"),
        };
    }

    let mut rl = Editor::<(), rustyline::history::DefaultHistory>::new().unwrap();
    if rl.load_history(".mal-history").is_err() {
        eprintln!("No previous history.");
    }

    loop {
        match rl.readline("user> ") {
            Ok(input) => rep(env.clone(), &input),
            Err(ReadlineError::Interrupted) => continue,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }

        let _ = rl.save_history(".mal-history");
    }
}
