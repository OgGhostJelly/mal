#![allow(clippy::pedantic)]

use std::{env, rc::Rc};

use rustyline::{error::ReadlineError, Editor};
use shtml::{env::Env, rep, types::MalVal};

// TODO: error when unparsed tokens

fn main() {
    // `()` can be used when no completer is required
    let mut rl = Editor::<(), rustyline::history::DefaultHistory>::new().unwrap();
    if rl.load_history(".mal-history").is_err() {
        eprintln!("No previous history.");
    }

    let env = Env::default();

    let _ = rep(
        env.clone(),
        r#"
    (do
        (def! not (fn* (a) (if a false true)))

        (def! load-file (fn* [file] (eval (read-string (slurp file))))))
    "#,
    )
    .expect("builtin scripts should be valid mal");

    let mut args = env::args().skip(1);
    if let Some(file) = args.next() {
        _ = env
            .eval(&MalVal::List(Rc::new(vec![
                MalVal::Sym("load-file".into()),
                MalVal::Str(file),
            ])))
            .expect("builtin scripts should be valid mal");

        let args: Vec<MalVal> = args.map(MalVal::Str).collect();
        let args = MalVal::List(Rc::new(args));
        env.set("*ARGV*".into(), args);
    }

    loop {
        match rl.readline("user> ") {
            Ok(input) => {
                if !input.is_empty() {
                    match rep(env.clone(), &input) {
                        Ok(ret) => println!("> {ret:#}"),
                        Err(e) => eprintln!("{e}"),
                    }
                }
            }
            Err(ReadlineError::Interrupted) => continue,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("error: {:?}", err);
                break;
            }
        }
    }
}
