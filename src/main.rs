#![warn(clippy::pedantic)]

use std::{env, rc::Rc};

use ogj_mal::{list, re, rep, str, sym, Env, MalVal};
use rustyline::{error::ReadlineError, Editor};

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

    re(&env, r#"(println (str "Mal [" *host-language* "]"))"#)
        .expect("static str should be valid mal");

    loop {
        match rl.readline("user> ") {
            Ok(input) => {
                let _ = rl.add_history_entry(&input);
                rep(&env, &input);
            }
            Err(ReadlineError::Interrupted) => continue,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                eprintln!("readline error: {err:?}");
                break;
            }
        }

        let _ = rl.save_history(".mal-history");
    }
}
