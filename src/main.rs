#![allow(clippy::pedantic)]

use rustyline::{error::ReadlineError, Editor};
use shtml::{env::Env, rep};

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
    (def! not (fn* (a) (if a false true)))

    (def! load-file (fn* [file] (eval (read-string (slurp file)))))
    "#,
    )
    .expect("builtin loader should be valid mal");

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
