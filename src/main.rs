#![allow(clippy::pedantic)]

use std::io::{stdin, stdout, Write};

use shtml::{env::Env, rep};

fn main() {
    let stdin = stdin();
    let mut stdout = stdout();
    let mut input = String::new();
    let env = Env::default();

    let _ = rep(env.clone(), "(def! not (fn* (a) (if a false true)))")
        .expect("not func should be valid mal");

    loop {
        print!("user> ");
        let _ = stdout.flush();

        input.clear();
        match stdin.read_line(&mut input) {
            Ok(_) => {}
            Err(e) => {
                eprintln!("{e}");
                continue;
            }
        }

        match rep(env.clone(), &input) {
            Ok(ret) => println!("> {ret:#}"),
            Err(e) => eprintln!("{e}"),
        }
    }
}
