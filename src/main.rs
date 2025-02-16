use std::io::{stdin, stdout, Write};

use shtml::rep;

fn main() {
    let stdin = stdin();
    let mut stdout = stdout();
    let mut input = String::new();

    loop {
        print!("user> ");
        let _ = stdout.flush();

        input.clear();
        match stdin.read_line(&mut input) {
            Ok(_) => {},
            Err(e) => {
                eprintln!("{e}");
                continue;
            },
        }

        match rep(&input) {
            Ok(()) => {},
            Err(e) => eprintln!("{e}"),
        }
    }
}
