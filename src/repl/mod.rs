use crate::{lexer::Lexer, parser::Parser};
use std::io::{stdin, stdout, Write};

static MONKEY_FACE: &str = r#"            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"#;

/**
 * Main loop of REPL
 */
pub fn start() {
    let prompt = ">> ";
    let mut line = String::new();

    loop {
        // prompt and flush
        print!("{}", prompt);
        let _ = stdout().flush();

        // get the next line
        match stdin().read_line(&mut line) {
            Ok(_) => {
                // setup the lexerlex
                let lexer = Lexer::new(line.as_str());
                let mut parser = Parser::new(lexer);
                match parser.parse_program() {
                    Some(program) => {
                        let output: String = program.iter().map(|s| format!("{}\n", s)).collect();
                        println!("{}", output);
                    }
                    None => print_parse_errors(parser.errors()),
                }
            }
            Err(_) => break,
        }

        println!();
        line.clear();
    }
}

fn print_parse_errors(errors: Vec<String>) {
    println!("{}", MONKEY_FACE);
    println!("Whoops! We ran into some monkey business here!");
    println!("parser errors:");
    for e in errors {
        println!("{}", e);
    }
}
