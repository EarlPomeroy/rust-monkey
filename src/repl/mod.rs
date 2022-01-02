use crate::{lexer::Lexer, token::Token};
use std::io::{stdin, stdout, Write};

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
                // setup the lexer
                let mut lexer = Lexer::new(line.as_str());

                // get each token and print it. Break on EOF.
                loop {
                    let tok = lexer.next_token();
                    if tok == Token::EOF {
                        break;
                    } else {
                        println!("{:?}", tok);
                    }
                }
                println!();
                // clear the buffer for the next line
                line.clear();
            }
            Err(_) => break,
        }
    }
}
