use crate::{lexer::Lexer, token::Token};
use std::io::{stdin, stdout, Write};

pub fn start() {
    let prompt = ">>";
    let mut line = String::new();

    loop {
        print!("{} ", prompt);
        let _ = stdout().flush();
        match stdin().read_line(&mut line) {
            Ok(_) => {
                let mut lexer = Lexer::new(line.as_str());

                loop {
                    let tok = lexer.next_token();
                    if tok == Token::EOF {
                        break;
                    } else {
                        println!("{:?}", tok);
                    }
                }
                println!("");
                line.clear();
            }
            Err(_) => break,
        }
    }
}
