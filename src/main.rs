mod lexer;
mod repl;
mod token;
mod ast;
mod parser;

fn main() {
    println!("Welcome the the Monkey programming language!");
    println!("Feel free to type in commands");
    repl::start();
}
