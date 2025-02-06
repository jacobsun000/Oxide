mod lexer;
mod parser;

use lexer::{Lexer, Token};

fn main() {
    let mut lexer = Lexer::new_from_file("hello_world.ox").unwrap();
    let tokens: Vec<Token> = std::iter::from_fn(|| {
        let tok = lexer.next_token();
        if tok == Token::Eof {
            None
        } else {
            Some(tok)
        }
    })
    .collect();
    println!("{:?}", tokens);
}
