mod lexer;
mod parser;

use lexer::{Lexer, Token};

fn main() {
    // let mut lexer = Lexer::new_from_file("hello_world.ox").unwrap();
    let case = "
fn greet(name) {
    return \"Hello, \" + name;
}
";
    let mut lexer = Lexer::new(case.to_string());
    let tokens: Vec<Token> = std::iter::from_fn(|| {
        let tok = lexer.next_token();
        if tok == Token::Eof {
            None
        } else {
            Some(tok)
        }
    })
    .collect();

    let mut parser = parser::Parser::new(tokens);
    let program = parser.parse_program();
    println!("{:?}", program);
}
