use std::env;
use std::fs;
use std::io;

mod interpreter;
mod lexer;
mod parser;
mod repl;

fn start_repl() {
    let mut repl = repl::REPL::new();
    repl.start();
}

fn execute_code(code: &str) {
    let mut lexer = lexer::Lexer::new(code.to_string());
    let tokens: Vec<lexer::Token> = std::iter::from_fn(|| {
        let tok = lexer.next_token();
        if tok == lexer::Token::Eof {
            None
        } else {
            Some(tok)
        }
    })
    .collect();
    println!("\n\nTokens:\n{:?}", tokens);

    let mut parser = parser::Parser::new(tokens);
    let program = parser.parse_program().unwrap();
    println!("\n\nProgram:\n{:?}", program);

    println!("\n\nInterpreter:");
    let mut interpreter = interpreter::Interpreter::new();
    for expr in program {
        println!("{}", interpreter.eval(&expr).unwrap());
    }
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: oxide <filename> OR oxide -e \"code\"");
        return Ok(());
    }

    if args[1] == "-e" {
        // Execute code directly
        if args.len() < 3 {
            eprintln!("Usage: oxide -e \"code\"");
            return Ok(());
        }
        let code = &args[2];
        execute_code(code);
    } else {
        // Execute file
        let filename = &args[1];
        let code = fs::read_to_string(filename)?;
        execute_code(&code);
    }
    Ok(())
}
