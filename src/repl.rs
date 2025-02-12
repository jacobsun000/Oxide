use std::io::{self, Write};

use crate::interpreter::Interpreter;
use crate::lexer::{Lexer, Token};
use crate::parser::Parser;

pub struct REPL {
    interpreter: Interpreter,
}

impl REPL {
    pub fn new() -> Self {
        Self {
            interpreter: Interpreter::new(),
        }
    }

    pub fn start(&mut self) {
        println!("Oxide - REPL Mode");
        println!("Type 'exit' to quit.");

        loop {
            print!("> ");
            io::stdout().flush().unwrap();

            let mut input = String::new();
            if io::stdin().read_line(&mut input).is_err() {
                println!("Error reading input");
                continue;
            }

            let input = input.trim();
            if input == "exit" {
                break;
            }

            if input.is_empty() {
                continue;
            }

            match self.evaluate_input(input) {
                Ok(result) => println!("{}", result),
                Err(e) => println!("Error: {}", e),
            }
        }
    }

    fn evaluate_input(&mut self, input: &str) -> Result<String, String> {
        let mut lexer = Lexer::new(input.to_string());
        let tokens: Vec<Token> = std::iter::from_fn(|| {
            let tok = lexer.next_token();
            if tok == Token::Eof {
                None
            } else {
                Some(tok)
            }
        })
        .collect();

        let mut parser = Parser::new(tokens);
        let program = parser.parse_program().map_err(|e| format!("{:?}", e))?;

        Ok(program
            .iter()
            .map(|expr| self.interpreter.eval(expr).unwrap().to_string())
            .collect::<Vec<_>>()
            .join("\n"))
    }
}
