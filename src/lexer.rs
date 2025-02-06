use std::fs;
use std::io::{self, Read};
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords
    Null,
    Let,
    If,
    Else,
    For,
    While,
    Fn,
    Return,
    Class,

    // Identifiers and Literals
    Identifier(String),
    Number(String),
    String(String),

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    PlusPlus,
    MinusMinus,
    Eq,
    EqEq,
    NotEq,
    Greater,
    Less,
    GreaterEq,
    LessEq,
    Not,
    AndAnd,
    OrOr,
    Question,
    Colon,
    Comma,
    Ampersand,
    Pipe,
    Caret,
    Tilde,
    ShiftLeft,
    ShiftRight,
    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,
    PercentEq,
    AndAndEq,
    OrOrEq,
    AmpEq,
    PipeEq,
    CaretEq,
    TildeEq,
    ShiftLeftEq,
    ShiftRightEq,

    // Access Operators
    DoubleColon,
    Dot,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBraces,
    RBraces,

    // Misc
    Unknown(char),
    Eof,
}

pub struct Lexer {
    input: Peekable<Chars<'static>>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let input = Box::leak(input.into_boxed_str());
        Self {
            input: input.chars().peekable(),
        }
    }

    pub fn new_from_file(filename: &str) -> io::Result<Self> {
        let mut file = fs::File::open(filename)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        Ok(Self::new(contents))
    }

    pub fn next_token(&mut self) -> Token {
        while let Some(&ch) = self.input.peek() {
            match ch {
                ' ' | '\t' | '\n' | '\r' | ';' => {
                    self.input.next();
                } // Skip whitespace
                '#' => {
                    self.consume_line();
                } // Skip comments
                '0'..='9' => return self.consume_number(),
                '"' => return self.consume_string(),
                'a'..='z' | 'A'..='Z' | '_' => return self.consume_identifier_or_keyword(),
                '+' | '-' | '*' | '/' | '%' | '=' | '!' | '>' | '<' | '&' | '|' | '^' | '~'
                | '?' | ':' | ',' => {
                    return self.consume_operator();
                }
                '.' => {
                    self.input.next();
                    return Token::Dot;
                }
                '(' => {
                    self.input.next();
                    return Token::LParen;
                }
                ')' => {
                    self.input.next();
                    return Token::RParen;
                }
                '[' => {
                    self.input.next();
                    return Token::LBracket;
                }
                ']' => {
                    self.input.next();
                    return Token::RBracket;
                }
                '{' => {
                    self.input.next();
                    return Token::LBraces;
                }
                '}' => {
                    self.input.next();
                    return Token::RBraces;
                }
                _ => {
                    self.input.next();
                    return Token::Unknown(ch);
                }
            }
        }
        Token::Eof
    }

    fn consume_line(&mut self) {
        while let Some(&ch) = self.input.peek() {
            if ch != '\n' {
                self.input.next();
            } else {
                break;
            }
        }
    }

    fn consume_number(&mut self) -> Token {
        let mut num = String::new();
        while let Some(&ch) = self.input.peek() {
            if ch.is_digit(10) || ch == '.' {
                num.push(ch);
                self.input.next();
            } else {
                break;
            }
        }
        Token::Number(num)
    }

    fn consume_string(&mut self) -> Token {
        self.input.next(); // Consume opening quote
        let mut string_content = String::new();
        let mut escape = false;

        while let Some(&ch) = self.input.peek() {
            self.input.next(); // Consume the character

            if escape {
                match ch {
                    'n' => string_content.push('\n'),
                    't' => string_content.push('\t'),
                    'r' => string_content.push('\r'),
                    '"' => string_content.push('"'),
                    '\\' => string_content.push('\\'),
                    _ => string_content.push(ch), // Unknown escape sequences are treated as literals
                }
                escape = false;
            } else if ch == '\\' {
                escape = true; // Next character should be treated as escaped
            } else if ch == '"' {
                return Token::String(string_content); // Closing quote found
            } else {
                string_content.push(ch);
            }
        }

        // If we reach here, the string was not properly closed
        Token::String(string_content) // Return what we have, treating it as an unclosed string
    }

    fn consume_identifier_or_keyword(&mut self) -> Token {
        let mut ident = String::new();
        while let Some(&ch) = self.input.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                ident.push(ch);
                self.input.next();
            } else {
                break;
            }
        }
        match ident.as_str() {
            "let" => Token::Let,
            "if" => Token::If,
            "else" => Token::Else,
            "for" => Token::For,
            "while" => Token::While,
            "fn" => Token::Fn,
            "return" => Token::Return,
            "class" => Token::Class,
            _ => Token::Identifier(ident),
        }
    }

    fn consume_operator(&mut self) -> Token {
        let mut op = String::new();
        if let Some(&start) = self.input.peek() {
            op.push(start);
            self.input.next();

            while let Some(&ch) = self.input.peek() {
                if ch == start || ch == '=' {
                    op.push(ch);
                    self.input.next();
                } else {
                    break;
                }
            }
        }
        match op.as_str() {
            "+" => Token::Plus,
            "-" => Token::Minus,
            "*" => Token::Star,
            "/" => Token::Slash,
            "%" => Token::Percent,
            "++" => Token::PlusPlus,
            "--" => Token::MinusMinus,
            "=" => Token::Eq,
            "==" => Token::EqEq,
            "!=" => Token::NotEq,
            ">" => Token::Greater,
            "<" => Token::Less,
            ">=" => Token::GreaterEq,
            "<=" => Token::LessEq,
            "!" => Token::Not,
            "&&" => Token::AndAnd,
            "||" => Token::OrOr,
            "?" => Token::Question,
            ":" => Token::Colon,
            "::" => Token::DoubleColon,
            "," => Token::Comma,
            "&" => Token::Ampersand,
            "|" => Token::Pipe,
            "^" => Token::Caret,
            "~" => Token::Tilde,
            "<<" => Token::ShiftLeft,
            ">>" => Token::ShiftRight,
            "+=" => Token::PlusEq,
            "-=" => Token::MinusEq,
            "*=" => Token::StarEq,
            "/=" => Token::SlashEq,
            "%=" => Token::PercentEq,
            "&&=" => Token::AndAndEq,
            "||=" => Token::OrOrEq,
            "&=" => Token::AmpEq,
            "|=" => Token::PipeEq,
            "^=" => Token::CaretEq,
            "~=" => Token::TildeEq,
            "<<=" => Token::ShiftLeftEq,
            ">>=" => Token::ShiftRightEq,
            _ => Token::Identifier(op),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(input: &str) -> Vec<Token> {
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

        tokens
    }

    #[test]
    fn test_keywords() {
        let tokens = lex("let if else for while fn return class");
        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::If,
                Token::Else,
                Token::For,
                Token::While,
                Token::Fn,
                Token::Return,
                Token::Class,
            ]
        );
    }

    #[test]
    fn test_identifiers() {
        let tokens = lex("var1 foo_bar abc123");
        assert_eq!(
            tokens,
            vec![
                Token::Identifier("var1".to_string()),
                Token::Identifier("foo_bar".to_string()),
                Token::Identifier("abc123".to_string()),
            ]
        );
    }

    #[test]
    fn test_numbers() {
        let tokens = lex("123 456.78 0.99");
        assert_eq!(
            tokens,
            vec![
                Token::Number("123".to_string()),
                Token::Number("456.78".to_string()),
                Token::Number("0.99".to_string()),
            ]
        );
    }

    #[test]
    fn test_basic_operators() {
        let tokens = lex("+ - * / %");
        assert_eq!(
            tokens,
            vec![
                Token::Plus,
                Token::Minus,
                Token::Star,
                Token::Slash,
                Token::Percent,
            ]
        );
    }

    #[test]
    fn test_increment_decrement() {
        let tokens = lex("++ --");
        assert_eq!(tokens, vec![Token::PlusPlus, Token::MinusMinus]);
    }

    #[test]
    fn test_comparison_operators() {
        let tokens = lex("== != > < >= <=");
        assert_eq!(
            tokens,
            vec![
                Token::EqEq,
                Token::NotEq,
                Token::Greater,
                Token::Less,
                Token::GreaterEq,
                Token::LessEq,
            ]
        );
    }

    #[test]
    fn test_logical_operators() {
        let tokens = lex("&& || !");
        assert_eq!(tokens, vec![Token::AndAnd, Token::OrOr, Token::Not]);
    }

    #[test]
    fn test_bitwise_operators() {
        let tokens = lex("& | ^ ~");
        assert_eq!(
            tokens,
            vec![Token::Ampersand, Token::Pipe, Token::Caret, Token::Tilde,]
        );
    }

    #[test]
    fn test_shift_operators() {
        let tokens = lex("<< >>");
        assert_eq!(tokens, vec![Token::ShiftLeft, Token::ShiftRight]);
    }

    #[test]
    fn test_assignment_operators() {
        let tokens = lex("+= -= *= /= %= &&= ||= &= |= ^= ~= <<= >>=");
        assert_eq!(
            tokens,
            vec![
                Token::PlusEq,
                Token::MinusEq,
                Token::StarEq,
                Token::SlashEq,
                Token::PercentEq,
                Token::AndAndEq,
                Token::OrOrEq,
                Token::AmpEq,
                Token::PipeEq,
                Token::CaretEq,
                Token::TildeEq,
                Token::ShiftLeftEq,
                Token::ShiftRightEq,
            ]
        );
    }

    #[test]
    fn test_punctuation() {
        let tokens = lex(": , ; .");
        assert_eq!(tokens, vec![Token::Colon, Token::Comma, Token::Dot]);
    }

    #[test]
    fn test_grouping_operators() {
        let tokens = lex("() [] {}");
        assert_eq!(
            tokens,
            vec![
                Token::LParen,
                Token::RParen,
                Token::LBracket,
                Token::RBracket,
                Token::LBraces,
                Token::RBraces,
            ]
        );
    }

    #[test]
    fn test_double_colon() {
        let tokens = lex("::");
        assert_eq!(tokens, vec![Token::DoubleColon]);
    }

    #[test]
    fn test_mixed_expressions() {
        let tokens = lex("let x = 5 + 3;");
        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::Identifier("x".to_string()),
                Token::Eq,
                Token::Number("5".to_string()),
                Token::Plus,
                Token::Number("3".to_string()),
            ]
        );
    }

    #[test]
    fn test_function_declaration() {
        let tokens = lex("fn add(a, b) { return a + b; }");
        assert_eq!(
            tokens,
            vec![
                Token::Fn,
                Token::Identifier("add".to_string()),
                Token::LParen,
                Token::Identifier("a".to_string()),
                Token::Comma,
                Token::Identifier("b".to_string()),
                Token::RParen,
                Token::LBraces,
                Token::Return,
                Token::Identifier("a".to_string()),
                Token::Plus,
                Token::Identifier("b".to_string()),
                Token::RBraces,
            ]
        );
    }

    #[test]
    fn test_class_definition() {
        let tokens = lex("class Dog { fn bark() { return \"woof\"; } }");
        assert_eq!(
            tokens,
            vec![
                Token::Class,
                Token::Identifier("Dog".to_string()),
                Token::LBraces,
                Token::Fn,
                Token::Identifier("bark".to_string()),
                Token::LParen,
                Token::RParen,
                Token::LBraces,
                Token::Return,
                Token::String("woof".to_string()),
                Token::RBraces,
                Token::RBraces,
            ]
        );
    }

    #[test]
    fn test_if_else_statement() {
        let tokens = lex("if x > 10 { return x; } else { return 0; }");
        assert_eq!(
            tokens,
            vec![
                Token::If,
                Token::Identifier("x".to_string()),
                Token::Greater,
                Token::Number("10".to_string()),
                Token::LBraces,
                Token::Return,
                Token::Identifier("x".to_string()),
                Token::RBraces,
                Token::Else,
                Token::LBraces,
                Token::Return,
                Token::Number("0".to_string()),
                Token::RBraces,
            ]
        );
    }

    #[test]
    fn test_for_loop() {
        let tokens = lex("for i = 0; i < 10; i++ {}");
        assert_eq!(
            tokens,
            vec![
                Token::For,
                Token::Identifier("i".to_string()),
                Token::Eq,
                Token::Number("0".to_string()),
                Token::Identifier("i".to_string()),
                Token::Less,
                Token::Number("10".to_string()),
                Token::Identifier("i".to_string()),
                Token::PlusPlus,
                Token::LBraces,
                Token::RBraces,
            ]
        );
    }

    #[test]
    fn test_while_loop() {
        let tokens = lex("while x > 0 { x--; }");
        assert_eq!(
            tokens,
            vec![
                Token::While,
                Token::Identifier("x".to_string()),
                Token::Greater,
                Token::Number("0".to_string()),
                Token::LBraces,
                Token::Identifier("x".to_string()),
                Token::MinusMinus,
                Token::RBraces,
            ]
        );
    }
}
