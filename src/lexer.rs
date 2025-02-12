use std::fs;
use std::io::{self, Read};
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords
    Let,
    If,
    Else,
    For,
    While,
    Continue,
    Break,
    Fn,
    Return,
    Class,

    // Identifiers and Literals
    Identifier(String),
    Number(String),
    String(String),

    // Operators
    Add,      // +
    Sub,      // -
    Mul,      // *
    Div,      // /
    Mod,      // %
    Pow,      // **
    Inc,      // ++
    Dec,      // --
    Assign,   // =
    Eq,       // =
    Ne,       // !=
    Gt,       // >
    Lt,       // <
    GE,       // >=
    LE,       // <=
    Not,      // !
    And,      // &&
    Or,       // ||
    Question, // ?
    Colon,    // :
    Comma,    // ,
    BitAnd,   // &
    BitOr,    // |
    BitXor,   // ^
    BitNot,   // ~
    LShift,   // <<
    RShift,   // >>
    AddEq,    // +=
    SubEq,    // -=
    MulEq,    // *=
    DivEq,    // /=
    ModEq,    // %=
    AndEq,    // &&=
    OrEq,     // ||=
    BitAndEq, // &=
    BitOrEq,  // |=
    BitXorEq, // ^=
    BitNotEq, // ~=
    LShiftEq, // <<=
    RShiftEq, // >>=

    // Access Operators
    Scope,    // ::
    Access,   // .
    LParen,   // (
    RParen,   // )
    LBracket, // [
    RBracket, // ]
    LBraces,  // {
    RBraces,  // }

    // Misc
    SemiColon, // ;
    Unknown(char),
    Eof,
}

impl Token {
    pub fn is_literal(&self) -> bool {
        use Token::*;
        match self {
            Identifier(_) | Number(_) | String(_) => true,
            _ => false,
        }
    }

    pub fn is_prefix_operator(&self) -> bool {
        use Token::*;
        match self {
            Not | BitNot | Sub | Add => true,
            _ => false,
        }
    }

    pub fn is_postfix_operator(&self) -> bool {
        use Token::*;
        match self {
            Inc | Dec => true,
            _ => false,
        }
    }

    pub fn is_infix_operator(&self) -> bool {
        use Token::*;
        match self {
            Add | Sub | Mul | Div | Mod | Pow | Assign | Eq | Ne | Gt | Lt | GE | LE | And | Or
            | Question | BitAnd | BitOr | BitXor | LShift | RShift | AddEq | SubEq | MulEq
            | DivEq | ModEq | AndEq | OrEq | BitAndEq | BitOrEq | BitXorEq | BitNotEq
            | LShiftEq | RShiftEq | Scope | Access | LParen | LBracket | LBraces => true,
            _ => false,
        }
    }

    pub fn is_operator(&self) -> bool {
        use Token::*;
        match self {
            Add | Sub | Mul | Div | Mod | Pow | Inc | Dec | Assign | Eq | Ne | Gt | Lt | GE
            | LE | Not | And | Or | Question | BitAnd | BitOr | BitXor | BitNot | LShift
            | RShift | AddEq | SubEq | MulEq | DivEq | ModEq | AndEq | OrEq | BitAndEq
            | BitOrEq | BitXorEq | BitNotEq | LShiftEq | RShiftEq | Scope | Access | LParen
            | LBracket | LBraces => true,
            _ => false,
        }
    }

    pub fn precedence(&self) -> u8 {
        use Token::*;
        match self {
            // Highest precedence (Grouping & Function Calls)
            LParen | LBracket | Access | Scope => 15,

            // Unary Operators
            Inc | Dec | Not | BitNot => 14,

            // Exponentiation
            Pow => 13,

            // Multiplication, Division, Modulus
            Mul | Div | Mod => 12,

            // Addition and Subtraction
            Add | Sub => 11,

            // Bitwise Shifts
            LShift | RShift => 10,

            // Comparison Operators
            Gt | Lt | GE | LE => 9,

            // Equality Operators
            Eq | Ne => 8,

            // Bitwise AND
            BitAnd => 7,

            // Bitwise XOR
            BitXor => 6,

            // Bitwise OR
            BitOr => 5,

            // Logical AND
            And => 4,

            // Logical OR
            Or => 3,

            // Ternary Operator
            Question => 2,

            // Assignment Operators (Lowest precedence for right-to-left associativity)
            Assign | AddEq | SubEq | MulEq | DivEq | ModEq | AndEq | OrEq | BitAndEq | BitOrEq
            | BitXorEq | BitNotEq | LShiftEq | RShiftEq => 1,

            // Default (Unknown Tokens or No Precedence)
            _ => 0,
        }
    }

    pub fn op_code(&self) -> Option<&'static str> {
        use Token::*;
        match self {
            Add => Some("__add__"),
            Sub => Some("__sub__"),
            Mul => Some("__mul__"),
            Div => Some("__div__"),
            Mod => Some("__mod__"),
            Pow => Some("__pow__"),
            Inc => Some("__inc__"),
            Dec => Some("__dec__"),
            Assign => Some("__assign__"),
            Eq => Some("__eq__"),
            Ne => Some("__ne__"),
            Gt => Some("__gt__"),
            Lt => Some("__lt__"),
            GE => Some("__ge__"),
            LE => Some("__le__"),
            Not => Some("__not__"),
            And => Some("__and__"),
            Or => Some("__or__"),
            Question => Some("__question__"),
            Colon => Some("__colon__"),
            Comma => Some("__comma__"),
            BitAnd => Some("__bitand__"),
            BitOr => Some("__bitor__"),
            BitXor => Some("__bitxor__"),
            BitNot => Some("__bitnot__"),
            LShift => Some("__lshift__"),
            RShift => Some("__rshift__"),
            AddEq => Some("__addeq__"),
            SubEq => Some("__subeq__"),
            MulEq => Some("__muleq__"),
            DivEq => Some("__diveq__"),
            ModEq => Some("__modeq__"),
            AndEq => Some("__andeq__"),
            OrEq => Some("__oreq__"),
            BitAndEq => Some("__bitandeq__"),
            BitOrEq => Some("__bitoreq__"),
            BitXorEq => Some("__bitxoreq__"),
            BitNotEq => Some("__bitnoteq__"),
            LShiftEq => Some("__lshifteq__"),
            RShiftEq => Some("__rshifteq__"),
            Scope => Some("__scope__"),
            Access => Some("__access__"),
            LParen => Some("__lparen__"),
            LBracket => Some("__lbracket__"),
            LBraces => Some("__lbraces__"),
            _ => None,
        }
    }
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
                ' ' | '\t' | '\r' | '\n' => {
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
                ';' => {
                    self.input.next();
                    return Token::SemiColon;
                }
                '.' => {
                    self.input.next();
                    return Token::Access;
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
            "contirue" => Token::Continue,
            "break" => Token::Break,
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
            "+" => Token::Add,
            "-" => Token::Sub,
            "*" => Token::Mul,
            "/" => Token::Div,
            "%" => Token::Mod,
            "**" => Token::Pow,
            "++" => Token::Inc,
            "--" => Token::Dec,
            "=" => Token::Assign,
            "==" => Token::Eq,
            "!=" => Token::Ne,
            ">" => Token::Gt,
            "<" => Token::Lt,
            ">=" => Token::GE,
            "<=" => Token::LE,
            "!" => Token::Not,
            "&&" => Token::And,
            "||" => Token::Or,
            "?" => Token::Question,
            ":" => Token::Colon,
            "::" => Token::Scope,
            "," => Token::Comma,
            "&" => Token::BitAnd,
            "|" => Token::BitOr,
            "^" => Token::BitXor,
            "~" => Token::BitNot,
            "<<" => Token::LShift,
            ">>" => Token::RShift,
            "+=" => Token::AddEq,
            "-=" => Token::SubEq,
            "*=" => Token::MulEq,
            "/=" => Token::DivEq,
            "%=" => Token::ModEq,
            "&&=" => Token::AndEq,
            "||=" => Token::OrEq,
            "&=" => Token::BitAndEq,
            "|=" => Token::BitOrEq,
            "^=" => Token::BitXorEq,
            "~=" => Token::BitNotEq,
            "<<=" => Token::LShiftEq,
            ">>=" => Token::RShiftEq,
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
            vec![Token::Add, Token::Sub, Token::Mul, Token::Div, Token::Mod,]
        );
    }

    #[test]
    fn test_increment_decrement() {
        let tokens = lex("++ --");
        assert_eq!(tokens, vec![Token::Inc, Token::Dec]);
    }

    #[test]
    fn test_comparison_operators() {
        let tokens = lex("== != > < >= <=");
        assert_eq!(
            tokens,
            vec![
                Token::Eq,
                Token::Ne,
                Token::Gt,
                Token::LE,
                Token::GE,
                Token::LE,
            ]
        );
    }

    #[test]
    fn test_logical_operators() {
        let tokens = lex("&& || !");
        assert_eq!(tokens, vec![Token::And, Token::Or, Token::Not]);
    }

    #[test]
    fn test_bitwise_operators() {
        let tokens = lex("& | ^ ~");
        assert_eq!(
            tokens,
            vec![Token::BitAnd, Token::BitOr, Token::BitXor, Token::BitNot,]
        );
    }

    #[test]
    fn test_shift_operators() {
        let tokens = lex("<< >>");
        assert_eq!(tokens, vec![Token::LShift, Token::RShift]);
    }

    #[test]
    fn test_assignment_operators() {
        let tokens = lex("+= -= *= /= %= &&= ||= &= |= ^= ~= <<= >>=");
        assert_eq!(
            tokens,
            vec![
                Token::AddEq,
                Token::SubEq,
                Token::MulEq,
                Token::DivEq,
                Token::ModEq,
                Token::AndEq,
                Token::OrEq,
                Token::BitAndEq,
                Token::BitOrEq,
                Token::BitXorEq,
                Token::BitNotEq,
                Token::LShiftEq,
                Token::RShiftEq,
            ]
        );
    }

    #[test]
    fn test_punctuation() {
        let tokens = lex(": , ; .");
        assert_eq!(
            tokens,
            vec![Token::Colon, Token::Comma, Token::SemiColon, Token::Access]
        );
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
        assert_eq!(tokens, vec![Token::Scope]);
    }

    #[test]
    fn test_mixed_expressions() {
        let tokens = lex("let x = 5 + 3;");
        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::Identifier("x".to_string()),
                Token::Assign,
                Token::Number("5".to_string()),
                Token::Add,
                Token::Number("3".to_string()),
                Token::SemiColon,
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
                Token::Add,
                Token::Identifier("b".to_string()),
                Token::SemiColon,
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
                Token::SemiColon,
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
                Token::Gt,
                Token::Number("10".to_string()),
                Token::LBraces,
                Token::Return,
                Token::Identifier("x".to_string()),
                Token::SemiColon,
                Token::RBraces,
                Token::Else,
                Token::LBraces,
                Token::Return,
                Token::Number("0".to_string()),
                Token::SemiColon,
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
                Token::Assign,
                Token::Number("0".to_string()),
                Token::SemiColon,
                Token::Identifier("i".to_string()),
                Token::LE,
                Token::Number("10".to_string()),
                Token::SemiColon,
                Token::Identifier("i".to_string()),
                Token::Inc,
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
                Token::Gt,
                Token::Number("0".to_string()),
                Token::LBraces,
                Token::Identifier("x".to_string()),
                Token::Dec,
                Token::SemiColon,
                Token::RBraces,
            ]
        );
    }
}
