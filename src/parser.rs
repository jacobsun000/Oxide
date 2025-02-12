use crate::lexer::Token;
use std::iter::Peekable;

#[derive(Debug, Clone)]
pub enum Expr {
    Identifier(String),
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Block(Vec<Expr>),
    Function {
        params: Box<Expr>,
        body: Box<Expr>,
    },
    Return(Box<Expr>),
    If {
        cond: Box<Expr>,
        then_block: Box<Expr>,
        else_block: Option<Box<Expr>>,
    },
    While {
        cond: Box<Expr>,
        body: Box<Expr>,
    },
    Infix {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Prefix {
        op: Token,
        expr: Box<Expr>,
    },
    Postfix {
        expr: Box<Expr>,
        op: Token,
    },
    Null,
}

pub struct Parser {
    tokens: Peekable<std::vec::IntoIter<Token>>,
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedEof,
    UnexpectedToken(Token),
    InvalidToken(Token),
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
        }
    }

    pub fn parse_program(&mut self) -> Result<Vec<Expr>, ParseError> {
        let blocks = self.parse_block(Token::Eof, Token::SemiColon, false)?;
        match blocks {
            Expr::Block(blocks) => Ok(blocks),
            _ => panic!("Invalid program"),
        }
    }

    fn expect(&mut self, expected: Token) -> Result<(), ParseError> {
        match self.tokens.peek() {
            Some(token) if *token == expected => {
                self.tokens.next();
                Result::Ok(())
            }
            Some(token) => Result::Err(ParseError::UnexpectedToken(token.clone())),
            None => Result::Err(ParseError::UnexpectedEof),
        }
    }

    fn parse_identifier(&mut self) -> Result<String, ParseError> {
        match self.tokens.next() {
            Some(Token::Identifier(name)) => Result::Ok(name),
            Some(token) => Result::Err(ParseError::InvalidToken(token)),
            None => Result::Err(ParseError::UnexpectedEof),
        }
    }

    fn parse_block(&mut self, wrap: Token, delim: Token, closed: bool) -> Result<Expr, ParseError> {
        let mut elements = Vec::new();
        while let Some(token) = self.tokens.peek() {
            if *token == wrap {
                break;
            } else if *token == delim {
                self.tokens.next();
                continue;
            }
            elements.push(self.parse_expr(0)?);
        }
        if closed {
            self.expect(wrap)?;
        }
        Ok(Expr::Block(elements))
    }

    fn parse_function(&mut self) -> Result<Expr, ParseError> {
        let name = self.parse_identifier()?;
        self.expect(Token::LParen)?;
        let params = self.parse_block(Token::RParen, Token::Comma, true)?;
        self.expect(Token::LBraces)?;
        let body = self.parse_block(Token::RBraces, Token::SemiColon, true)?;

        Ok(Expr::Infix {
            left: Box::new(Expr::Identifier(name)),
            op: Token::Assign,
            right: Box::new(Expr::Function {
                params: Box::new(params),
                body: Box::new(body),
            }),
        })
    }

    fn parse_return(&mut self) -> Result<Expr, ParseError> {
        let expr = if let Some(token) = self.tokens.peek() {
            if *token == Token::SemiColon {
                Expr::Null
            } else {
                self.parse_expr(0)?
            }
        } else {
            Expr::Null
        };
        self.expect(Token::SemiColon)?;
        Ok(Expr::Return(Box::new(expr)))
    }

    fn parse_if(&mut self) -> Result<Expr, ParseError> {
        let condition = self.parse_expr(0)?;
        let then_block = Box::new(self.parse_block(Token::RBraces, Token::SemiColon, true)?);
        let else_block = if let Some(token) = self.tokens.peek() {
            if *token == Token::Else {
                self.tokens.next();
                Some(Box::new(self.parse_expr(0)?))
            } else {
                None
            }
        } else {
            None
        };

        Ok(Expr::If {
            cond: Box::new(condition),
            then_block,
            else_block,
        })
    }

    fn parse_while(&mut self) -> Result<Expr, ParseError> {
        let condition = Box::new(self.parse_expr(0)?);
        let body = Box::new(self.parse_block(Token::RBraces, Token::SemiColon, true)?);
        Ok(Expr::While {
            cond: condition,
            body,
        })
    }

    fn parse_expr(&mut self, precedence: u8) -> Result<Expr, ParseError> {
        let token = self.tokens.peek().ok_or(ParseError::UnexpectedEof)?;
        let mut left = match token {
            Token::Let => {
                self.tokens.next();
                self.parse_expr(0)?
            }
            Token::Fn => {
                self.tokens.next();
                self.parse_function()?
            }
            Token::Return => {
                self.tokens.next();
                self.parse_return()?
            }
            Token::LBraces => {
                self.tokens.next();
                self.parse_block(Token::RBraces, Token::SemiColon, true)?
            }
            Token::If => {
                self.tokens.next();
                self.parse_if()?
            }
            Token::While => {
                self.tokens.next();
                self.parse_while()?
            }
            token if token.is_prefix_operator() => self.parse_prefix(precedence)?,
            token if token.is_literal() => self.parse_primary()?,
            _ => {
                return Err(ParseError::InvalidToken(token.clone()));
            }
        };
        while let Some(operator) = self.match_operator() {
            let next_precedence = operator.precedence();
            if precedence > next_precedence {
                break;
            }
            if operator.is_postfix_operator() {
                left = Expr::Postfix {
                    expr: Box::new(left),
                    op: operator,
                };
                continue;
            }
            let right = self.parse_expr(next_precedence)?;
            left = Expr::Infix {
                left: Box::new(left),
                op: operator,
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn match_operator(&mut self) -> Option<Token> {
        let token = self.tokens.peek()?.clone();
        if token.is_operator() {
            self.tokens.next();
            Some(token)
        } else {
            None
        }
    }

    fn parse_prefix(&mut self, precedence: u8) -> Result<Expr, ParseError> {
        let token = self.tokens.next().ok_or(ParseError::UnexpectedEof)?;
        let next_precedence = token.precedence();
        if precedence < next_precedence {
            self.tokens.next();
            let operand = self.parse_expr(next_precedence)?;
            Ok(Expr::Prefix {
                op: token,
                expr: Box::new(operand),
            })
        } else {
            Err(ParseError::InvalidToken(token.clone()))
        }
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        let token = self.tokens.next().ok_or(ParseError::UnexpectedEof)?;
        match token {
            Token::Identifier(name) => Ok(Expr::Identifier(name)),
            Token::Number(n) => {
                if let Ok(int_val) = n.parse::<i64>() {
                    Ok(Expr::Integer(int_val))
                } else if let Ok(float_val) = n.parse::<f64>() {
                    Ok(Expr::Float(float_val))
                } else {
                    Err(ParseError::InvalidToken(Token::Number(n)))
                }
            }
            Token::String(s) => Ok(Expr::String(s)),
            _ => Err(ParseError::InvalidToken(token)),
        }
    }
}
