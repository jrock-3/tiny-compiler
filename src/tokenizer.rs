use std::io::BufRead;

use crate::{
    input::Input,
    tokenizer_data::{RelOp, Token},
};

// Characters
const ASCII_ZERO: u8 = '0' as u8;
const ASCII_NINE: u8 = '9' as u8;
const ASCII_LOWER_A: u8 = 'a' as u8;
const ASCII_LOWER_Z: u8 = 'z' as u8;
const ASCII_UPPER_A: u8 = 'A' as u8;
const ASCII_UPPER_Z: u8 = 'Z' as u8;

// Symbols
const ASCII_COMMA: u8 = ',' as u8;
const ASCII_SEMICOLON: u8 = ';' as u8;
const ASCII_PERIOD: u8 = '.' as u8;

// Operators
const ASCII_PLUS: u8 = '+' as u8;
const ASCII_MINUS: u8 = '-' as u8;
const ASCII_ASTERISK: u8 = '*' as u8;
const ASCII_FORWARD_SLASH: u8 = '/' as u8;
const ASCII_EQUAL: u8 = '=' as u8;
const ASCII_BANG: u8 = '!' as u8;
const ASCII_LEFT_ARROW: u8 = '<' as u8;
const ASCII_RIGHT_ARROW: u8 = '>' as u8;

// Delimiters
const ASCII_SPACE: u8 = ' ' as u8;
const ASCII_NEWLINE: u8 = '\n' as u8;
const ASCII_OPEN_PAREN: u8 = '(' as u8;
const ASCII_CLOSE_PAREN: u8 = ')' as u8;
const ASCII_OPEN_BRACE: u8 = '{' as u8;
const ASCII_CLOSE_BRACE: u8 = '}' as u8;

#[derive(Debug, PartialEq)]
pub struct Tokenizer<R: BufRead> {
    input: Input<R>,
    curr: Option<Token>,
    idents: Vec<String>,
}

impl<R: BufRead> Tokenizer<R> {
    pub fn new(input: R) -> Tokenizer<R> {
        let mut res = Tokenizer {
            input: Input::new(input),
            curr: None,
            idents: vec![
                "InputNum".to_string(),
                "OutputNum".to_string(),
                "OutputNewLine".to_string(),
            ],
        };
        res.next();
        res
    }

    pub fn peek(&mut self) -> Option<Token> {
        self.curr
    }

    pub fn get_var(&self, var: usize) -> String {
        self.idents
            .get(var)
            .expect("Ident should exist")
            .to_string()
    }

    pub fn next(&mut self) -> Option<Token> {
        let old = self.curr;

        if let Some(token) = self.input.peek() {
            match token {
                ASCII_SPACE | ASCII_NEWLINE => {
                    self.input.next();
                    self.next();
                }

                // Operators
                ASCII_PLUS => {
                    self.curr = Some(Token::Plus);
                    self.input.next();
                }
                ASCII_MINUS => {
                    self.curr = Some(Token::Minus);
                    self.input.next();
                }
                ASCII_ASTERISK => {
                    self.curr = Some(Token::Times);
                    self.input.next();
                }
                ASCII_FORWARD_SLASH => {
                    self.curr = Some(Token::Divide);
                    self.input.next();
                }
                ASCII_EQUAL => {
                    self.input.next();
                    self.curr = if let Some(ASCII_EQUAL) = self.input.peek() {
                        self.input.next();
                        Some(Token::RelOp(RelOp::Equal))
                    } else {
                        None
                    }
                }
                ASCII_BANG => {
                    self.input.next();
                    self.curr = if let Some(ASCII_EQUAL) = self.input.peek() {
                        self.input.next();
                        Some(Token::RelOp(RelOp::NotEqual))
                    } else {
                        None
                    }
                }
                ASCII_LEFT_ARROW => {
                    self.input.next();
                    self.curr = if let Some(token) = self.input.peek() {
                        match token {
                            ASCII_MINUS => {
                                self.input.next();
                                Some(Token::Assignment)
                            }
                            ASCII_EQUAL => {
                                self.input.next();
                                Some(Token::RelOp(RelOp::LessThanOrEqual))
                            }
                            _ => Some(Token::RelOp(RelOp::LessThan)),
                        }
                    } else {
                        None
                    }
                }
                ASCII_RIGHT_ARROW => {
                    self.input.next();
                    self.curr = if let Some(token) = self.input.peek() {
                        match token {
                            ASCII_EQUAL => {
                                self.input.next();
                                Some(Token::RelOp(RelOp::GreaterThanOrEqual))
                            }
                            _ => Some(Token::RelOp(RelOp::GreaterThan)),
                        }
                    } else {
                        None
                    }
                }

                // Delimiters
                ASCII_OPEN_PAREN => {
                    self.curr = Some(Token::OpenParen);
                    self.input.next();
                }
                ASCII_CLOSE_PAREN => {
                    self.curr = Some(Token::CloseParen);
                    self.input.next();
                }
                ASCII_OPEN_BRACE => {
                    self.curr = Some(Token::OpenBrace);
                    self.input.next();
                }
                ASCII_CLOSE_BRACE => {
                    self.curr = Some(Token::CloseBrace);
                    self.input.next();
                }

                // Multiline Tokens
                letter @ (ASCII_UPPER_A..=ASCII_UPPER_Z | ASCII_LOWER_A..=ASCII_LOWER_Z) => {
                    self.input.next();
                    let mut res = String::new();
                    res.push(letter as char);

                    loop {
                        if let letter @ Some(
                            ASCII_UPPER_A..=ASCII_UPPER_Z
                            | ASCII_LOWER_A..=ASCII_LOWER_Z
                            | ASCII_ZERO..=ASCII_NINE,
                        ) = self.input.peek()
                        {
                            self.input.next();
                            res.push(letter.expect("Should not fail") as char);
                        } else {
                            break;
                        }
                    }

                    self.curr = Some(match res.as_str() {
                        // Keywords
                        "main" => Token::Main,
                        "var" => Token::Var,
                        "let" => Token::Let,
                        "call" => Token::Call,
                        "if" => Token::If,
                        "fi" => Token::Fi,
                        "then" => Token::Then,
                        "else" => Token::Else,
                        "while" => Token::While,
                        "do" => Token::Do,
                        "od" => Token::Od,
                        "void" => Token::Void,
                        "function" => Token::Function,
                        "return" => Token::Return,
                        identifier => Token::Ident(
                            match self.idents.iter().position(|e| e == identifier) {
                                Some(id) => id,
                                None => {
                                    let res = self.idents.len();
                                    self.idents.push(identifier.to_string());

                                    res
                                }
                            }
                            .try_into()
                            .expect("Should not overflow"),
                        ),
                    });
                }
                num @ (ASCII_ZERO..=ASCII_NINE) => {
                    self.input.next();
                    let mut res = (num - ASCII_ZERO) as isize;

                    loop {
                        match self.input.peek() {
                            num @ Some(ASCII_ZERO..=ASCII_NINE) => {
                                self.input.next();
                                res = res * 10
                                    + (num.expect("Should not fail") - ASCII_ZERO) as isize;
                            }
                            _ => break,
                        }
                    }

                    self.curr = Some(Token::Number(res));
                }

                // Symbols
                ASCII_COMMA => {
                    self.curr = Some(Token::Comma);
                    self.input.next();
                }
                ASCII_PERIOD => {
                    self.curr = Some(Token::Period);
                    self.input.next();
                }
                ASCII_SEMICOLON => {
                    self.curr = Some(Token::Semicolon);
                    self.input.next();
                }
                _ => {
                    self.curr = None;
                }
            }
        } else {
            self.curr = None;
        }

        old
    }
}
