#![allow(warnings)]

use std::any::Any;
use std::iter::Peekable;
use std::num::ParseIntError;
use std::process::exit;
use std::str::Chars;

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum Symbol {
  LBracket, // (
  RBracket, // )
  LBrace, // {
  RBrace, // }
  Semicolon, // ;
  Colon, // :
  Dot, // .
  Comma, // ,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Operator {
  Plus, // +
  Star, // *
  Minus, // -
  BitwiseComplement, // ~
  Slash, // /
  Modulo, // %
  PlusAssign, // +=
  MinusAssign, // -=
  SlashAssign, // /=
  StarAssign, // *=
  ModAssign, // %=
  LeftShiftAssign, // <<=
  RightShiftAssign, // >>=
  ANDAssign, // &=
  ORAssign, // |=
  XORAssign, // ^=
  And, // &&
  Or, // ||
  EqualEqual, // ==
  NotEqual, // !=
  Assignment, // =
  LessThan, // <
  LessEqual, // <=
  GreaterThan, // >
  GreaterEqual, // >=
  BitwiseAND, // &
  BitwiseOR, // |
  BitwiseXOR, // ^
  BitwiseShiftLeft, // <<
  BitwiseShiftRight, // >>
  QuestionMark, // ?
  LogicalNegation,   // !
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Keyword {
  Int, Return, If, Else, For, Do, While, Break, Continue, String, Struct,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
  Symbol(Symbol),
  Operator(Operator),
  Integer(i32),
  Keyword(Keyword),
  String(String),
}

#[derive(Debug)]
pub struct Tokenizer<'a> {
  code: &'a str,
  line: usize,
  pos: usize,

  chars: Peekable<Chars<'a>>,

  // number of iteration before
  iteration: usize,
}

#[derive(Debug)]
pub struct TokenData<'a> {
  token: Token,
  str: &'a str,
  line: usize,
  pos: usize,
}

impl<'a> Tokenizer<'a> {
  fn new(code: &'a str) -> Tokenizer<'a> {
    Tokenizer {
      code,
      line: 0,
      pos: 0,
      chars: code.chars().peekable(),
      iteration: 0,
    }
  }

  fn next_char(&mut self) -> Option<char> {
    match self.chars.next() {
      None => None,
      Some(ch) => {
        if ch == '\n' {
          self.line += 1;
          self.pos = 0;
        } else {
          self.pos += 1;
        }
        Some(ch)
      }
    }
  }
}

fn work(token: Token, str: &str, line: usize, pos: usize) -> Option<TokenData> {
  Some(TokenData { token, str, line, pos })
}

impl<'a> Iterator for Tokenizer<'a> {
  type Item = TokenData<'a>;
  fn next(&mut self) -> Option<Self::Item> {
    loop {
      return match self.next_char() {
        None => None,
        Some(ch) => {
          match ch {
            // todo()! delete repeated code
            '(' => work(Token::Symbol(Symbol::LBracket), ")", self.line, self.pos),
            ')' => work(Token::Symbol(Symbol::RBracket), "(", self.line, self.pos),
            '{' => work(Token::Symbol(Symbol::LBrace), "{", self.line, self.pos),
            '}' => work(Token::Symbol(Symbol::RBrace), "}", self.line, self.pos),
            ';' => work(Token::Symbol(Symbol::Semicolon), ";", self.line, self.pos),
            ':' => work(Token::Symbol(Symbol::Semicolon), ":", self.line, self.pos),
            '.' => work(Token::Symbol(Symbol::Dot), ".", self.line, self.pos),
            ',' => work(Token::Symbol(Symbol::Comma), ",", self.line, self.pos),

            '+' => match self.chars.peek() {
              Some(&'=') => {
                self.next_char();
                work(Token::Operator(Operator::PlusAssign), "+=", self.line, self.pos - 1)
              }
              _ => work(Token::Operator(Operator::Plus), "+", self.line, self.pos),
            },

            '-' => match self.chars.peek() {
              Some(&'=') => {
                self.next_char();
                work(Token::Operator(Operator::PlusAssign), "-=", self.line, self.pos - 1)
              }
              _ => work(Token::Operator(Operator::Minus), "-", self.line, self.pos),
            },

            '*' => match self.chars.peek() {
              Some(&'=') => {
                self.next_char();
                work(Token::Operator(Operator::StarAssign), "*=", self.line, self.pos - 1)
              }
              _ => work(Token::Operator(Operator::Star), "*", self.line, self.pos),
            },

            '~' => work(Token::Operator(Operator::BitwiseComplement), "~", self.line, self.pos),

            '/' => match self.chars.peek() {
              Some(&'=') => {
                self.next_char();
                work(Token::Operator(Operator::SlashAssign), "/=", self.line, self.pos - 1)
              }
              _ => work(Token::Operator(Operator::Slash), "/", self.line, self.pos),
            },

            '%' => match self.chars.peek() {
              Some(&'=') => {
                self.next_char();
                work(Token::Operator(Operator::ModAssign), "%=", self.line, self.pos - 1)
              }
              _ => work(Token::Operator(Operator::Modulo), "%", self.line, self.pos),
            },

            '<' => match self.chars.peek() { // Noob code, but who cares : )
              Some(&'<') => {
                self.next_char();
                match self.chars.peek() {
                  Some(&'=') => {
                    self.next_char();
                    work(Token::Operator(Operator::LeftShiftAssign), "<<=", self.line, self.pos - 2)
                  }
                  _ => work(Token::Operator(Operator::LessEqual), "<<", self.line, self.pos - 1),
                }
              },
              Some(&'=') => {
                self.next_char();
                work(Token::Operator(Operator::LessEqual), "<=", self.line, self.pos - 1)
              },
              _ => work(Token::Operator(Operator::LessThan), "<", self.line, self.pos),
            },

            '>' => match self.chars.peek() { // Noob code, but who cares : )
              Some(&'>') => {
                self.next_char();
                match self.chars.peek() {
                  Some(&'=') => {
                    self.next_char();
                    work(Token::Operator(Operator::LeftShiftAssign), ">>=", self.line, self.pos - 2)
                  },
                  _ => work(Token::Operator(Operator::LessEqual), ">>", self.line, self.pos - 1),
                }
              },
              Some(&'=') => {
                self.next_char();
                work(Token::Operator(Operator::LessEqual), ">=", self.line, self.pos - 1)
              },
              _ => work(Token::Operator(Operator::LessThan), ">", self.line, self.pos),
            },

            '&' => match self.chars.peek() {
              Some(&'=') => {
                self.next_char();
                work(Token::Operator(Operator::ANDAssign), "&=", self.line, self.pos - 1)
              },
              Some(&'&') => {
                self.next_char();
                work(Token::Operator(Operator::ANDAssign), "&&", self.line, self.pos - 1)
              },
              _ => work(Token::Operator(Operator::BitwiseAND), "&", self.line, self.pos),
            },

            '|' => match self.chars.peek() {
              Some(&'=') => {
                self.next_char();

                work(Token::Operator(Operator::ORAssign), "|=", self.line, self.pos - 1)
              },
              Some(&'|') => {
                self.next_char();
                work(Token::Operator(Operator::Or), "||", self.line, self.pos - 1)
              },
              _ => work(Token::Operator(Operator::BitwiseOR), "|", self.line, self.pos),
            },

            '^' => match self.chars.peek() {
              Some(&'=') => {
                self.next_char();
                work(Token::Operator(Operator::ORAssign), "^=", self.line, self.pos - 1)
              },
              _ => work(Token::Operator(Operator::BitwiseXOR), "^", self.line, self.pos),
            },

            _ => {
              if ch.is_whitespace() {
                continue;
              } else if ch.is_alphabetic() || ch.is_numeric() || ch == '_' {
                let mut parse = ch.to_string();
                let mut start = self.pos;
                while let Some(fc) = self.chars.peek() {
                  if fc.is_alphabetic() || fc.is_numeric() || fc == &'_' {
                    parse.push(self.next_char().unwrap());
                  } else {
                    break;
                  }
                }
                let slice = &self.code[start-1..=self.pos-1];
                // Return, If, Else, For, Do, While, Break, Continue, String, Struct,
                // todo()! void, and other types...
                let pos = self.pos - slice.len();
                return match slice.to_lowercase().as_str() {
                  "int" => work(Token::Keyword(Keyword::Int), "int", self.line, pos),
                  "return" => work(Token::Keyword(Keyword::Return), "return", self.line, pos),
                  "if" => work(Token::Keyword(Keyword::If), "if", self.line, pos),
                  "else" => work(Token::Keyword(Keyword::Else), "else", self.line, pos),
                  "for" => work(Token::Keyword(Keyword::For), "for", self.line, pos),
                  "break" => work(Token::Keyword(Keyword::Break), "break", self.line, pos),
                  "continue" => work(Token::Keyword(Keyword::Continue), "continue", self.line, pos),
                  "string" => work(Token::Keyword(Keyword::String), "string", self.line, pos), // why????
                  "struct" => work(Token::Keyword(Keyword::Struct), "struct", self.line, pos),
                  _ => {
                    return match slice.to_string().parse::<i32>() {
                      Ok(it) => work(Token::Integer(it), slice, self.line, start),
                      Err(_) => work(Token::String(slice.clone().to_string()), slice, self.line, start),
                    };
                  }
                }
              }
              self.next_char();
              unimplemented!()
            }
          }
        }
      }
    }
  }
}

#[test]
fn foo1() {
  let code = "<<=";
  let mut tokenizer = Tokenizer::new(code);
  assert_eq!(tokenizer.next().unwrap().token, Token::Operator(Operator::LeftShiftAssign));

  let code = "<==";
  let mut tokenizer = Tokenizer::new(code);
  assert_eq!(tokenizer.next().unwrap().token, Token::Operator(Operator::LessEqual));

  let code = "||&";
  let mut tokenizer = Tokenizer::new(code);
  assert_eq!(tokenizer.next().unwrap().token, Token::Operator(Operator::Or));

  let code = "131312e";
  let mut tokenizer = Tokenizer::new(code);

  if let Token::String(_) = tokenizer.next().unwrap().token {} else {
    assert!(false);
  }

  let code = "131312";
  let mut tokenizer = Tokenizer::new(code);

  if let Token::Integer(_) = tokenizer.next().unwrap().token {} else {
    assert!(false);
  }

  let code = "struct C {};";
  let mut tokenizer = Tokenizer::new(code);

  println!("{:?}", tokenizer.next().unwrap());
}
