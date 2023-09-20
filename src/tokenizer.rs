use std::any::Any;
use std::iter::Peekable;
use std::num::ParseIntError;
use std::str::Chars;

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum Symbol {
  LBracket, // (
  RBracket, // )
  LBrace,// {
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
  LogicalNegation, // !
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Keyword {
  Int,
  Return,
  If,
  Else,
  For,
  Do,
  While,
  Break,
  Continue,
  String,
  Struct,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
  Symbol(Symbol),
  Operator(Operator),
  Integer(i32),
  Keyword(Keyword),
  String(String),
}

#[derive(Debug, Clone)]
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
          self.pos += 1;
        } else {
          self.pos += 1;
        }
        Some(ch)
      }
    }
  }
}

fn new(token: Token,
       str: &str,
       line: usize,
       pos: usize) -> Option<TokenData> {
  Some(TokenData { token, str, line, pos })
}

impl<'a> Iterator for Tokenizer<'a> {
  type Item = TokenData<'a>;
  fn next(&mut self) -> Option<Self::Item> {
    macro_rules! symbol_token {
      ($variant:expr, $symbol:expr) => {
        new($variant, $symbol, self.line, self.pos)
      };
    }

    macro_rules! operator_token {
      ($variant:expr, $variant_extended: expr, $symbol:expr) => {
        match self.chars.peek() {
          Some(&'=') => {
            self.next_char();
            new($variant_extended, concat!($symbol, "="), self.line, self.pos - 1)
          }
          _ => new($variant, $symbol, self.line, self.pos),
        }
      }
    }

    loop {
      return match self.next_char() {
        None => None,
        Some(ch) => {
          match ch {
            '(' => symbol_token!(Token::Symbol(Symbol::LBracket), "("),
            ')' => symbol_token!(Token::Symbol(Symbol::RBracket), ")"),
            '{' => symbol_token!(Token::Symbol(Symbol::LBrace), "{"),
            '}' => symbol_token!(Token::Symbol(Symbol::RBrace), "}"),
            ';' => symbol_token!(Token::Symbol(Symbol::Semicolon), ";"),
            ':' => symbol_token!(Token::Symbol(Symbol::Colon), ":"),
            '.' => symbol_token!(Token::Symbol(Symbol::Dot), "."),
            ',' => symbol_token!(Token::Symbol(Symbol::Comma), ","),

            '+' => operator_token!(Token::Operator(Operator::Plus), Token::Operator(Operator::PlusAssign), "+"),
            '-' => operator_token!(Token::Operator(Operator::Minus), Token::Operator(Operator::MinusAssign), "-"),
            '/' => operator_token!(Token::Operator(Operator::Slash), Token::Operator(Operator::SlashAssign), "/"),
            '*' => operator_token!(Token::Operator(Operator::Star), Token::Operator(Operator::SlashAssign), "*"),
            '%' => operator_token!(Token::Operator(Operator::Modulo), Token::Operator(Operator::ModAssign), "%"),
            '^' => operator_token!(Token::Operator(Operator::BitwiseXOR), Token::Operator(Operator::XORAssign), "^"),
            '!' => operator_token!(Token::Operator(Operator::LogicalNegation), Token::Operator(Operator::NotEqual), "!"),
            '=' => operator_token!(Token::Operator(Operator::Assignment), Token::Operator(Operator::EqualEqual), "="),


            '<' => match self.chars.peek() { // bad code
              Some(&'<') => {
                self.next_char();
                match self.chars.peek() {
                  Some(&'=') => {
                    self.next_char();
                    new(Token::Operator(Operator::LeftShiftAssign), "<<=", self.line, self.pos - 2)
                  }
                  _ => new(Token::Operator(Operator::BitwiseShiftLeft), "<<", self.line, self.pos - 1),
                }
              },
              Some(&'=') => {
                self.next_char();
                new(Token::Operator(Operator::LessEqual), "<=", self.line, self.pos - 1)
              },
              _ => new(Token::Operator(Operator::LessThan), "<", self.line, self.pos),
            },

            '>' => match self.chars.peek() { // bad code
              Some(&'>') => {
                self.next_char();
                match self.chars.peek() {
                  Some(&'=') => {
                    self.next_char();
                    new(Token::Operator(Operator::BitwiseShiftRight), ">>=", self.line, self.pos - 2)
                  },
                  _ => new(Token::Operator(Operator::BitwiseShiftRight), ">>", self.line, self.pos - 1),
                }
              },
              Some(&'=') => {
                self.next_char();
                new(Token::Operator(Operator::GreaterEqual), ">=", self.line, self.pos - 1)
              },
              _ => new(Token::Operator(Operator::GreaterThan), ">", self.line, self.pos),
            },

            '&' => match self.chars.peek() {
              Some(&'=') => {
                self.next_char();
                new(Token::Operator(Operator::ANDAssign), "&=", self.line, self.pos - 1)
              },
              Some(&'&') => {
                self.next_char();
                new(Token::Operator(Operator::And), "&&", self.line, self.pos - 1)
              },
              _ => new(Token::Operator(Operator::BitwiseAND), "&", self.line, self.pos),
            },

            '|' => match self.chars.peek() {
              Some(&'=') => {
                self.next_char();

                new(Token::Operator(Operator::ORAssign), "|=", self.line, self.pos - 1)
              },
              Some(&'|') => {
                self.next_char();
                new(Token::Operator(Operator::Or), "||", self.line, self.pos - 1)
              },
              _ => new(Token::Operator(Operator::BitwiseOR), "|", self.line, self.pos),
            },

            _ => {
              if ch.is_whitespace() || ch == '\n' { continue; }

              if !ch.is_alphabetic() && !ch.is_numeric() && ch != '_' { unimplemented!() }

              let mut parse = ch.to_string();
              let mut start = self.pos;

              while let Some(fc) = self.chars.peek() {
                if !fc.is_alphabetic() && !fc.is_numeric() && fc != &'_' { break; }
                parse.push(self.next_char().unwrap());
              }

              let slice = &self.code[start - 1..=self.pos - 1];
              let pos = self.pos - slice.len();

              let token_type = match slice.to_lowercase().as_str() {
                "int" => Token::Keyword(Keyword::Int),
                "return" => Token::Keyword(Keyword::Return),
                "if" => Token::Keyword(Keyword::If),
                "else" => Token::Keyword(Keyword::Else),
                "for" => Token::Keyword(Keyword::For),
                "break" => Token::Keyword(Keyword::Break),
                "continue" => Token::Keyword(Keyword::Continue),
                "string" => Token::Keyword(Keyword::String),
                "struct" => Token::Keyword(Keyword::Struct),
                _ => match parse.to_string().parse::<i32>() {
                  Ok(it) => Token::Integer(it),
                  Err(_) => Token::String(slice.clone().to_string()),
                },
              };
              return new(token_type, slice, self.line, pos);
            }
          }
        }
      };
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

#[test]
fn real_code1() {
  let code = "
  void foo() {
     return 0;
  }";
  // it's actually an invalid example, but while parsing it is not important
  let mut tokenizer = Tokenizer::new(code);
  assert_eq!(tokenizer.clone().nth(0).unwrap().token, Token::String(String::from("void")));
  assert_eq!(tokenizer.nth(5).unwrap().token, Token::Keyword(Keyword::Return));
}

#[test]
fn real_code2() {
  let code = "
  int foo() {
     int x = 3;
     int y = (x != 2);
     return 0;
  }";
  // it's actually an invalid example, but while parsing it is not important
  let mut tokenizer = Tokenizer::new(code);
  // todo!() - test!
}
