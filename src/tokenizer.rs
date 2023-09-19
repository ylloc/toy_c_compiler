use std::iter::Peekable;
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

  fn next_cher_token(&mut self) -> Option<char> {
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
      return match self.next_cher_token() {
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
                self.next_cher_token();
                work(Token::Operator(Operator::PlusAssign), "+=", self.line, self.pos - 1)
              }
              _ => work(Token::Operator(Operator::Plus), "+", self.line, self.pos),
            },

            '-' => match self.chars.peek() {
              Some(&'=') => {
                self.next_cher_token();
                work(Token::Operator(Operator::PlusAssign), "-=", self.line, self.pos - 1)
              }
              _ => work(Token::Operator(Operator::Minus), "-", self.line, self.pos),
            },

            '*' => match self.chars.peek() {
              Some(&'=') => {
                self.next_cher_token();
                work(Token::Operator(Operator::StarAssign), "*=", self.line, self.pos - 1)
              }
              _ => work(Token::Operator(Operator::Star), "*", self.line, self.pos),
            },

            '~' => work(Token::Operator(Operator::BitwiseComplement), "~", self.line, self.pos),

            '/' => match self.chars.peek() {
              Some(&'=') => {
                self.next_cher_token();
                work(Token::Operator(Operator::SlashAssign), "/=", self.line, self.pos - 1)
              }
              _ => work(Token::Operator(Operator::Slash), "/", self.line, self.pos),
            },

            '%' => match self.chars.peek() {
              Some(&'=') => {
                self.next_cher_token();
                work(Token::Operator(Operator::ModAssign), "%=", self.line, self.pos - 1)
              }
              _ => work(Token::Operator(Operator::Modulo), "%", self.line, self.pos),
            },

            '<' => match self.chars.peek() { // Noob code, but who cares : )
              Some(&'<') => {
                self.next_cher_token();
                match self.chars.peek() {
                  Some(&'=') => {
                    self.next_cher_token();
                    work(Token::Operator(Operator::LeftShiftAssign), "<<=", self.line, self.pos - 2)
                  }
                  _ => work(Token::Operator(Operator::LessEqual), "<<", self.line, self.pos - 1),
                }
              },
              Some(&'=') => {
                self.next_cher_token();
                work(Token::Operator(Operator::LessEqual), "<=", self.line, self.pos - 1)
              },
              _ => work(Token::Operator(Operator::LessThan), "<", self.line, self.pos),
            },

            '>' => match self.chars.peek() { // Noob code, but who cares : )
              Some(&'>') => {
                self.next_cher_token();
                match self.chars.peek() {
                  Some(&'=') => {
                    self.next_cher_token();
                    work(Token::Operator(Operator::LeftShiftAssign), ">>=", self.line, self.pos - 2)
                  },
                  _ => work(Token::Operator(Operator::LessEqual), ">>", self.line, self.pos - 1),
                }
              },
              Some(&'=') => {
                self.next_cher_token();
                work(Token::Operator(Operator::LessEqual), ">=", self.line, self.pos - 1)
              },
              _ => work(Token::Operator(Operator::LessThan), ">", self.line, self.pos),
            },

            '&' => match self.chars.peek() {
              Some(&'=') => {
                self.next_cher_token();
                work(Token::Operator(Operator::ANDAssign), "&=", self.line, self.pos - 1)
              },
              Some(&'&') => {
                self.next_cher_token();
                work(Token::Operator(Operator::ANDAssign), "&&", self.line, self.pos - 1)
              },
              _ => work(Token::Operator(Operator::BitwiseAND), "&", self.line, self.pos),
            },

            '|' => match self.chars.peek() {
              Some(&'=') => {
                self.next_cher_token();

                work(Token::Operator(Operator::ORAssign), "|=", self.line, self.pos - 1)
              },
              Some(&'|') => {
                self.next_cher_token();
                work(Token::Operator(Operator::Or), "||", self.line, self.pos - 1)
              },
              _ => work(Token::Operator(Operator::BitwiseOR), "|", self.line, self.pos),
            },

            '^' => match self.chars.peek() {
              Some(&'=') => {
                self.next_cher_token();
                work(Token::Operator(Operator::ORAssign), "^=", self.line, self.pos - 1)
              },
              _ => work(Token::Operator(Operator::BitwiseXOR), "^", self.line, self.pos),
            },

            _ => {
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
}
