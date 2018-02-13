use num_bigint::BigInt;

use Ident;

#[derive(Clone, Debug, PartialEq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn new(line: usize, column: usize) -> Self {
        Position {
            line,
            column
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Tok {
    pub token: Token,
    pub from: Position,
    pub to: Position,
}

impl Tok {
    pub fn new(token: Token, from: Position, to: Position) -> Self {
        Tok {
            token,
            from,
            to
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Identifier(Ident),
    Literal(Literal),
    Punctuation(Punctuation),
    Keyword(Keyword),
    Operator(Operator),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Integer(BigInt),
    StringLit(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operator {
    Assignment,
    Equality,
    LessThan,
    Addition,
    Substraction,
    Multiplication,
    Division,
    Range,
    And,
    Not,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Punctuation {
    Semicolon,
    Colon,
    Parenthesis(Side),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Side {
    Open,
    Close
}

#[derive(Clone, Debug, PartialEq)]
pub enum Keyword {
    Var,
    For,
    End,
    In,
    Do,
    Read,
    Print,
    Int,
    Str,
    Bool,
    Assert,
}