use Ident;

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
    Integer(i64),
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