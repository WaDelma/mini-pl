use parsco::{Parser, tag, many0, alt, fun, terminated, delimited, take_while, ws};

use self::tokens::Token;
use self::tokens::Punctuation::*;
use self::tokens::Side::*;
use self::tokens::Keyword::*;
use self::tokens::Operator::*;
use self::tokens::Literal::*;

pub mod tokens;
#[cfg(test)]
mod tests;

pub fn tokenize(s: &str) -> Option<(Vec<Token>, &str)> {
    terminated(many0(ws(
        alt()
            | fun(operator)
            | fun(punctuation)
            | fun(keyword)
            | fun(identifier)
            | fun(integer)
            | fun(str_literal)
    )), ws(())).parse(s)
}

fn operator(s: &str) -> Option<(Token, &str)> {
    (alt()
        | tag("+", Addition)
        | tag("-", Substraction)
        | tag("*", Multiplication)
        | tag(":=", Assignment)
        | tag("=", Equality)
        | tag("..", Range)
    ).parse(s)
        .map(|(p, s)| (Token::Operator(p), s))
}

fn punctuation(s: &str) -> Option<(Token, &str)> {
    (alt()
        | tag(";", Semicolon)
        | tag(":", Colon)
        | tag("(", Parenthesis(Open))
        | tag(")", Parenthesis(Close))
    ).parse(s)
        .map(|(p, s)| (Token::Punctuation(p), s))
}

fn keyword(s: &str) -> Option<(Token, &str)> {
    (alt()
        | tag("var", Var)
        | tag("for", For)
        | tag("end", End)
        | tag("do", Do)
        | tag("read", Read)
        | tag("print", Print)
        | tag("int", Int)
        | tag("in", In)
        | tag("string", Str)
        | tag("bool", Bool)
        | tag("assert", Assert)
    ).parse(s)
        .map(|(p, s)| (Token::Keyword(p), s))
}

fn identifier(s: &str) -> Option<(Token, &str)> {
    take_while(char::is_alphabetic)
        .parse(s)
        .map(|(p, s)| (Token::Identifier(p), s))
}

fn str_literal(s: &str) -> Option<(Token, &str)> {
    delimited(
        tag("\"", ()),
        take_while(|c| c != '"'),
        tag("\"", ())
    )
        .parse(s)
        .map(|(p, s)| (Token::Literal(StringLit(p)), s))
}

fn integer(s: &str) -> Option<(Token, &str)> {
    take_while(|c| char::is_digit(c, 10))
        .parse(s)
        .map(|(p, s)| (Token::Literal(Integer(p.parse::<i64>().unwrap())), s))
}