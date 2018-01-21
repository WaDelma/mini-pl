use num_bigint::BigInt;

use parsco::{Parser, tag, many0, alt, fun, preceded, terminated, delimited, take_while, take_until, ws, fst, opt, map};

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
    terminated(many0(preceded(
        ws(opt(fun(comment))),
        ws(
            alt()
                | fun(operator)
                | fun(punctuation)
                | fun(keyword)
                | fun(identifier)
                | fun(integer)
                | fun(str_literal)
        )
    )), ws(opt(fun(comment)))).parse(s)
}

fn comment(s: &str) -> Option<((), &str)> {
    (alt()
        | fun(multiline_comment)
        | terminated(tag("//", ()), take_while(|c| c != '\n'))
    ).parse(s)
}

fn multiline_comment(s: &str) -> Option<((), &str)> {
    fn nested_comment(s: &str) -> Option<((), &str)> {
        take_until(
            alt()
                | tag("*/", true)
                | tag("/*", false)
        ).parse(s)
            .and_then(|((_, end), s)| if end {
                None
            } else {
                map(delimited(
                    opt(fun(nested_comment)),
                    take_until(tag("*/", ())),
                    opt(fun(nested_comment))
                ), |_| ()).parse(s)
            })
    }
    map(delimited(
        tag("/*", ()),
        opt(fun(nested_comment)),
        take_until(tag("*/", ()))
    ), |_| ()).parse(s)
}

fn operator(s: &str) -> Option<(Token, &str)> {
    (alt()
        | tag("+", Addition)
        | tag("-", Substraction)
        | tag("*", Multiplication)
        | tag("/", Division)
        | tag("&", And)
        | tag("!", Not)
        | tag(":=", Assignment)
        | tag("=", Equality)
        | tag("<", LessThan)
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
    fst()
        .parse(s)
        .and_then(|(t, s)|
            if t.is_alphabetic() {
                Some((t, s))
            } else {
                None
            })
        .map(|(t1, s)|
            if let Some((t2, s)) = take_while(|c| char::is_alphabetic(c) || char::is_digit(c, 10) || c == '_').parse(s) {
                (format!("{}{}", t1, t2), s)
            } else {
                (t1.to_string(), s)
            }
        )
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
        .map(|(p, s)| (Token::Literal(Integer(p.parse::<BigInt>().unwrap())), s))
}