use std::char;

use num_bigint::BigInt;

use parsco::{Parser, tag, many0, alt, fun, preceded, terminated, delimited, take_while, take_until, ws, fst, opt, map, eat, take};

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
        | map((tag("//"), take_while(|c| c != '\n')), |_| ())
    ).parse(s)
}

fn multiline_comment(s: &str) -> Option<((), &str)> {
    fn nested_comment(s: &str) -> Option<((), &str)> {
        take_until(
            alt()
                | map(tag("*/"), |_| true)
                | map(tag("/*"), |_| false)
        ).parse(s)
            .and_then(|((_, end), s)| if end {
                None
            } else {
                map((
                    opt(fun(nested_comment)),
                    take_until(tag("*/")),
                    opt(fun(nested_comment))
                ), |_| ()).parse(s)
            })
    }
    map(delimited(
        tag("/*"),
        opt(fun(nested_comment)),
        take_until(tag("*/"))
    ), |_| ()).parse(s)
}

fn operator(s: &str) -> Option<(Token, &str)> {
    (alt()
        | eat(tag("+"), Addition)
        | eat(tag("-"), Substraction)
        | eat(tag("*"), Multiplication)
        | eat(tag("/"), Division)
        | eat(tag("&"), And)
        | eat(tag("!"), Not)
        | eat(tag(":="), Assignment)
        | eat(tag("="), Equality)
        | eat(tag("<"), LessThan)
        | eat(tag(".."), Range)
    ).parse(s)
        .map(|(p, s)| (Token::Operator(p), s))
}

fn punctuation(s: &str) -> Option<(Token, &str)> {
    (alt()
        | eat(tag(";"), Semicolon)
        | eat(tag(":"), Colon)
        | eat(tag("("), Parenthesis(Open))
        | eat(tag(")"), Parenthesis(Close))
    ).parse(s)
        .map(|(p, s)| (Token::Punctuation(p), s))
}

fn keyword(s: &str) -> Option<(Token, &str)> {
    (alt()
        | eat(tag("var"), Var)
        | eat(tag("for"), For)
        | eat(tag("end"), End)
        | eat(tag("do"), Do)
        | eat(tag("read"), Read)
        | eat(tag("print"), Print)
        | eat(tag("int"), Int)
        | eat(tag("in"), In)
        | eat(tag("string"), Str)
        | eat(tag("bool"), Bool)
        | eat(tag("assert"), Assert)
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

fn hex_as_string(x: &str) -> String {
    char::from_u32(
        u32::from_str_radix(&x, 16).unwrap()
    ).unwrap().to_string()
}

fn str_literal(s: &str) -> Option<(Token, &str)> {
    fn parse_str(s: &str) -> Option<(String, &str)> {
        take_until(
            alt()
                | eat(tag(r#"\a"#), "\x07".to_owned())
                | eat(tag(r#"\b"#), "\x08".to_owned())
                | eat(tag(r#"\f"#), "\x0C".to_owned())
                | eat(tag(r#"\n"#), "\n".to_owned())
                | eat(tag(r#"\t"#), "\t".to_owned())
                | eat(tag(r#"\v"#), "\x0B".to_owned())
                | eat(tag(r#"\'"#), "\'".to_owned())
                | eat(tag(r#"\""#), "\"".to_owned())
                | eat(tag(r#"\\"#), "\\".to_owned())
                | eat(tag(r#"\?"#), "?".to_owned())
                // TODO: \nnnn The byte whose numerical value is given by nnn interpreted as an octal number
                | map(
                    preceded(
                        tag(r#"\x"#),
                        take(2)
                    ),
                    |x| String::from_utf8(vec![u8::from_str_radix(x, 16).unwrap()]).unwrap()
                )
                | map(
                    preceded(
                        tag(r#"\U"#),
                        take(8)
                    ),
                    hex_as_string
                )
                | map(
                    preceded(
                        tag(r#"\u"#),
                        take(4)
                    ),
                    hex_as_string
                )
                | map(tag(r#"\"#), |_| panic!("Unknown escape sequence."))
                | eat(tag(r#"""#), "".to_owned())
        ).parse(s)
            .and_then(|((b, t), s)| if t.is_empty() {
                Some((b.to_string(), s))
            } else if let Some((a, s)) = parse_str(s) {
                Some((b.to_string() + &t + &a, s))
            } else {
                None
            })
            
    }
    preceded(
        tag("\""),
        fun(parse_str),
    ).parse(s)
        .map(|(p, s)| (Token::Literal(StringLit(p)), s))
}

fn integer(s: &str) -> Option<(Token, &str)> {
    take_while(|c| char::is_digit(c, 10))
        .parse(s)
        .map(|(p, s)| (Token::Literal(Integer(p.parse::<BigInt>().unwrap())), s))
}