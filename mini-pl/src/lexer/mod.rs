use std::char;
use std::num::ParseIntError;
use std::string::FromUtf8Error;
use std::str::Utf8Error;

use num_bigint::BigInt;

use parsco::{Parser, FromErr, tag, many0, alt, fun, preceded, terminated, delimited, take_while, take_until, ws, fst, opt, map, eat, take, flat_map};

use self::tokens::Token;
use self::tokens::Punctuation::*;
use self::tokens::Side::*;
use self::tokens::Keyword::*;
use self::tokens::Operator::*;
use self::tokens::Literal::*;
use self::LexError::*;

type Result<'a, T> = ::parsco::Result<&'a str, T, LexError>;

pub mod tokens;
#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq)]
pub enum LexError {
    HexadecimalLexError(HexadecimalLexError),
    Unknown,
}

impl FromErr<()> for LexError {
    fn from(_: ()) -> Self {
        LexError::Unknown
    }
}

impl FromErr<LexError> for LexError {
    fn from(l: LexError) -> Self {
        l
    }
}

impl From<HexadecimalLexError> for LexError {
    fn from(e: HexadecimalLexError) -> Self {
        LexError::HexadecimalLexError(e)
    }
}

#[derive(Debug, PartialEq)]
pub enum HexadecimalLexError {
    ParseIntError(ParseIntError),
    FromUtf8Error(Utf8Error),
}

impl From<ParseIntError> for HexadecimalLexError {
    fn from(e: ParseIntError) -> Self {
        HexadecimalLexError::ParseIntError(e)
    }
}

impl From<FromUtf8Error> for HexadecimalLexError {
    fn from(e: FromUtf8Error) -> Self {
        HexadecimalLexError::FromUtf8Error(e.utf8_error())
    }
}

pub fn tokenize(s: &str) -> Result<Vec<Token>> {
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
        .map_err(FromErr::from)
}

fn comment(s: &str) -> Result<()> {
    (alt()
        | fun(multiline_comment)
        | map((tag("//"), take_while(|c| c != '\n')), |_| ())
    ).parse(s)
        .map_err(FromErr::from)
}

fn multiline_comment(s: &str) -> Result<()> {
    fn nested_comment(s: &str) -> Result<()> {
        take_until(
            alt()
                | map(tag("*/"), |_| true)
                | map(tag("/*"), |_| false)
        ).parse(s)
            .map_err(FromErr::from)
            .and_then(|((_, end), s)| if end {
                Err(Unknown)
            } else {
                map((
                    opt(fun(nested_comment)),
                    take_until(tag("*/")),
                    opt(fun(nested_comment))
                ), |_| ()).parse(s)
                    .map_err(FromErr::from)
            })
    }
    map(delimited(
        tag("/*"),
        opt(fun(nested_comment)),
        take_until(tag("*/"))
    ), |_| ()).parse(s)
        .map_err(FromErr::from)
}

fn operator(s: &str) -> Result<Token> {
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
        .map_err(FromErr::from)
        .map(|(p, s)| (Token::Operator(p), s))
}

fn punctuation(s: &str) -> Result<Token> {
    (alt()
        | eat(tag(";"), Semicolon)
        | eat(tag(":"), Colon)
        | eat(tag("("), Parenthesis(Open))
        | eat(tag(")"), Parenthesis(Close))
    ).parse(s)
        .map_err(FromErr::from)
        .map(|(p, s)| (Token::Punctuation(p), s))
}

// TODO: This doesn't allow identifiers that start with keyword
fn keyword(s: &str) -> Result<Token> {
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
        .map_err(FromErr::from)
        .map(|(p, s)| (Token::Keyword(p), s))
}

fn identifier(s: &str) -> Result<Token> {
    fst()
        .parse(s)
        .map_err(FromErr::from)
        .and_then(|(t, s)|
            if t.is_alphabetic() {
                Ok((t, s))
            } else {
                Err(Unknown)
            })
        .map(|(t1, s)|
            if let Ok((t2, s)) = take_while(|c| char::is_alphanumeric(c) || c == '_').parse(s) {
                (format!("{}{}", t1, t2), s)
            } else {
                (t1.to_string(), s)
            }
        )
        .map(|(p, s)| (Token::Identifier(p), s))
}

fn integer(s: &str) -> Result<Token> {
    take_while(|c| char::is_digit(c, 10))
        .parse(s)
        .map_err(FromErr::from)
        .map(|(p, s)| (Token::Literal(Integer(p.parse::<BigInt>().unwrap())), s))
}

fn hex_as_string(x: &str) -> String {
    char::from_u32(
        u32::from_str_radix(x, 16).unwrap()
    ).unwrap().to_string()
}

fn str_literal(s: &str) -> Result<Token> {
    fn parse_str(s: &str) -> Result<String> {
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
                | map(
                    preceded(
                        tag(r#"\"#),
                        alt()
                            | flat_map(
                                take(3),
                                |x| u8::from_str_radix(x, 8).ok()
                            )
                            | flat_map(
                                take(2),
                                |x| u8::from_str_radix(x, 8).ok()
                            )
                            | flat_map(
                                take(1),
                                |x| u8::from_str_radix(x, 8).ok()
                            )
                    ),
                    |x| String::from_utf8(vec![x]).unwrap()
                )
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
            .map_err(FromErr::from)
            .and_then(|((b, t), s)| if t.is_empty() {
                Ok((b.to_string(), s))
            } else if let Ok((a, s)) = parse_str(s) {
                Ok((b.to_string() + &t + &a, s))
            } else {
                Err(Unknown)
            })
            
    }
    preceded(
        tag("\""),
        fun(parse_str),
    ).parse(s)
        .map_err(FromErr::from)
        .map(|(p, s)| (Token::Literal(StringLit(p)), s))
}