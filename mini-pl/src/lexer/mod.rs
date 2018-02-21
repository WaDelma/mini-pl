use std::char;
use std::num::ParseIntError;
use std::string::FromUtf8Error;
use std::str::Utf8Error;
use std::cell::Cell;

use num_bigint::BigInt;

use parsco::{Parser, FromErr, tag, many0, alt, fun, preceded, terminated, delimited, take_while, take_until, ws, fst, opt, map, eat, take, flat_map};

use self::tokens::{Token, Tok, Position};
use self::tokens::Punctuation::*;
use self::tokens::Side::*;
use self::tokens::Keyword::*;
use self::tokens::Operator::*;
use self::tokens::Literal::*;
use self::LexError::*;

//TODO: Use dedicated error token. Remove LexError from error type.
type ParseResult<'a, T> = ::parsco::Result<&'a str, T, LexError>;

pub mod tokens;
#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq, Clone)]
pub enum LexError {
    HexadecimalLexError(HexadecimalLexError),
    UnknownEscape(String),
    Unknown,
}

impl FromErr<()> for LexError {
    fn from(_: ()) -> Self {
        LexError::Unknown
    }
}

impl FromErr<::parsco::common::Void> for LexError {
    fn from(_: ::parsco::common::Void) -> Self {
        unreachable!("Void is never type.")
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

#[derive(Debug, PartialEq, Clone)]
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

pub fn tokenize(s: &str) -> ParseResult<Vec<Tok>> {
    let line = Cell::new(0);
    let column = Cell::new(0);
    terminated(many0(
        map(
            (
                ws(opt(fun(comment))),
                ws(map(
                    alt()
                        | fun(operator)
                        | fun(punctuation)
                        | fun(keyword_or_identifier)
                        | fun(integer)
                        | fun(str_literal),
                    |r, _, p| (r, p)
                ))
            ),
            |((_, comment_lines, comment_columns), ((token, token_size), preceding_lines, preceding_columns)), _, eaten_chars| {
                let cur_line = line.get();
                line.set(cur_line + comment_lines + preceding_lines);
                let mut cur_column = column.get();
                if comment_lines + preceding_lines == 0 {
                    column.set(cur_column + eaten_chars);
                } else if preceding_lines == 0 {
                    cur_column = 0;
                    column.set(comment_columns + token_size);
                } else {
                    cur_column = 0;
                    column.set(preceding_columns + token_size);
                }
                Tok {
                    token,
                    from: Position {
                        line: cur_line + comment_lines,
                        column: cur_column + comment_columns,
                    },
                    to: Position {
                        line: line.get(),
                        column: column.get(),
                    },
                }
            }
        )
    ), ws(opt(fun(comment)))).parse(s)
        .map_err(|(e, r)| (FromErr::from(e), r))
}

fn comment(s: &str) -> ParseResult<()> {
    (alt()
        | fun(multiline_comment)
        | map((tag("//"), take_while(|c| c != '\n')), |_, _, _| ())
    ).parse(s)
        .map_err(|(e, r)| (FromErr::from(e), r))
}

fn multiline_comment(s: &str) -> ParseResult<()> {
    fn nested_comment(s: &str) -> ParseResult<()> {
        take_until(
            alt()
                | map(tag("*/"), |_, _, _| true)
                | map(tag("/*"), |_, _, _| false)
        ).parse(s)
            .map_err(|(e, r)| (FromErr::from(e), r))
            .and_then(|((_, end), s, p)| if end {
                Err((Unknown, 0..p))
            } else {
                map((
                    opt(fun(nested_comment)),
                    take_until(tag("*/")),
                    opt(fun(nested_comment))
                ), |_, _, _| ()).parse(s)
                    .map(|(r, s, pp)| (r, s, p + pp))
                    .map_err(|(e, r)| (FromErr::from(e), r))
            })
    }
    map(delimited(
        tag("/*"),
        opt(fun(nested_comment)),
        take_until(tag("*/"))
    ), |_, _, _| ()).parse(s)
        .map_err(|(e, r)| (FromErr::from(e), r))
}

fn operator(s: &str) -> ParseResult<Token> {
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
        .map_err(|(e, r)| (FromErr::from(e), r))
        .map(|(t, s, p)| (Token::Operator(t), s, p))
}

fn punctuation(s: &str) -> ParseResult<Token> {
    (alt()
        | eat(tag(";"), Semicolon)
        | eat(tag(":"), Colon)
        | eat(tag("("), Parenthesis(Open))
        | eat(tag(")"), Parenthesis(Close))
    ).parse(s)
        .map_err(|(e, r)| (FromErr::from(e), r))
        .map(|(t, s, p)| (Token::Punctuation(t), s, p))
}

fn keyword(s: &str) -> ParseResult<Token> {
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
        .map_err(|(e, r)| (FromErr::from(e), r))
        .map(|(t, s, p)| (Token::Keyword(t), s, p))
}

fn keyword_or_identifier(s: &str) -> ParseResult<Token> {
    fst()
        .parse(s)
        .map_err(|(e, r)| (FromErr::from(e), r))
        .and_then(|(t, s, p)| if t.is_alphabetic() {
            Ok((t, s, p))
        } else {
            Err((Unknown, 0..p))
        })
        .map(|(t1, s, p)| if let Ok((t2, s, pp)) = take_while(|c| char::is_alphanumeric(c) || c == '_').parse(s) {
            (format!("{}{}", t1, t2), s, p + pp)
        } else {
            (t1.to_string(), s, p)
        })
        .map(|(t, s, p)| {
            if let Ok((k, ss, p)) = keyword(&t) {
                if ss.is_empty() {
                    return (k, s, p);
                }
            }
            (Token::Identifier(t), s, p)
        })
}

fn integer(s: &str) -> ParseResult<Token> {
    fst()
        .parse(s)
        .map_err(|(e, r)| (FromErr::from(e), r))
        .and_then(|(t, s, p)| if t.is_digit(10) {
            Ok((t, s, p))
        } else {
            Err((Unknown, 0..p))
        })
        .map(|(t1, s, p)| {
            let (t, s, p) = if let Ok((t2, s, pp)) = take_while(|c| char::is_digit(c, 10)).parse(s) {
                (format!("{}{}", t1, t2), s, p + pp)
            } else {
                (t1.to_string(), s, p)
            };
            (Token::Literal(Integer(t.parse::<BigInt>().unwrap())), s, p)
        })
}

fn hex_as_string(x: &str) -> String {
    char::from_u32(
        u32::from_str_radix(x, 16).unwrap()
    ).unwrap().to_string()
}

fn str_literal(s: &str) -> ParseResult<Token> {
    fn parse_str(s: &str) -> ParseResult<Result<String, LexError>> {
        take_until(
            alt()
                | eat(tag(r#"\a"#), Ok("\x07".to_owned()))
                | eat(tag(r#"\b"#), Ok("\x08".to_owned()))
                | eat(tag(r#"\f"#), Ok("\x0C".to_owned()))
                | eat(tag(r#"\n"#), Ok("\n".to_owned()))
                | eat(tag(r#"\t"#), Ok("\t".to_owned()))
                | eat(tag(r#"\v"#), Ok("\x0B".to_owned()))
                | eat(tag(r#"\'"#), Ok("\'".to_owned()))
                | eat(tag(r#"\""#), Ok("\"".to_owned()))
                | eat(tag(r#"\\"#), Ok("\\".to_owned()))
                | eat(tag(r#"\?"#), Ok("?".to_owned()))
                | map(
                    preceded(
                        tag(r#"\"#),
                        alt()
                            | flat_map(
                                take(3),
                                |x, s, p| u8::from_str_radix(x, 8)
                                    .map(|i| (i, s, p))
                                    .map_err(|e| (e, 0..p))
                            )
                            | flat_map(
                                take(2),
                                |x, s, p| u8::from_str_radix(x, 8)
                                    .map(|i| (i, s, p))
                                    .map_err(|e| (e, 0..p))
                            )
                            | flat_map(
                                take(1),
                                |x, s, p| u8::from_str_radix(x, 8)
                                    .map(|i| (i, s, p))
                                    .map_err(|e| (e, 0..p))
                            )
                    ),
                    |x, _, _| Ok(String::from_utf8(vec![x]).unwrap())
                )
                | map(
                    preceded(
                        tag(r#"\x"#),
                        take(2)
                    ),
                    |x, _, _| Ok(String::from_utf8(vec![u8::from_str_radix(x, 16).unwrap()]).unwrap())
                )
                | map(
                    preceded(
                        tag(r#"\U"#),
                        take(8)
                    ),
                    |r, _, _| Ok(hex_as_string(r))
                )
                | map(
                    preceded(
                        tag(r#"\u"#),
                        take(4)
                    ),
                    |r, _, _| Ok(hex_as_string(r))
                )
                | map((tag(r#"\"#), fst()), |(_, e), _, _| Err(UnknownEscape(e.to_string())))
                | eat(tag(r#"""#), Ok("".to_owned()))
        ).parse(s)
            .map_err(|(e, r)| (FromErr::from(e), r))
            .and_then(|((b, t), s, p)| t.map(|t| if t.is_empty() {
                    Ok((Ok(b.to_string()), s, p))
                } else if let Ok((a, s, pp)) = parse_str(s) {
                    match a {
                        Ok(a) => Ok((Ok(b.to_string() + &t + &a), s, p + pp)),
                        e => Ok((e, s, p + pp)),
                    }
                } else {
                    Err((Unknown, 0..p))
                }).unwrap_or_else(|e| {
                    Ok(if let Ok((_, s, pp)) = take_until(tag(r#"""#)).parse(s) {
                        (Err(e), s, p + pp)
                    } else {
                        (Err(e), s, p)
                    })
                })
            )
    }
    preceded(
        tag("\""),
        fun(parse_str),
    ).parse(s)
        .map_err(|(e, r)| (FromErr::from(e), r))
        .map(|(t, s, p)| (t.map(|t| Token::Literal(StringLit(t))).unwrap_or_else(Token::Error), s, p))
}