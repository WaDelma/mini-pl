use std::char;
use std::num::ParseIntError;
use std::string::FromUtf8Error;
use std::str::Utf8Error;
use std::cell::Cell;

use num_bigint::BigInt;

use parsco::{Parser, FromErr, tag, many0, alt, fun, preceded, terminated, delimited, take_while0, take_while1, take_until, ws, fst, opt, map, eat, take, flat_map, satisfying};

use self::tokens::{Token, Tok, Position};
use self::tokens::Punctuation::*;
use self::tokens::Side::*;
use self::tokens::Keyword::*;
use self::tokens::Operator::*;
use self::tokens::Literal::*;
use self::LexError::*;

//TODO: Remove LexError from error type.
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
                    |token, _, place| (token, place)
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
        .map_err(|(err, pos)| (FromErr::from(err), pos))
}

fn comment(input: &str) -> ParseResult<()> {
    (alt()
        | fun(multiline_comment)
        | map((tag("//"), take_while0(|c| c != '\n')), |_, _, _| ())
    ).parse(input)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
}

fn multiline_comment(input: &str) -> ParseResult<()> {
    fn nested_comment(input: &str) -> ParseResult<()> {
        take_until(
            alt()
                | map(tag("*/"), |_, _, _| true)
                | map(tag("/*"), |_, _, _| false)
        ).parse(input)
            .map_err(|(err, pos)| (FromErr::from(err), pos))
            .and_then(|((_, is_comment_end), rest, pos)| if is_comment_end {
                Err((Unknown, 0..pos))
            } else {
                map((
                    opt(fun(nested_comment)),
                    take_until(tag("*/")),
                    opt(fun(nested_comment))
                ), |_, _, _| ()).parse(rest)
                    .map(|(cur, rest, pos2)| (cur, rest, pos + pos2))
                    .map_err(|(err, pos)| (FromErr::from(err), pos))
            })
    }
    map(delimited(
        tag("/*"),
        opt(fun(nested_comment)),
        take_until(tag("*/"))
    ), |_, _, _| ()).parse(input)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
}

fn operator(input: &str) -> ParseResult<Token> {
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
    ).parse(input)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
        .map(|(oper, rest, pos)| (Token::Operator(oper), rest, pos))
}

fn punctuation(input: &str) -> ParseResult<Token> {
    (alt()
        | eat(tag(";"), Semicolon)
        | eat(tag(":"), Colon)
        | eat(tag("("), Parenthesis(Open))
        | eat(tag(")"), Parenthesis(Close))
    ).parse(input)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
        .map(|(punct, rest, pos)| (Token::Punctuation(punct), rest, pos))
}

fn keyword(input: &str) -> ParseResult<Token> {
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
    ).parse(input)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
        .map(|(keyword, rest, pos)| (Token::Keyword(keyword), rest, pos))
}

fn keyword_or_identifier(input: &str) -> ParseResult<Token> {
    map((
        satisfying(fst(), |c: &char| c.is_alphabetic()),
        take_while0(|c| char::is_alphanumeric(c) || c == '_')
    ), |(fst, rest), _, _| {
        let ident = format!("{}{}", fst, rest);
        if let Ok((keyword, after_keyword, _)) = keyword(&ident) {
            if after_keyword.is_empty() {
                return keyword;
            }
        }
        Token::Identifier(ident)
    }).parse(input)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
}

fn integer(input: &str) -> ParseResult<Token> {
    map(
        take_while1(|c| char::is_digit(c, 10)),
        |number: &str, _, _| Token::Literal(Integer(number.parse::<BigInt>().unwrap()))
    )
    .parse(input)
    .map_err(|(err, pos)| (FromErr::from(err), pos))
}

fn hex_as_string(x: &str) -> String {
    char::from_u32(
        u32::from_str_radix(x, 16).unwrap()
    ).unwrap().to_string()
}

fn str_literal(input: &str) -> ParseResult<Token> {
    fn parse_str(input: &str) -> ParseResult<Result<String, LexError>> {
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
                                |x, rest, pos| u8::from_str_radix(x, 8)
                                    .map(|i| (i, rest, pos))
                                    .map_err(|err| (err, 0..pos))
                            )
                            | flat_map(
                                take(2),
                                |x, rest, pos| u8::from_str_radix(x, 8)
                                    .map(|i| (i, rest, pos))
                                    .map_err(|err| (err, 0..pos))
                            )
                            | flat_map(
                                take(1),
                                |x, rest, pos| u8::from_str_radix(x, 8)
                                    .map(|i| (i, rest, pos))
                                    .map_err(|err| (err, 0..pos))
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
                | map((tag(r#"\"#), fst()), |(_, escape), _, _| Err(UnknownEscape(escape.to_string())))
                | eat(tag(r#"""#), Ok("".to_owned()))
        ).parse(input)
            .map_err(|(err, pos)| (FromErr::from(err), pos))
            .and_then(|((str_before, escape), rest, pos)| escape.map(|escaped| if escaped.is_empty() {
                    Ok((Ok(str_before.to_string()), rest, pos))
                } else if let Ok((str_after, rest, pos2)) = parse_str(rest) {
                    Ok(match str_after {
                        Ok(str_after) => (Ok(str_before.to_string() + &escaped + &str_after), rest, pos + pos2),
                        err => (err, rest, pos + pos2),
                    })
                } else {
                    Err((Unknown, 0..pos))
                }).unwrap_or_else(|err| {
                    Ok(if let Ok((_, rest, pos2)) = take_until(tag(r#"""#)).parse(rest) {
                        (Err(err), rest, pos + pos2)
                    } else {
                        (Err(err), rest, pos)
                    })
                })
            )
    }
    preceded(
        tag("\""),
        fun(parse_str),
    ).parse(input)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
        .map(|(string, rest, pos)| (string.map(|string| Token::Literal(StringLit(string))).unwrap_or_else(Token::Error), rest, pos))
}