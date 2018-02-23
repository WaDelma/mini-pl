use std::char;
use std::cell::Cell;

use parsco::{Parser, FromErr, tag, many0, alt, fun, preceded, terminated, take_while0, take_while1, take_until, ws, fst, opt, map, eat, take, flat_map, satisfying, take_nm};

use self::tokens::{Token, Tok, Position, LexError, HexadecimalLexError, OctalLexError};
use self::tokens::Punctuation::*;
use self::tokens::Side::*;
use self::tokens::Keyword::*;
use self::tokens::Operator::*;
use self::tokens::Literal::*;
use self::tokens::LexError::*;

//TODO: Remove LexError from error type.
type ParseResult<'a, T> = ::parsco::Result<&'a str, T, self::tokens::LexError>;

pub mod tokens;
#[cfg(test)]
mod tests;

/// Lexes given string to mini-pl tokens
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
                column.set(if comment_lines + preceding_lines == 0 {
                    cur_column + eaten_chars
                } else {
                    cur_column = 0;
                    token_size + if preceding_lines == 0 {
                        comment_columns 
                    } else {
                        preceding_columns
                    }
                });

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

/// Parses single and multiline comments
fn comment(input: &str) -> ParseResult<()> {
    (alt()
        | fun(multiline_comment)
        | map((tag("//"), take_while0(|c| c != '\n')), |_, _, _| ())
    ).parse(input)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
}

/// Parses multiline comments
fn multiline_comment(input: &str) -> ParseResult<()> {
    map((
        tag("/*"),
        opt(fun(nested_comment)),
        take_until(tag("*/"))
    ), |_, _, _| ())
        .parse(input)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
}

/// Handles nested multiline comments
fn nested_comment(input: &str) -> ParseResult<()> {
    map((
        satisfying(
            take_until(alt()
                | map(tag("*/"), |_, _, _| false)
                | map(tag("/*"), |_, _, _| true)),
            |&(_, c)| c
        ),
        opt(fun(nested_comment)),
        take_until(tag("*/")),
        opt(fun(nested_comment))
    ), |_, _, _| ())
        .parse(input)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
}

/// Lexes single operator
fn operator(input: &str) -> ParseResult<Token> {
    map(alt()
            | eat(tag("+"), Addition)
            | eat(tag("-"), Substraction)
            | eat(tag("*"), Multiplication)
            | eat(tag("/"), Division)
            | eat(tag("&"), And)
            | eat(tag("!"), Not)
            | eat(tag(":="), Assignment)
            | eat(tag("="), Equality)
            | eat(tag("<"), LessThan)
            | eat(tag(".."), Range),
        |oper, _, _| Token::Operator(oper)
    ).parse(input)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
}

/// Lexes single punctuation
fn punctuation(input: &str) -> ParseResult<Token> {
    map(alt()
            | eat(tag(";"), Semicolon)
            | eat(tag(":"), Colon)
            | eat(tag("("), Parenthesis(Open))
            | eat(tag(")"), Parenthesis(Close)),
        |punct, _, _| Token::Punctuation(punct)
    ).parse(input)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
}

/// Lexes reserved keywords
fn keyword(input: &str) -> ParseResult<Token> {
    map(alt()
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
            | eat(tag("assert"), Assert),
        |keyword, _, _| Token::Keyword(keyword)
    ).parse(input)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
}

/// Lexes either identifier or keyword
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

/// Lexes integer literal
fn integer(input: &str) -> ParseResult<Token> {
    map(
        take_while1(|c| char::is_digit(c, 10)),
        |number: &str, _, _| Token::Literal(Integer(number.parse().unwrap()))
    )
    .parse(input)
    .map_err(|(err, pos)| (FromErr::from(err), pos))
}

/// Lexes string literal
fn str_literal(input: &str) -> ParseResult<Token> {
    map(preceded(
            tag(r#"""#),
            fun(str_contents),
        ),
        |string, _, _| string.into_res()
            .map(StringLit)
            .map(Token::Literal)
            .unwrap_or_else(Token::Error)
    ).parse(input)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
}

/// Parses the contents of string literal
fn str_contents(input: &str) -> ParseResult<StrOrLexErr> {
    use self::StrOrLexErr::*;
    flat_map(take_until(
        alt()
            | eat(tag(r#"\a"#), Str("\x07".into()))
            | eat(tag(r#"\b"#), Str("\x08".into()))
            | eat(tag(r#"\f"#), Str("\x0C".into()))
            | eat(tag(r#"\n"#), Str("\n".into()))
            | eat(tag(r#"\t"#), Str("\t".into()))
            | eat(tag(r#"\v"#), Str("\x0B".into()))
            | eat(tag(r#"\'"#), Str("\'".into()))
            | eat(tag(r#"\""#), Str("\"".into()))
            | eat(tag(r#"\\"#), Str("\\".into()))
            | eat(tag(r#"\?"#), Str("?".into()))
            | preceded(
                tag(r#"\"#),
                map(
                    take_nm(1, 3, |c| char::is_digit(c, 8)),
                    |x, _, _| oct_as_byte(x).into()
                )
            )
            | preceded(
                tag(r#"\x"#),
                map(
                    take_while1(|c| char::is_digit(c, 16)),
                    |x, _, _| hex_as_byte(x).into()
                )
            )
            | map(alt()
                    | preceded(tag(r#"\U"#), take(8))
                    | preceded(tag(r#"\u"#), take(4)),
                |r, _, _| hex_as_char(r).into()
            )
            | map((tag(r#"\"#), fst()), |(_, escape), _, _| LexErr(UnknownEscape(escape.to_string())))
            | eat(tag(r#"""#), Str("".into()))
    ), |(str_before, escape), rest, pos|
        Ok(match escape {
            Str(ref e) if e.is_empty() => (Str(str_before.into()), rest, pos),
            Str(escaped) => if let Ok((str_after, rest, pos2)) = str_contents(rest) {
                (
                    str_after
                        .map(|s| [str_before, &escaped, &s].concat()),
                    rest,
                    pos + pos2
                )
            } else {
                Err((Unknown, 0..pos))?
            },
            err @ LexErr(_) => if let Ok((_, rest, pos2)) = take_until(tag(r#"""#)).parse(rest) {
                (err, rest, pos + pos2)
            } else {
                (err, rest, pos)
            },
        })
    ).parse(input)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
}

/// Trasforms given hexadecimal code to character corresponding to it
fn hex_as_char(x: &str) -> Result<String, HexadecimalLexError> {
    use self::HexadecimalLexError::*;
    Ok(char::from_u32(
        u32::from_str_radix(x, 16)?
    ).ok_or(InvalidUtf8)?.to_string())
}

/// Trasforms given octal code to byte corresponding to it
fn oct_as_byte(x: &str) -> Result<String, OctalLexError> {
    use self::OctalLexError::*;
    Ok(char::from_u32(
        u8::from_str_radix(x, 8)? as u32
    ).ok_or(InvalidUtf8)?.to_string())
}

/// Trasforms given hexadecimal code to byte corresponding to it
fn hex_as_byte(x: &str) -> Result<String, HexadecimalLexError> {
    use self::HexadecimalLexError::*;
    Ok(char::from_u32(
        u8::from_str_radix(x, 16)? as u32
    ).ok_or(InvalidUtf8)?.to_string())
}

#[derive(Clone)]
enum StrOrLexErr {
    Str(String),
    LexErr(LexError),
}

impl<E: Into<LexError>> From<Result<String, E>> for StrOrLexErr {
    fn from(r: Result<String, E>) -> Self {
        use self::StrOrLexErr::*;
        match r {
            Ok(s) => Str(s),
            Err(e) => LexErr(e.into()),
        }
    }
}

impl StrOrLexErr {
    fn map<F>(self, f: F) -> Self
        where F: FnOnce(String) -> String,
    {
        use self::StrOrLexErr::*;
        match self {
            Str(s) => Str(f(s)),
            e => e,
        }
    }

    fn into_res(self) -> Result<String, LexError> {
        use self::StrOrLexErr::*;
        match self {
            Str(s) => Ok(s),
            LexErr(e) => Err(e),
        }
    }
}