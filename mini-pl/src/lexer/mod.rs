//! Turns input string to vector of tokens of mini-pl language.
//! 
//! Lexing also records the line and column positions of the tokens in the input string.
//! 
//! Handled tokenization errors are inserted as special tokens and unhandled errors
//! will bubble out as `Err` variant of the `Result` enum.
//! 
//! There shouldn't be any panics while tokenizing.
use std::char;
use std::cell::Cell;

use parsco::{Parser, FromErr, tag, many0, alt, fun, preceded, terminated, take_while0, take_while1, take_until, ws, fst, opt, map, eat, take, flat_map, satisfying, take_nm};
use parsco::common::Void;

use self::tokens::{Token, LexError, HexadecimalLexError, OctalLexError};
use self::tokens::Punctuation::*;
use self::tokens::Side::*;
use self::tokens::Keyword::*;
use self::tokens::Operator::*;
use self::tokens::Literal::*;
use self::tokens::LexError::*;
use util::{Positioned, Position, UpdateCell};

//TODO: Remove LexError from error type.
type LexResult<'a, T> = ::parsco::Result<&'a str, T, self::tokens::LexError>;

pub mod tokens;
#[cfg(test)]
mod tests;

/// Lexes given string to mini-pl tokens
pub fn tokenize(s: &str) -> LexResult<Vec<Positioned<Token>>> {
    // These variables keep track the line and column were at lexing.
    let line = Cell::new(0);
    let column = Cell::new(0);
    terminated(
        many0(
            map(
                (
                    // Handles comment before first token and between tokens
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
                    // Increment the line counter by the amount preceding comment and statement takes.
                    let cur_line = line.update(|c| c + comment_lines + preceding_lines);

                    let has_comment_lines = comment_lines > 0;
                    let has_preceding_lines = preceding_lines > 0;

                    let cur_column = if has_comment_lines || has_preceding_lines {
                        // There were lines so we need to move column counter to right place.
                        column.set(token_size + if has_preceding_lines {
                            preceding_columns
                        } else {
                            comment_columns
                        });
                        // And the preceding column count is 0.
                        0
                    } else {
                        // We didn't advance any lines so we increment column counter by eaten characters.
                        column.update(|c| c + eaten_chars)
                    };

                    Positioned {
                        data: token,
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
        ),
        // Handles comment after last token
        ws(opt(fun(comment)))
    ).parse(s)
        .map_err(|(_err, pos)| (LexError::Unknown, pos)) // TODO: Better error
}

/// Parses multiline comments
pub fn comment(input: &str) -> LexResult<()> {
    map((
        tag("{*"),
        opt(fun(nested_comment)),
        take_until(tag("*}"))
    ), |_, _, _| ())
        .parse(input)
        .map_err(|(_err, pos)| (LexError::Unknown, pos)) // TODO: Better error
}

/// Handles nested multiline comments
pub fn nested_comment(input: &str) -> LexResult<()> {
    map((
        satisfying(
            take_until(alt()
                // If we find *} we are at the end of the multiline comment
                | map(tag("*}"), |_, _, _| false)
                // If we find {* there is nested multiline comment
                | map(tag("{*"), |_, _, _| true)),
            |&(_, c)| c
        ),
        // Handle nested multiline comments within multiline comments
        opt(fun(nested_comment)),
        take_until(tag("*}")),
        // Handle adjanced multiline comments withing multiline comments
        opt(fun(nested_comment))
    ), |_, _, _| ())
        .parse(input)
        .map_err(|(_err, pos)| (LexError::Unknown, pos)) // TODO: Better error
}

/// Lexes single operator
pub fn operator(input: &str) -> LexResult<Token> {
    map(alt()
            | eat(tag("+"), Addition)
            | eat(tag("-"), Substraction)
            | eat(tag("*"), Multiplication)
            | eat(tag("/"), Division)
            | eat(tag(":="), Assignment)
            | eat(tag("<>"), Inequality)
            | eat(tag(">="), GreaterThanEqual)
            | eat(tag(">"), GreaterThan)
            | eat(tag("<="), LessThanEqual)
            | eat(tag("<"), LessThan)
            | eat(tag("="), Equality)
            | eat(tag(".."), Range),
        |oper, _, _| Token::Operator(oper)
    ).parse(input)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
}

/// Lexes single punctuation
pub fn punctuation(input: &str) -> LexResult<Token> {
    map(alt()
            | eat(tag(";"), Semicolon)
            | eat(tag(":"), Colon)
            | eat(tag("."), Dot)
            | eat(tag(","), Comma)
            | eat(tag("("), Parenthesis(Open))
            | eat(tag(")"), Parenthesis(Close)),
        |punct, _, _| Token::Punctuation(punct)
    ).parse(input)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
}

/// Lexes reserved keywords
pub fn keyword(input: &str) -> LexResult<Token> {
    (alt()
        | eat(tag("and"), Token::Operator(And))
        | eat(tag("not"), Token::Operator(Not))
        | eat(tag("or"), Token::Operator(Or))
        | map(alt()
            | eat(tag("program"), Program)
            | eat(tag("procedure"), Procedure)
            | eat(tag("function"), Function)
            | eat(tag("var"), Var)
            | eat(tag("while"), While)
            | eat(tag("do"), Do)
            | eat(tag("begin"), Begin)
            | eat(tag("end"), End)
            | eat(tag("assert"), Assert)
            | eat(tag("array"), Array)
            | eat(tag("of"), Of)
            | eat(tag("if"), If)
            | eat(tag("then"), Then)
            | eat(tag("else"), Else),
        |keyword, _, _| Token::Keyword(keyword)
    )).parse(input)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
}

/// Checks if given character is letter [a-zA-Z]
pub fn is_letter(c: char) -> bool {
    match c {
        'a'...'z' | 'A'...'Z' => true,
        _ => false,
    }
}

/// Checks if given character is digit [0-9]
pub fn is_digit(c: char) -> bool {
    match c {
        '0'...'9' => true,
        _ => false,
    }
}

/// Lexes either identifier or keyword
pub fn keyword_or_identifier(input: &str) -> LexResult<Token> {
    map((
        satisfying(fst(), |c| is_letter(*c)),
        take_while0(|c| is_letter(c) || is_digit(c) || c == '_')
    ), |(fst, rest), _, _| {
        let ident = fst.to_string() + rest;
        // Check that is the identifier actually a keyword
        if let Ok((keyword, after_keyword, _)) = keyword(&ident) {
            if after_keyword.is_empty() {
                return keyword;
            }
        }
        Token::Identifier(ident)
    }).parse(input)
        .map_err(|(_err, pos)| (LexError::Unknown, pos)) // TODO: Better error
}

/// Lexes integer literal
pub fn integer(input: &str) -> LexResult<Token> {
    flat_map(
        take_while1(|c| c.is_alphanumeric()),
        |number: &str, rest, pos| {
            Ok::<_, (Void, _)>((
                number.parse()
                    .map(|i| Token::Literal(Integer(i)))
                    .unwrap_or_else(|e| Token::Error(e.into())),
                rest,
                pos
            ))
        }
    )
    .parse(input)
    .map_err(|(err, pos)| (FromErr::from(err), pos))
}

/// Lexes string literal
pub fn str_literal(input: &str) -> LexResult<Token> {
    map(preceded(
            tag(r#"""#),
            fun(str_contents),
        ),
        |string, _, _| string.into_res()
            .map(StringLit)
            .map(Token::Literal)
            .unwrap_or_else(Token::Error)
    ).parse(input)
        .map_err(|(_err, pos)| (LexError::Unknown, pos)) // TODO: Better error
}

/// Parses the contents of string literal
pub fn str_contents(input: &str) -> LexResult<StrOrLexErr> {
    use self::StrOrLexErr::*;
    flat_map(take_until(
        alt()
            // Alert (Beep, Bell)
            | eat(tag(r#"\a"#), Str("\x07".into()))
            // Backspace
            | eat(tag(r#"\b"#), Str("\x08".into()))
            // Formfeed
            | eat(tag(r#"\f"#), Str("\x0C".into()))
            // Newline
            | eat(tag(r#"\n"#), Str("\n".into()))
            // Carriage Return
            | eat(tag(r#"\r"#), Str("\r".into()))
            // Horizontal Tab
            | eat(tag(r#"\t"#), Str("\t".into()))
            // Vertical Tab
            | eat(tag(r#"\v"#), Str("\x0B".into()))
            // Backslash
            | eat(tag(r#"\\"#), Str("\\".into()))
            // Single quotation mark
            | eat(tag(r#"\'"#), Str("\'".into()))
            // Double quotation mark
            | eat(tag(r#"\""#), Str("\"".into()))
            // Question mark
            | eat(tag(r#"\?"#), Str("?".into()))
            // The byte whose numerical value is given by nnn interpreted as an octal number
            | preceded(
                tag(r#"\"#),
                map(
                    take_nm(1, 3, |c| char::is_digit(c, 8)),
                    |x, _, _| oct_as_byte(x).into()
                )
            )
            // The byte whose numerical value is given by hh… interpreted as a hexadecimal number
            | preceded(
                tag(r#"\x"#),
                map(
                    take_while1(|c| char::is_digit(c, 16)),
                    |x, _, _| hex_as_byte(x).into()
                )
            )
            // Escape character
            | eat(tag(r#"\e"#), Str("\x1B".into()))
            | map(alt()
                    // Unicode code point where h is a hexadecimal digit
                    | preceded(tag(r#"\U"#), take(8))
                    // Unicode code point below 10000 hexadecimal
                    | preceded(tag(r#"\u"#), take(4)),
                |r, _, _| hex_as_char(r).into()
            )
            | map((tag(r#"\"#), fst()), |(_, escape), _, _| LexErr(UnknownEscape(escape.to_string())))
            | eat(tag(r#"""#), Str("".into()))
    ), |(str_before, escape), rest, pos|
        Ok(match escape {
            // We found end of the string
            Str(ref e) if e.is_empty() => (Str(str_before.into()), rest, pos),
            // We found escape and well try to find next escape/end of string recursively.
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
            // If there was unknown escape we skip to the ending " and continue lexing tokens after that.
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
pub fn hex_as_char(x: &str) -> Result<String, HexadecimalLexError> {
    use self::HexadecimalLexError::*;
    Ok(char::from_u32(
        u32::from_str_radix(x, 16)?
    ).ok_or(InvalidUtf8)?.to_string())
}

/// Trasforms given octal code to byte corresponding to it
pub fn oct_as_byte(x: &str) -> Result<String, OctalLexError> {
    use self::OctalLexError::*;
    Ok(char::from_u32(
        u8::from_str_radix(x, 8)? as u32
    ).ok_or(InvalidUtf8)?.to_string())
}

/// Trasforms given hexadecimal code to byte corresponding to it
pub fn hex_as_byte(x: &str) -> Result<String, HexadecimalLexError> {
    use self::HexadecimalLexError::*;
    Ok(char::from_u32(
        u8::from_str_radix(x, 16)? as u32
    ).ok_or(InvalidUtf8)?.to_string())
}

/// Helper enum for parsing contents of string literal.
#[derive(Clone)]
pub enum StrOrLexErr {
    /// Part of string literal
    Str(String),
    /// Lex error that happened while lexing contents of string literal
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