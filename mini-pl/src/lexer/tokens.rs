use std::num::ParseIntError;
use std::string::FromUtf8Error;
use std::str::Utf8Error;

use num_bigint::{BigInt, ParseBigIntError};

use parsco::{Sym, FromErr};

use Ident;
use util::Positioned;

impl Sym for Positioned<Token> {
    type Sym = Token;
    fn sym(&self) -> &Self::Sym {
        &self.data
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Error(LexError),
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

#[derive(Debug, PartialEq, Clone)]
pub enum LexError {
    HexadecimalLexError(HexadecimalLexError),
    OctalLexError(OctalLexError),
    InvalidInteger,
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

impl From<ParseBigIntError> for LexError {
    fn from(_: ParseBigIntError) -> Self {
        LexError::InvalidInteger
    }
}

impl From<HexadecimalLexError> for LexError {
    fn from(e: HexadecimalLexError) -> Self {
        LexError::HexadecimalLexError(e)
    }
}

impl From<OctalLexError> for LexError {
    fn from(e: OctalLexError) -> Self {
        LexError::OctalLexError(e)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum OctalLexError {
    ParseIntError(ParseIntError),
    FromUtf8Error(Utf8Error),
    InvalidUtf8,
}

impl From<ParseIntError> for OctalLexError {
    fn from(e: ParseIntError) -> Self {
        OctalLexError::ParseIntError(e)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum HexadecimalLexError {
    ParseIntError(ParseIntError),
    FromUtf8Error(Utf8Error),
    InvalidUtf8,
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
