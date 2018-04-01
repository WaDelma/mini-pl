//! Tokens of mini-pl.
//! 
//! Tokens are grouped to different enums to give more structured types instead of one flat one.
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

/// Mini-pl tokens
///
/// Also contains variant for lexer error.
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    /// Error that happened while lexing a token
    Error(LexError),
    /// Lexed identifier
    Identifier(Ident),
    /// Lexed literal
    Literal(Literal),
    /// Punctuation that can appear in the code
    Punctuation(Punctuation),
    /// One of the keywords
    Keyword(Keyword),
    /// Binary or unary operators
    Operator(Operator),
}

/// Literal tokens
#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    /// Integer literal.
    /// 
    /// It's size wasn't defined in the specification, so this implementation uses big integer for it.
    Integer(BigInt),
    /// String literal.
    /// 
    /// All of the escapes are transformed to their unescaped forms.
    StringLit(String),
}

/// Operator tokens
/// 
/// Some of operator like tokens are also here.
#[derive(Clone, Debug, PartialEq)]
pub enum Operator {
    /// Assignment operator
    Assignment,
    /// Equality comparison operator
    Equality,
    /// Inequality comparison operator
    Inequality,
    /// Less than comparison operator
    LessThan,
    /// Less than or equal comparison operator
    LessThanEqual,
    /// Greater than comparison operator
    GreaterThan,
    /// Greater than or equal comparison operator
    GreaterThanEqual,
    /// Addition/concatenation operator
    Addition,
    /// Substraction operator
    Substraction,
    /// Multiplication operator
    Multiplication,
    /// Modulo operator
    Modulo,
    /// Disivision operator
    Division,
    /// Range use in for
    Range,
    /// And operator
    And,
    /// Or operator
    Or,
    /// Unary not operator
    Not,
}

/// Punctuation tokens
#[derive(Clone, Debug, PartialEq)]
pub enum Punctuation {
    /// Semicolon used for end of statements
    Semicolon,
    /// Colon used for type ascription
    Colon,
    /// Parenthesis used for operator precedence
    Parenthesis(Side),
    /// Square brackets used for arrays
    SquareBracket(Side),
}

/// Bracket sides
#[derive(Clone, Debug, PartialEq)]
pub enum Side {
    /// Opening bracket
    Open,
    /// Closing bracket
    Close
}

/// Keyword tokens
#[derive(Clone, Debug, PartialEq)]
pub enum Keyword {
    /// Start of program keyword
    Program,
    /// Start of procedure keyword
    Procedure,
    /// Start of function keyword
    Function,
    /// Start of variable assigment
    Var,
    /// Start of while-loop
    While
    /// Ending of start of while-loop
    Do,
    /// Ending of block
    End,
    /// Reading to variable
    Read,
    /// Printing variable or literal
    Print,
    /// Integer type
    Int,
    /// String type
    Str,
    /// Boolean type
    Bool,
    /// Array type constructor
    Array,
    /// Part of array type constructor
    Of,
    /// Asserting condition
    Assert,
    /// Returning from function with value
    Return,
    /// Start of branch
    If,
    /// End of start of branch
    Then,
    /// Else branch
    Else
}

/// Errors that can happen while lexing tokens
#[derive(Debug, PartialEq, Clone)]
pub enum LexError {
    /// Parsing hexadecimal number failed
    HexadecimalLexError(HexadecimalLexError),
    /// Parsing octal number failed
    OctalLexError(OctalLexError),
    /// Parsing integer failed
    InvalidInteger,
    /// Unknown escape sequence in string literal
    UnknownEscape(String),
    /// Unknown lexing error
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

/// Errors that can happen while parsing octal numbers
#[derive(Debug, PartialEq, Clone)]
pub enum OctalLexError {
    /// Parsing string as integer failed
    ParseIntError(ParseIntError),
    /// Converting from utf-8 failed
    FromUtf8Error(Utf8Error),
    /// Transforming octal number to utf-8 failed
    InvalidUtf8,
}

impl From<ParseIntError> for OctalLexError {
    fn from(e: ParseIntError) -> Self {
        OctalLexError::ParseIntError(e)
    }
}

/// Errors that can happen while parsing hexadecimal numbers
#[derive(Debug, PartialEq, Clone)]
pub enum HexadecimalLexError {
    /// Parsing string as integer failed
    ParseIntError(ParseIntError),
    /// Converting from utf-8 failed
    FromUtf8Error(Utf8Error),
    /// Transforming hexadecimal number to utf-8 failed
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
