use super::super::{Tok, Token, LexError, InvalidInteger, Position, tokenize};
use super::super::tokens::Literal;
use super::tok;

use std::num::ParseIntError;

#[test]
fn error_invalid_escape() {
    assert_eq!(
        Ok((
            vec![
                tok(Token::Error(LexError::UnknownEscape("i".into())), (0, 0), (0, 4))
            ],
            "",
            4
        )),
        tokenize(r#""\i""#)
    );
}

#[test]
fn error_invalid_identifier() {
    assert_eq!(
        Ok((
            vec![
                tok(Token::Error(LexError::InvalidInteger), (0, 0), (0, 4))
            ],
            "",
            4
        )),
        tokenize(r#"1foo"#)
    );
}