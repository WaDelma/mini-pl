use super::super::{Token, LexError, tokenize};
use super::tok;

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