use super::super::{Tok, Token, LexError, Position, tokenize};

#[test]
fn error_invalid_escape() {
    assert_eq!(
        Err((LexError::UnknownEscape("i".into()), 0..1)),
        tokenize(r#""\i""#)
    );
}