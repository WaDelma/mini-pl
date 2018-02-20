use super::super::{Tok, Token, LexError, Position, tokenize};
use super::super::tokens::Literal;

#[test]
fn error_invalid_escape() {
    assert_eq!(
        Ok(((
            vec![
                Tok::new(Token::Error(LexError::UnknownEscape("i".into())), Position::new(0, 0), Position::new(0, 4))
            ],
            vec![
                LexError::UnknownEscape("i".into())
            ]),
            "",
            4
        )),
        tokenize(r#""\i""#)
    );
}