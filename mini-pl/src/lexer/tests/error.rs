use super::super::{Tok, Token, LexError, Position, tokenize};
use super::super::tokens::Literal;
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