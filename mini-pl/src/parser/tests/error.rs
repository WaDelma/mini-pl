use lexer::tokenize;
use lexer::tokens::Keyword;
use lexer::tokens::Side::*;
use util::{Positioned, Position};

use super::super::ast::Stmt::*;
use super::super::ast::Expr::*;
use super::super::ast::Opnd::*;
use super::super::ast::Type::*;
use super::super::ast::OpndError::*;
use super::super::ast::ExprError::*;
use super::super::ast::ParseError::*;
use super::super::ast::TypeError::*;
use super::super::parse;

#[test]
fn error_missing_expr() {
    assert_eq!(
        Ok((
            vec![
                Positioned::new(
                    Print {
                        expr: Positioned::new(
                            Opnd(Positioned::new(
                                OpndErr(InvalidOperand),
                                // TODO: Should these both be 18?
                                Position::new(1, 17),
                                Position::new(1, 18),  
                            )),
                            Position::new(1, 17),
                            Position::new(1, 18),
                        )
                    },
                    Position::new(1, 12),
                    Position::new(1, 18),
                )
            ],
            &[][..],
            2
        )),
        parse(&tokenize("
            print;
        ").unwrap().0)
    );
}

#[test]
fn error_keyword_as_expr() {
    assert_eq!(
        Ok((
            vec![
                Positioned::new(
                    ErrStmt(MissingSemicolon),
                    Position::new(1, 18),
                    Position::new(1, 19)
                ),
            ],
            &[][..],
            3
        )),
        parse(&tokenize("
            print print;
        ").unwrap().0)
    );
}

#[test]
fn error_missing_end_parenthesis_in_expr() {
    assert_eq!(
        Ok((
            vec![
                Positioned::new(
                    Print {
                        expr: Positioned::new(
                            Opnd(Positioned::new(
                                OpndErr(MissingEndParenthesis),
                                Position::new(1, 20),
                                Position::new(1, 20)
                            )),
                            // TODO: Should this be 12?
                            Position::new(1, 20),
                            Position::new(1, 20)
                        )
                    },
                    Position::new(1, 12),
                    Position::new(1, 20)
                )
            ],
            &[][..],
            3
        )),
        parse(&tokenize("
            print (;
        ").unwrap().0)
    );
}

#[test]
fn error_missing_end_parenthesis_content_after_in_expr() {
    assert_eq!(
        Ok((
            vec![
                Positioned::new(
                    Print {
                        expr: Positioned::new(
                            Opnd(Positioned::new(
                                OpndErr(MissingEndParenthesis),
                                Position::new(1, 24),
                                Position::new(1, 24)
                            )),
                            // TODO: Should this be 18?
                            Position::new(1, 24),
                            Position::new(1, 24)
                        )
                    },
                    Position::new(1, 12),
                    Position::new(1, 25)
                )
            ],
            &[][..],
            6
        )),
        parse(&tokenize("
            print (1 + 1;
        ").unwrap().0)
    );
}

#[test]
fn error_missing_semicolon() {
    assert_eq!(
        Ok((
            vec![
                Positioned::new(
                    ErrStmt(MissingSemicolon),
                    Position::new(1, 25),
                    Position::new(1, 25),
                ),
                Positioned::new(
                    Print {
                        expr: Positioned::new(
                            Opnd(Positioned::new(
                                Ident(String::from("X")),
                                Position::new(2, 18),
                                Position::new(2, 19)
                            )),
                            Position::new(2, 18),
                            Position::new(2, 19)
                        ),
                    },
                    Position::new(2, 12),
                    Position::new(2, 20)
                )
            ],
            &[][..],
            9
        )),
        parse(&tokenize("
            print (1 + 1)
            print X;
        ").unwrap().0)
    );
}

#[test]
fn error_missing_semicolon_last_line() {
    assert_eq!(
        Ok((
            vec![
                Positioned::new(
                    ErrStmt(MissingSemicolon),
                    Position::new(1, 25),
                    Position::new(1, 25),
                )
            ],
            &[][..],
            6
        )),
        parse(&tokenize("
            print (1 + 1)
        ").unwrap().0)
    );
}

#[test]
fn error_no_parenthesis() {
    assert_eq!(
        Ok((
            vec![
                
            ],
            &[][..],
            7
        )),
        parse(&tokenize("
            print 1 + 1 + 1;
        ").unwrap().0)
    );
}

#[test]
fn error_no_start_parenthesis_in_assert() {
    assert_eq!(
        Ok((
            vec![
                Positioned::new(
                    Assert {
                        expr: Positioned::new(
                            ErrExpr(MissingParenthesis(Open)),
                            Position::new(1, 19),
                            Position::new(1, 19),
                        )
                    },
                    Position::new(1, 13),
                    Position::new(1, 26),
                )
            ],
            &[][..],
            6
        )),
        parse(&tokenize("
            assert 1 + 1);
        ").unwrap().0)
    );
}

#[test]
fn error_no_end_parenthesis_in_assert() {
    assert_eq!(
        Ok((
            vec![
                Positioned::new(
                    Assert {
                        expr: Positioned::new(
                            ErrExpr(MissingParenthesis(Close)),
                            Position::new(1, 25),
                            Position::new(1, 25),
                        )
                    },
                    Position::new(1, 22),
                    Position::new(1, 26),
                )
            ],
            &[][..],
            6
        )),
        parse(&tokenize("
            assert (1 + 1;
        ").unwrap().0)
    );
}

#[test]
fn error_no_parenthesis_in_assert() {
    assert_eq!(
        Ok((
            vec![
                Positioned::new(
                    Assert {
                        expr: Positioned::new(
                            ErrExpr(MissingParenthesis(Open)),
                            Position::new(1, 19),
                            Position::new(1, 20),
                        )
                    },
                    Position::new(1, 12),
                    Position::new(1, 25),
                )
            ],
            &[][..],
            5
        )),
        parse(&tokenize("
            assert 1 + 1;
        ").unwrap().0)
    );
}

#[test]
fn error_no_type_annotation() {
    assert_eq!(
        Ok((
            vec![
                Positioned::new(
                    Declaration {
                        ident: "x".into(),
                        ty: TypeErr(NoTypeAnnotation),
                        value: None,
                    },
                    Position::new(1, 12),
                    Position::new(1, 18),
                )
            ],
            &[][..],
            3
        )),
        parse(&tokenize("
            var x;
        ").unwrap().0)
    );
}

#[test]
fn error_missing_type_in_annotation() {
    assert_eq!(
        Ok((
            vec![
                
            ],
            &[][..],
            6
        )),
        parse(&tokenize("
            var x: = 0;
        ").unwrap().0)
    );
}

#[test]
fn error_missing_range_in_for() {
    assert_eq!(
        Ok((
            vec![
                
            ],
            &[][..],
            8
        )),
        parse(&tokenize("
            for x in do; end for;
        ").unwrap().0)
    );
}

#[test]
fn error_missing_end_for_in_for() {
    assert_eq!(
        Ok((
            vec![
                
            ],
            &[][..],
            8
        )),
        parse(&tokenize("
            for x in 0..1 do;
        ").unwrap().0)
    );
}

#[test]
fn error_missing_for_in_for() {
    assert_eq!(
        Ok((
            vec![
                
            ],
            &[][..],
            10
        )),
        parse(&tokenize("
            for x in 0..1 do; end;
        ").unwrap().0)
    );
}

#[test]
fn error_missing_variable_in_for() {
    assert_eq!(
        Ok((
            vec![
                
            ],
            &[][..],
            10
        )),
        parse(&tokenize("
            for in 0..1 do; end for;
        ").unwrap().0)
    );
}

#[test]
fn error_missing_in_in_for() {
    assert_eq!(
        Ok((
            vec![
                
            ],
            &[][..],
            10
        )),
        parse(&tokenize("
            for x 0..1 do; end for;
        ").unwrap().0)
    );
}

#[test]
fn error_keyword_as_type() {
    assert_eq!(
        Ok((
            vec![
                Positioned::new(
                    Declaration {
                        ident: "x".into(),
                        ty: TypeErr(KeywordNotType(Keyword::For)),
                        value: None,
                    },
                    Position::new(1, 12),
                    Position::new(1, 23),
                )
            ],
            &[][..],
            5
        )),
        parse(&tokenize("
            var x: for;
        ").unwrap().0)
    );
}

#[test]
fn error_unknown_type() {
    assert_eq!(
        Ok((
            vec![
                Positioned::new(
                    Declaration {
                        ident: "x".into(),
                        ty: TypeErr(UnknownType("ImaginaryType".into())),
                        value: None,
                    },
                    Position::new(1, 12),
                    Position::new(1, 33),
                )
            ],
            &[][..],
            5
        )),
        parse(&tokenize("
            var x: ImaginaryType;
        ").unwrap().0)
    );
}