use lexer::tokenize;
use lexer::tokens::Keyword;
use lexer::tokens::Side::*;

use super::super::ast::Stmt::*;
use super::super::ast::Expr::*;
use super::super::ast::Opnd::*;
use super::super::ast::Type::*;
use super::super::ast::BinOp::*;
use super::super::ast::UnaOp::*;
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
                Print {
                    expr: Opnd(OpndErr(MissingEndParenthesis))
                }
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
                ErrStmt(MissingSemicolon),
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
                Print {
                    expr: Opnd(OpndErr(MissingEndParenthesis))
                }
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
                Print {
                    expr: Opnd(OpndErr(MissingEndParenthesis))
                }
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
                ErrStmt(MissingSemicolon)
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
                Assert {
                    expr: ErrExpr(MissingParenthesis(Open))
                }
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
                Assert {
                    expr: ErrExpr(MissingParenthesis(Close))
                }
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
                Declaration {
                    ident: "x".into(),
                    ty: TypeErr(NoTypeAnnotation),
                    value: None,
                }
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
                Declaration {
                    ident: "x".into(),
                    ty: TypeErr(KeywordNotType(Keyword::For)),
                    value: None,
                }
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
                Declaration {
                    ident: "x".into(),
                    ty: TypeErr(UnknownType("ImaginaryType".into())),
                    value: None,
                }
            ],
            &[][..],
            5
        )),
        parse(&tokenize("
            var x: ImaginaryType;
        ").unwrap().0)
    );
}