use lexer::tokenize;

use super::super::ast::Stmt::*;
use super::super::ast::Expr::*;
use super::super::ast::Opnd::*;
use super::super::ast::Type::*;
use super::super::ast::BinOp::*;
use super::super::ast::UnaOp::*;
use super::super::parse;

#[test]
fn error_invalid_expr() {
    assert_eq!(
        Ok((
            vec![
                Print {
                    expr: Opnd(Ident(String::from("X"))),
                }
            ],
            &[][..],
            16
        )),
        parse(&tokenize("
            print (1 + 1;
        ").unwrap().0)
    );
}