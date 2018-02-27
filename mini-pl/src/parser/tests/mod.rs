use lexer::tokenize;
use lexer::tokens::Position;

use super::ast::Positioned;
use super::ast::Stmt::*;
use super::ast::Expr::*;
use super::ast::Opnd::*;
use super::ast::Type::*;
use super::ast::BinOp::*;
use super::ast::UnaOp::*;
use super::parse;

//mod error;

#[test]
fn complex_expr() {
    let bop = |lhs, op, rhs, from: (usize, usize), to: (usize, usize)|
        Expr(
            Box::new(Positioned::new(
                BinOper { lhs, op, rhs },
                Position::new(from.0, from.1),
                Position::new(to.0, to.1)
            ))
        );
    assert_eq!(
        Ok((
            vec![
                Positioned::new(Print {
                    expr: Positioned::new(BinOper {
                        lhs: bop(
                                bop(
                                    bop(
                                        Int(4.into()),
                                        Division,
                                        Int(2.into()),
                                        (4, 21), (6, 22)
                                    ),
                                    Addition,
                                    Int(1.into()),
                                    (3, 17), (7, 18)
                                ),
                                LessThan,
                                bop(
                                    Int(1.into()),
                                    Substraction,
                                    bop(
                                        Int(2.into()),
                                        Multiplication,
                                        Int(3.into()),
                                        (8, 23), (10, 22)
                                    ),
                                    (7, 19), (11, 18)
                                ),
                                (2, 13), (12, 14)
                            ),
                        op: Equality,
                        rhs: bop(
                            Int(0.into()),
                            And,
                            Expr(Box::new(Positioned::new(UnaOper {
                                op: Not,
                                rhs: Int(1.into())
                            }, Position::new(14, 19), Position::new(5, 18)))),
                            (13, 15), (16, 14)
                        ),
                    }, Position::new(2, 13), Position::new(16, 14)),
                }, Position::new(1, 13), Position::new(16, 14)),
            ],
            &[][..],
            32
        )),
        parse(&tokenize("
            print
            (
                (
                    (
                        4/2
                    )+1
                )<(
                    1-(
                        2*3
                    )
                )
            )=(
                0&(
                    !1
                )
            );
        ").unwrap().0)
    );
}

#[test]
fn example1_parses() {
    assert_eq!(
        Ok((
            vec![
                Positioned::new(Declaration {
                    ident: String::from("X"),
                    ty: Integer,
                    value: Some(
                        Positioned::new(BinOper {
                            lhs: Int(4.into()),
                            op: Addition,
                            rhs: Expr(Box::new(Positioned::new(BinOper {
                                lhs: Int(6.into()),
                                op: Multiplication,
                                rhs: Int(2.into()),
                            }, Position::new(1, 32), Position::new(1, 39)))),
                        }, Position::new(1, 28), Position::new(1, 39))
                    )
                }, Position::new(1, 13), Position::new(1, 40)),
                Positioned::new(Print {
                    expr: Positioned::new(
                        Opnd(Ident(String::from("X"))),
                        Position::new(2, 19),
                        Position::new(2, 20)
                    ),
                }, Position::new(2, 13), Position::new(2, 21))
            ],
            &[][..],
            16
        )),
        parse(&tokenize("
            var X : int := 4 + (6 * 2);
            print X;
        ").unwrap().0)
    );
}

#[test]
fn example2_parses() {
    assert_eq!(
        Ok((
            vec![
                Positioned::new(Declaration {
                    ident: String::from("nTimes"),
                    ty: Integer,
                    value: Some(Positioned::new(
                        Opnd(Int(0.into())),
                        Position::new(1, 33),
                        Position::new(1, 34)
                    ))
                }, Position::new(1, 13), Position::new(1, 35)),
                Positioned::new(Print {
                    expr: Positioned::new(
                        Opnd(StrLit(String::from("How many times?"))),
                        Position::new(2, 19),
                        Position::new(2, 36)
                    ),
                }, Position::new(2, 13), Position::new(2, 37)),
                Positioned::new(Read {
                    ident: String::from("nTimes"),
                }, Position::new(3, 13), Position::new(3, 25)),
                Positioned::new(Declaration {
                    ident: String::from("x"),
                    ty: Integer,
                    value: None,
                }, Position::new(4, 13), Position::new(4, 25)),
                Positioned::new(Loop {
                    ident: String::from("x"),
                    from: Positioned::new(
                        Opnd(Int(0.into())),
                        Position::new(6, 23),
                        Position::new(6, 24)
                    ),
                    to: Positioned::new(
                        BinOper {
                            lhs: Ident(String::from("nTimes")),
                            op: Substraction,
                            rhs: Int(1.into())
                        },
                        Position::new(6, 25),
                        Position::new(6, 33)
                    ),
                    stmts: vec![
                        Positioned::new(Print {
                            expr: Opnd(Ident(String::from("x")))
                        }, Position::new(6, 17), Position::new(6, 25)),
                        Positioned::new(Print {
                            expr: Opnd(StrLit(String::from(" : Hello, World!\n"))),
                        }, Position::new(7, 17), Position::new(7, 44))
                    ]
                }, Position::new(5, 13), Position::new(8, 21)),
                Positioned::new(Assert {
                    expr: BinOper {
                        lhs: Ident(String::from("x")),
                        op: Equality,
                        rhs: Ident(String::from("nTimes"))
                    }
                }, Position::new(4, 13), Position::new(4, 25))
            ],
            &[][..],
            43
        )),
        parse(&tokenize(r#"
            var nTimes : int := 0;
            print "How many times?";
            read nTimes;
            var x : int;
            for x in 0..nTimes-1 do 
                print x;
                print " : Hello, World!\n";
            end for;
            assert (x = nTimes);
        "#).unwrap().0)
    );
}

#[test]
fn example3_parses() {
    assert_eq!(
        Ok((
            vec![
                Positioned::new(Print {
                    expr: Opnd(StrLit(String::from("Give a number"))),
                }, Position::new(1, 13), Position::new(1, 35)),
                Positioned::new(Declaration {
                    ident: String::from("n"),
                    ty: Integer,
                    value: None,
                }, Position::new(2, 13), Position::new(2, 25)),
                Positioned::new(Read {
                    ident: String::from("n")
                }, Position::new(3, 13), Position::new(3, 20)),
                Positioned::new(Declaration {
                    ident: String::from("v"),
                    ty: Integer,
                    value: Some(
                        Opnd(Int(1.into()))
                    )
                }, Position::new(4, 13), Position::new(4, 30)),
                Positioned::new(Declaration {
                    ident: String::from("i"),
                    ty: Integer,
                    value: None,
                }, Position::new(5, 13), Position::new(5, 25)),
                Positioned::new(Loop {
                    ident: String::from("i"),
                    from: Opnd(Int(1.into())),
                    to: Opnd(Ident(String::from("n"))),
                    stmts: vec![
                        Positioned::new(Assignment {
                            ident: String::from("v"),
                            value: BinOper {
                                lhs: Ident(String::from("v")),
                                op: Multiplication,
                                rhs: Ident(String::from("i")),
                            }
                        }, Position::new(7, 17), Position::new(7, 28))
                    ],
                }, Position::new(6, 13), Position::new(8, 21)),
                Positioned::new(Print {
                    expr: Opnd(StrLit(String::from("The result is: "))),
                }, Position::new(9, 13), Position::new(9, 37)),
                Positioned::new(Print {
                    expr: Opnd(Ident(String::from("v")))
                }, Position::new(10, 13), Position::new(4, 21))
            ],
            &[][..],
            45
        )),
        parse(&tokenize(r#"
            print "Give a number"; 
            var n : int;
            read n;
            var v : int := 1;
            var i : int;
            for i in 1..n do
                v := v * i;
            end for;
            print "The result is: ";
            print v;
        "#).unwrap().0)
    );
}