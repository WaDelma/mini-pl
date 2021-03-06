use lexer::tokenize;
use util::{Positioned, Position};

use super::ast::Stmt::*;
use super::ast::Expr::*;
use super::ast::Opnd::*;
use super::ast::Type::*;
use super::ast::BinOp::*;
use super::ast::UnaOp::*;
use super::parse;

mod error;

#[test]
fn simple_expr() {
    assert_eq!(
        Ok((
            vec![
                Positioned::new(
                    Declaration {
                        ident: "x".into(),
                        ty: Integer,
                        value: Some(Positioned::new(
                            BinOper {
                                lhs: Positioned::new(
                                    Int(1.into()),
                                    Position::new(0, 14),
                                    Position::new(0, 15),
                                ),
                                op: Addition,
                                rhs: Positioned::new(
                                    Int(2.into()),
                                    Position::new(0, 18),
                                    Position::new(0, 19),
                                )
                            },
                            Position::new(0, 14),
                            Position::new(0, 19),
                        ))
                    },
                    Position::new(0, 0),
                    Position::new(0, 20),
                )
            ],
            &[][..],
            9
        )),
        parse(&tokenize("var x: int := 1 + 2;").unwrap().0)
    );
}

#[test]
fn complex_expr() {
    let bop = |lhs, (lhs_from, lhs_to): ((usize, usize), (usize, usize)), op, rhs, (rhs_from, rhs_to): ((usize, usize), (usize, usize)), (from, to): ((usize, usize), (usize, usize))|
        Expr(
            Box::new(Positioned::new(
                BinOper {
                    lhs: Positioned::new(
                        lhs,
                        Position::new(lhs_from.0, lhs_from.1),
                        Position::new(lhs_to.0, lhs_to.1),
                    ),
                    op,
                    rhs: Positioned::new(
                        rhs,
                        Position::new(rhs_from.0, rhs_from.1),
                        Position::new(rhs_to.0, rhs_to.1),
                    ),
                },
                Position::new(from.0, from.1),
                Position::new(to.0, to.1)
            ))
        );
    assert_eq!(
        Ok((
            vec![
                Positioned::new(
                    Print {
                        expr: Positioned::new(
                            BinOper {
                                lhs: Positioned::new(
                                    bop(
                                        bop(
                                            bop(
                                                Int(4.into()), ((5, 24), (5, 25)),
                                                Division,
                                                Int(2.into()), ((5, 26), (5, 27)),
                                                ((5, 24), (5, 27))
                                            ), ((4, 20), (6, 21)),
                                            Addition,
                                            Int(1.into()), ((6, 22), (6, 23)),
                                            ((4, 20), (6, 23))
                                        ), ((3, 16), (7, 17)),
                                        LessThan,
                                        bop(
                                            Int(1.into()), ((8, 20), (8, 21)),
                                            Substraction,
                                            bop(
                                                Int(2.into()), ((9, 24), (9, 25)),
                                                Multiplication,
                                                Int(3.into()), ((9, 26), (9, 27)),
                                                ((9, 24), (9, 27))
                                            ), ((8, 22), (10, 21)),
                                            ((8, 20), (10, 21))
                                        ), ((7, 18), (11, 17)),
                                        ((3, 16), (11, 17))
                                    ),
                                    Position::new(2, 12),
                                    Position::new(12, 13),
                                ),
                                op: Equality,
                                rhs: Positioned::new(
                                    bop(
                                        Int(0.into()), ((13, 16), (13, 17)),
                                        And,
                                        Expr(Box::new(Positioned::new(
                                            UnaOper {
                                                op: Not,
                                                rhs: Positioned::new(
                                                    Int(1.into()),
                                                    Position::new(14, 21),
                                                    Position::new(14, 22),
                                                )
                                            },
                                            Position::new(14, 20),
                                            Position::new(14, 22)
                                        ))), ((13, 18), (15, 17)),
                                        ((13, 16), (15, 17))
                                    ),
                                    Position::new(12, 14),
                                    Position::new(16, 13),
                                ),
                            },
                            Position::new(2, 12),
                            Position::new(16, 13)
                        ),
                    },
                    Position::new(1, 12),
                    Position::new(16, 14)
                ),
            ],
            &[][..],
            32
        )),
        parse(&tokenize(r#"
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
        "#).unwrap().0)
    );
}

#[test]
fn example1_parses() {
    assert_eq!(
        Ok((
            vec![
                Positioned::new(
                    Declaration {
                        ident: String::from("X"),
                        ty: Integer,
                        value: Some(
                            Positioned::new(
                                BinOper {
                                    lhs: Positioned::new(
                                        Int(4.into()),
                                        Position::new(1, 27),
                                        Position::new(1, 28)
                                    ),
                                    op: Addition,
                                    rhs: Positioned::new(
                                            Expr(Box::new(Positioned::new(
                                            BinOper {
                                                lhs: Positioned::new(
                                                    Int(6.into()),
                                                    Position::new(1, 32),
                                                    Position::new(1, 33)
                                                ),
                                                op: Multiplication,
                                                rhs: Positioned::new(
                                                    Int(2.into()),
                                                    Position::new(1, 36),
                                                    Position::new(1, 37)
                                                ),
                                            },
                                            Position::new(1, 32),
                                            Position::new(1, 37)
                                        ))),
                                        Position::new(1, 31),
                                        Position::new(1, 38)
                                    ),
                                },
                                Position::new(1, 27),
                                Position::new(1, 38)
                            )
                        )
                    },
                    Position::new(1, 12),
                    Position::new(1, 39)
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
                Positioned::new(
                    Declaration {
                        ident: String::from("nTimes"),
                        ty: Integer,
                        value: Some(Positioned::new(
                            Opnd(Positioned::new(
                                Int(0.into()),
                                Position::new(1, 32),
                                Position::new(1, 33)
                            )),
                            Position::new(1, 32),
                            Position::new(1, 33)
                        ))
                    },
                    Position::new(1, 12),
                    Position::new(1, 34)
                ),
                Positioned::new(
                    Print {
                        expr: Positioned::new(
                            Opnd(Positioned::new(
                                StrLit(String::from("How many times?")),
                                Position::new(2, 18),
                                Position::new(2, 35)
                            )),
                            Position::new(2, 18),
                            Position::new(2, 35)
                        ),
                    },
                    Position::new(2, 12),
                    Position::new(2, 36)
                ),
                Positioned::new(
                    Read {
                        ident: String::from("nTimes"),
                    },
                    Position::new(3, 12),
                    Position::new(3, 24)
                ),
                Positioned::new(
                    Declaration {
                        ident: String::from("x"),
                        ty: Integer,
                        value: None,
                    },
                    Position::new(4, 12),
                    Position::new(4, 24)
                ),
                Positioned::new(
                    Loop {
                        ident: String::from("x"),
                        from: Positioned::new(
                            Opnd(Positioned::new(
                                Int(0.into()),
                                Position::new(5, 21),
                                Position::new(5, 22),
                            )),
                            Position::new(5, 21),
                            Position::new(5, 22)
                        ),
                        to: Positioned::new(
                            BinOper {
                                lhs: Positioned::new(
                                    Ident(String::from("nTimes")),
                                    Position::new(5, 24),
                                    Position::new(5, 30),
                                ),
                                op: Substraction,
                                rhs: Positioned::new(
                                    Int(1.into()),
                                    Position::new(5, 31),
                                    Position::new(5, 32)
                                )
                            },
                            Position::new(5, 24),
                            Position::new(5, 32)
                        ),
                        stmts: vec![
                            Positioned::new(
                                Print {
                                    expr: Positioned::new(
                                        Opnd(Positioned::new(
                                            Ident(String::from("x")),
                                            Position::new(6, 22),
                                            Position::new(6, 23)
                                        )),
                                        Position::new(6, 22),
                                        Position::new(6, 23)
                                    )
                                },
                                Position::new(6, 16),
                                Position::new(6, 24)
                            ),
                            Positioned::new(
                                Print {
                                    expr: Positioned::new(
                                        Opnd(Positioned::new(
                                            StrLit(String::from(" : Hello, World!\n")),
                                            Position::new(7, 22),
                                            Position::new(7, 42)
                                        )),
                                        Position::new(7, 22),
                                        Position::new(7, 42)
                                    )
                                },
                                Position::new(7, 16),
                                Position::new(7, 43)
                            )
                        ]
                    },
                    Position::new(5, 12),
                    Position::new(8, 20)
                ),
                Positioned::new(
                    Assert {
                        expr: Positioned::new(
                            BinOper {
                                lhs: Positioned::new(
                                    Ident(String::from("x")),
                                    Position::new(9, 20),
                                    Position::new(9, 21)
                                ),
                                op: Equality,
                                rhs:  Positioned::new(
                                    Ident(String::from("nTimes")),
                                    Position::new(9, 24),
                                    Position::new(9, 30)
                                ),
                            },
                            Position::new(9, 20),
                            Position::new(9, 30)
                        )
                    },
                    Position::new(9, 12),
                    Position::new(9, 32)
                )
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
                Positioned::new(
                    Print {
                        expr: Positioned::new(
                            Opnd(Positioned::new(
                                StrLit(String::from("Give a number")),
                                Position::new(1, 18),
                                Position::new(1, 33)
                            )),
                            Position::new(1, 18),
                            Position::new(1, 33)
                        )
                    },
                    Position::new(1, 12),
                    Position::new(1, 34)
                ),
                Positioned::new(
                    Declaration {
                        ident: String::from("n"),
                        ty: Integer,
                        value: None,
                    },
                    Position::new(2, 12),
                    Position::new(2, 24)
                ),
                Positioned::new(
                    Read {
                        ident: String::from("n")
                    },
                    Position::new(3, 12),
                    Position::new(3, 19)
                ),
                Positioned::new(
                    Declaration {
                        ident: String::from("v"),
                        ty: Integer,
                        value: Some(Positioned::new(
                            Opnd(Positioned::new(
                                Int(1.into()),
                                Position::new(4, 27),
                                Position::new(4, 28)
                            )),
                            Position::new(4, 27),
                            Position::new(4, 28)
                        ))
                    },
                    Position::new(4, 12),
                    Position::new(4, 29)
                ),
                Positioned::new(
                    Declaration {
                        ident: String::from("i"),
                        ty: Integer,
                        value: None,
                    },
                    Position::new(5, 12),
                    Position::new(5, 24)
                ),
                Positioned::new(
                    Loop {
                        ident: String::from("i"),
                        from: Positioned::new(
                            Opnd(Positioned::new(
                                Int(1.into()),
                                Position::new(6, 21),
                                Position::new(6, 22)
                            )),
                            Position::new(6, 21),
                            Position::new(6, 22)
                        ),
                        to: Positioned::new(
                            Opnd(Positioned::new(
                                Ident(String::from("n")),
                                Position::new(6, 24),
                                Position::new(6, 25)
                                
                            )),
                            Position::new(6, 24),
                            Position::new(6, 25)
                        ),
                        stmts: vec![
                            Positioned::new(
                                Assignment {
                                    ident: String::from("v"),
                                    value: Positioned::new(
                                        BinOper {
                                            lhs: Positioned::new(
                                                Ident(String::from("v")),
                                                Position::new(7, 21),
                                                Position::new(7, 22)
                                            ),
                                            op: Multiplication,
                                            rhs: Positioned::new(
                                                Ident(String::from("i")),
                                                Position::new(7, 25),
                                                Position::new(7, 26)
                                            )
                                        },
                                        Position::new(7, 21),
                                        Position::new(7, 26)
                                    ),
                                },
                                Position::new(7, 16),
                                Position::new(7, 27)
                            )
                        ],
                    },
                    Position::new(6, 12),
                    Position::new(8, 20)
                ),
                Positioned::new(
                    Print {
                        expr: Positioned::new(
                            Opnd(Positioned::new(
                                StrLit(String::from("The result is: ")),
                                Position::new(9, 18),
                                Position::new(9, 35)
                            )),
                            Position::new(9, 18),
                            Position::new(9, 35)
                        ),
                    },
                    Position::new(9, 12),
                    Position::new(9, 36)
                ),
                Positioned::new(
                    Print {
                        expr: Positioned::new(
                            Opnd(Positioned::new(
                                Ident(String::from("v")),
                                Position::new(10, 18),
                                Position::new(10, 19)
                            )),
                            Position::new(10, 18),
                            Position::new(10, 19)
                        ),
                    },
                    Position::new(10, 12),
                    Position::new(10, 20)
                )
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