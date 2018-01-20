use lexer::tokenize;

use super::ast::Stmt::*;
use super::ast::Expr::*;
use super::ast::Opnd::*;
use super::ast::Type::*;
use super::ast::BinOp::*;
use super::ast::UnaOp::*;
use super::parse;

#[test]
fn complex_expr() {
    let bop = |lhs, op, rhs| Expr(Box::new(BinOper { lhs, op, rhs }));
    assert_eq!(
        Some((
            vec![
                Print {
                    expr: BinOper {
                        lhs: bop(
                                bop(
                                    bop(
                                        Int(4),
                                        Division,
                                        Int(2),
                                    ),
                                    Addition,
                                    Int(1),
                                ),
                                LessThan,
                                bop(
                                    Int(1),
                                    Substraction,
                                    bop(
                                        Int(2),
                                        Multiplication,
                                        Int(3),
                                    ),
                                ),
                            ),
                        op: Equality,
                        rhs: bop(
                            Int(0),
                            And,
                            Expr(Box::new(UnaOper {
                                op: Not,
                                rhs: Int(1)
                            })),
                        ),
                    },
                },
            ],
            &[][..]
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
        Some((
            vec![
                Declaration {
                    ident: String::from("X"),
                    ty: Integer,
                    value: Some(
                        BinOper {
                            lhs: Int(4),
                            op: Addition,
                            rhs: Expr(Box::new(BinOper {
                                lhs: Int(6),
                                op: Multiplication,
                                rhs: Int(2),
                            })),
                        }
                    )
                },
                Print {
                    expr: Opnd(Ident(String::from("X"))),
                }
            ],
            &[][..]
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
        Some((
            vec![
                Declaration {
                    ident: String::from("nTimes"),
                    ty: Integer,
                    value: Some(
                        Opnd(Int(0))
                    )
                },
                Print {
                    expr: Opnd(StrLit(String::from("How many times?"))),
                },
                Read {
                    ident: String::from("nTimes"),
                },
                Declaration {
                    ident: String::from("x"),
                    ty: Integer,
                    value: None,
                },
                Loop {
                    ident: String::from("x"),
                    from: Opnd(Int(0)),
                    to: BinOper {
                        lhs: Ident(String::from("nTimes")),
                        op: Substraction,
                        rhs: Int(1)
                    },
                    stmts: vec![
                        Print {
                            expr: Opnd(Ident(String::from("x")))
                        },
                        Print {
                            expr: Opnd(StrLit(String::from(" : Hello, World!\\n"))),
                        }
                    ]
                },
                Assert {
                    expr: BinOper {
                        lhs: Ident(String::from("x")),
                        op: Equality,
                        rhs: Ident(String::from("nTimes"))
                    }
                }
            ],
            &[][..]
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
        Some((
            vec![
                Print {
                    expr: Opnd(StrLit(String::from("Give a number"))),
                },
                Declaration {
                    ident: String::from("n"),
                    ty: Integer,
                    value: None,
                },
                Read {
                    ident: String::from("n")
                },
                Declaration {
                    ident: String::from("v"),
                    ty: Integer,
                    value: Some(
                        Opnd(Int(1))
                    )
                },
                Declaration {
                    ident: String::from("i"),
                    ty: Integer,
                    value: None,
                },
                Loop {
                    ident: String::from("i"),
                    from: Opnd(Int(1)),
                    to: Opnd(Ident(String::from("n"))),
                    stmts: vec![
                        Assignment {
                            ident: String::from("v"),
                            value: BinOper {
                                lhs: Ident(String::from("v")),
                                op: Multiplication,
                                rhs: Ident(String::from("i")),
                            }
                        }
                    ],
                },
                Print {
                    expr: Opnd(StrLit(String::from("The result is: "))),
                },
                Print {
                    expr: Opnd(Ident(String::from("v")))
                }
            ],
            &[][..]
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