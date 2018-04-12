use lexer::tokenize;
use util::{Positioned, Position};

use super::{
    parse,
    ast::{
        Program,
        Stmt::*,
        Expr::*,
        Opnd::*,
        Type::*,
        BinOp::*,
        UnaOp::*
    },
};

// mod error;

#[test]
fn simple_expr() {
    assert_eq!(
        Ok((
            Positioned::new(
                Program {
                    name: Positioned::new(
                        "p".into(),
                        Position::new(0, 8),
                        Position::new(0, 9)
                    ),
                    functions: vec![],
                    stmts: Positioned::new(
                        Block {
                            stmts: vec![
                                Positioned::new(
                                    Declaration {
                                        idents: vec![
                                            Positioned::new(
                                                "x".into(),
                                                Position::new(0, 21),
                                                Position::new(0, 22),
                                            ),
                                            Positioned::new(
                                                "y".into(),
                                                Position::new(0, 24),
                                                Position::new(0, 25),
                                            ),
                                        ],
                                        ty: Integer,
                                    },
                                    Position::new(0, 17),
                                    Position::new(0, 34),
                                )
                            ]
                        },
                        Position::new(0, 11),
                        Position::new(0, 38),
                    )
                },
                Position::new(0, 0),
                Position::new(0, 39)
            ),
            &[][..],
            12
        )),
        parse(&tokenize("program p; begin var x, y: integer end.").unwrap().0)
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
            Positioned::new(
                Program {
                    name: Positioned::new(
                        "p".into(),
                        Position::new(0, 8),
                        Position::new(0, 9)
                    ),
                    functions: vec![],
                    stmts: Positioned::new(
                        Block {
                            stmts: vec![
                                Positioned::new(
                                    Assignment {
                                        ident: "x".into(),
                                        value: Positioned::new(
                                            BinOper {
                                                lhs: Positioned::new(
                                                    bop(
                                                        bop(
                                                            bop(
                                                                Int(4.into()), ((4, 24), (4, 25)),
                                                                Division,
                                                                Int(2.into()), ((4, 26), (4, 27)),
                                                                ((4, 24), (4, 27))
                                                            ), ((3, 20), (5, 21)),
                                                            Addition,
                                                            Int(1.into()), ((5, 22), (5, 23)),
                                                            ((3, 20), (5, 23))
                                                        ), ((2, 16), (6, 17)),
                                                        LessThan,
                                                        bop(
                                                            Int(1.into()), ((7, 20), (7, 21)),
                                                            Substraction,
                                                            bop(
                                                                Int(2.into()), ((8, 24), (8, 25)),
                                                                Multiplication,
                                                                Int(3.into()), ((8, 26), (8, 27)),
                                                                ((8, 24), (8, 27))
                                                            ), ((7, 22), (9, 21)),
                                                            ((7, 20), (9, 21))
                                                        ), ((6, 18), (10, 17)),
                                                        ((2, 16), (10, 17))
                                                    ),
                                                    Position::new(1, 17),
                                                    Position::new(11, 13),
                                                ),
                                                op: Equality,
                                                rhs: Positioned::new(
                                                    bop(
                                                        Int(0.into()), ((12, 16), (12, 17)),
                                                        And,
                                                        Expr(Box::new(Positioned::new(
                                                            UnaOper {
                                                                op: Not,
                                                                rhs: Positioned::new(
                                                                    Int(1.into()),
                                                                    Position::new(13, 24),
                                                                    Position::new(13, 25),
                                                                )
                                                            },
                                                            Position::new(13, 20),
                                                            Position::new(13, 25)
                                                        ))), ((12, 22), (14, 17)),
                                                        ((12, 16), (14, 17))
                                                    ),
                                                    Position::new(11, 14),
                                                    Position::new(15, 13),
                                                ),
                                            },
                                            Position::new(1, 17),
                                            Position::new(15, 13)
                                        ),
                                    },
                                    Position::new(1, 12),
                                    Position::new(15, 13)
                                ),
                            ],   
                        },
                        Position::new(0, 11),
                        Position::new(15, 17)
                    )
                },
                Position::new(0, 0),
                Position::new(15, 18)
            ),
            &[][..],
            38
        )),
        parse(&tokenize(r#"program p; begin
            x := (
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
                0 and (
                    not 1
                )
            ) end.
        "#).unwrap().0)
    );
}

// #[test]
// fn example1_parses() {
//     assert_eq!(
//         Ok((
//             vec![
//                 Positioned::new(
//                     Declaration {
//                         ident: String::from("X"),
//                         ty: Integer,
//                         value: Some(
//                             Positioned::new(
//                                 BinOper {
//                                     lhs: Positioned::new(
//                                         Int(4.into()),
//                                         Position::new(1, 27),
//                                         Position::new(1, 28)
//                                     ),
//                                     op: Addition,
//                                     rhs: Positioned::new(
//                                             Expr(Box::new(Positioned::new(
//                                             BinOper {
//                                                 lhs: Positioned::new(
//                                                     Int(6.into()),
//                                                     Position::new(1, 32),
//                                                     Position::new(1, 33)
//                                                 ),
//                                                 op: Multiplication,
//                                                 rhs: Positioned::new(
//                                                     Int(2.into()),
//                                                     Position::new(1, 36),
//                                                     Position::new(1, 37)
//                                                 ),
//                                             },
//                                             Position::new(1, 32),
//                                             Position::new(1, 37)
//                                         ))),
//                                         Position::new(1, 31),
//                                         Position::new(1, 38)
//                                     ),
//                                 },
//                                 Position::new(1, 27),
//                                 Position::new(1, 38)
//                             )
//                         )
//                     },
//                     Position::new(1, 12),
//                     Position::new(1, 39)
//                 ),
//                 Positioned::new(
//                     Print {
//                         expr: Positioned::new(
//                             Opnd(Positioned::new(
//                                 Ident(String::from("X")),
//                                 Position::new(2, 18),
//                                 Position::new(2, 19)
//                             )),
//                             Position::new(2, 18),
//                             Position::new(2, 19)
//                         ),
//                     },
//                     Position::new(2, 12),
//                     Position::new(2, 20)
//                 )
//             ],
//             &[][..],
//             16
//         )),
//         parse(&tokenize("
//             var X : int := 4 + (6 * 2);
//             print X;
//         ").unwrap().0)
//     );
// }

// #[test]
// fn example2_parses() {
//     assert_eq!(
//         Ok((
//             vec![
//                 Positioned::new(
//                     Declaration {
//                         ident: String::from("nTimes"),
//                         ty: Integer,
//                         value: Some(Positioned::new(
//                             Opnd(Positioned::new(
//                                 Int(0.into()),
//                                 Position::new(1, 32),
//                                 Position::new(1, 33)
//                             )),
//                             Position::new(1, 32),
//                             Position::new(1, 33)
//                         ))
//                     },
//                     Position::new(1, 12),
//                     Position::new(1, 34)
//                 ),
//                 Positioned::new(
//                     Print {
//                         expr: Positioned::new(
//                             Opnd(Positioned::new(
//                                 StrLit(String::from("How many times?")),
//                                 Position::new(2, 18),
//                                 Position::new(2, 35)
//                             )),
//                             Position::new(2, 18),
//                             Position::new(2, 35)
//                         ),
//                     },
//                     Position::new(2, 12),
//                     Position::new(2, 36)
//                 ),
//                 Positioned::new(
//                     Read {
//                         ident: String::from("nTimes"),
//                     },
//                     Position::new(3, 12),
//                     Position::new(3, 24)
//                 ),
//                 Positioned::new(
//                     Declaration {
//                         ident: String::from("x"),
//                         ty: Integer,
//                         value: None,
//                     },
//                     Position::new(4, 12),
//                     Position::new(4, 24)
//                 ),
//                 Positioned::new(
//                     Loop {
//                         ident: String::from("x"),
//                         from: Positioned::new(
//                             Opnd(Positioned::new(
//                                 Int(0.into()),
//                                 Position::new(5, 21),
//                                 Position::new(5, 22),
//                             )),
//                             Position::new(5, 21),
//                             Position::new(5, 22)
//                         ),
//                         to: Positioned::new(
//                             BinOper {
//                                 lhs: Positioned::new(
//                                     Ident(String::from("nTimes")),
//                                     Position::new(5, 24),
//                                     Position::new(5, 30),
//                                 ),
//                                 op: Substraction,
//                                 rhs: Positioned::new(
//                                     Int(1.into()),
//                                     Position::new(5, 31),
//                                     Position::new(5, 32)
//                                 )
//                             },
//                             Position::new(5, 24),
//                             Position::new(5, 32)
//                         ),
//                         stmts: vec![
//                             Positioned::new(
//                                 Print {
//                                     expr: Positioned::new(
//                                         Opnd(Positioned::new(
//                                             Ident(String::from("x")),
//                                             Position::new(6, 22),
//                                             Position::new(6, 23)
//                                         )),
//                                         Position::new(6, 22),
//                                         Position::new(6, 23)
//                                     )
//                                 },
//                                 Position::new(6, 16),
//                                 Position::new(6, 24)
//                             ),
//                             Positioned::new(
//                                 Print {
//                                     expr: Positioned::new(
//                                         Opnd(Positioned::new(
//                                             StrLit(String::from(" : Hello, World!\n")),
//                                             Position::new(7, 22),
//                                             Position::new(7, 42)
//                                         )),
//                                         Position::new(7, 22),
//                                         Position::new(7, 42)
//                                     )
//                                 },
//                                 Position::new(7, 16),
//                                 Position::new(7, 43)
//                             )
//                         ]
//                     },
//                     Position::new(5, 12),
//                     Position::new(8, 20)
//                 ),
//                 Positioned::new(
//                     Assert {
//                         expr: Positioned::new(
//                             BinOper {
//                                 lhs: Positioned::new(
//                                     Ident(String::from("x")),
//                                     Position::new(9, 20),
//                                     Position::new(9, 21)
//                                 ),
//                                 op: Equality,
//                                 rhs:  Positioned::new(
//                                     Ident(String::from("nTimes")),
//                                     Position::new(9, 24),
//                                     Position::new(9, 30)
//                                 ),
//                             },
//                             Position::new(9, 20),
//                             Position::new(9, 30)
//                         )
//                     },
//                     Position::new(9, 12),
//                     Position::new(9, 32)
//                 )
//             ],
//             &[][..],
//             43
//         )),
//         parse(&tokenize(r#"
//             var nTimes : int := 0;
//             print "How many times?";
//             read nTimes;
//             var x : int;
//             for x in 0..nTimes-1 do 
//                 print x;
//                 print " : Hello, World!\n";
//             end for;
//             assert (x = nTimes);
//         "#).unwrap().0)
//     );
// }

// #[test]
// fn example3_parses() {
//     assert_eq!(
//         Ok((
//             vec![
//                 Positioned::new(
//                     Print {
//                         expr: Positioned::new(
//                             Opnd(Positioned::new(
//                                 StrLit(String::from("Give a number")),
//                                 Position::new(1, 18),
//                                 Position::new(1, 33)
//                             )),
//                             Position::new(1, 18),
//                             Position::new(1, 33)
//                         )
//                     },
//                     Position::new(1, 12),
//                     Position::new(1, 34)
//                 ),
//                 Positioned::new(
//                     Declaration {
//                         ident: String::from("n"),
//                         ty: Integer,
//                         value: None,
//                     },
//                     Position::new(2, 12),
//                     Position::new(2, 24)
//                 ),
//                 Positioned::new(
//                     Read {
//                         ident: String::from("n")
//                     },
//                     Position::new(3, 12),
//                     Position::new(3, 19)
//                 ),
//                 Positioned::new(
//                     Declaration {
//                         ident: String::from("v"),
//                         ty: Integer,
//                         value: Some(Positioned::new(
//                             Opnd(Positioned::new(
//                                 Int(1.into()),
//                                 Position::new(4, 27),
//                                 Position::new(4, 28)
//                             )),
//                             Position::new(4, 27),
//                             Position::new(4, 28)
//                         ))
//                     },
//                     Position::new(4, 12),
//                     Position::new(4, 29)
//                 ),
//                 Positioned::new(
//                     Declaration {
//                         ident: String::from("i"),
//                         ty: Integer,
//                         value: None,
//                     },
//                     Position::new(5, 12),
//                     Position::new(5, 24)
//                 ),
//                 Positioned::new(
//                     Loop {
//                         ident: String::from("i"),
//                         from: Positioned::new(
//                             Opnd(Positioned::new(
//                                 Int(1.into()),
//                                 Position::new(6, 21),
//                                 Position::new(6, 22)
//                             )),
//                             Position::new(6, 21),
//                             Position::new(6, 22)
//                         ),
//                         to: Positioned::new(
//                             Opnd(Positioned::new(
//                                 Ident(String::from("n")),
//                                 Position::new(6, 24),
//                                 Position::new(6, 25)
                                
//                             )),
//                             Position::new(6, 24),
//                             Position::new(6, 25)
//                         ),
//                         stmts: vec![
//                             Positioned::new(
//                                 Assignment {
//                                     ident: String::from("v"),
//                                     value: Positioned::new(
//                                         BinOper {
//                                             lhs: Positioned::new(
//                                                 Ident(String::from("v")),
//                                                 Position::new(7, 21),
//                                                 Position::new(7, 22)
//                                             ),
//                                             op: Multiplication,
//                                             rhs: Positioned::new(
//                                                 Ident(String::from("i")),
//                                                 Position::new(7, 25),
//                                                 Position::new(7, 26)
//                                             )
//                                         },
//                                         Position::new(7, 21),
//                                         Position::new(7, 26)
//                                     ),
//                                 },
//                                 Position::new(7, 16),
//                                 Position::new(7, 27)
//                             )
//                         ],
//                     },
//                     Position::new(6, 12),
//                     Position::new(8, 20)
//                 ),
//                 Positioned::new(
//                     Print {
//                         expr: Positioned::new(
//                             Opnd(Positioned::new(
//                                 StrLit(String::from("The result is: ")),
//                                 Position::new(9, 18),
//                                 Position::new(9, 35)
//                             )),
//                             Position::new(9, 18),
//                             Position::new(9, 35)
//                         ),
//                     },
//                     Position::new(9, 12),
//                     Position::new(9, 36)
//                 ),
//                 Positioned::new(
//                     Print {
//                         expr: Positioned::new(
//                             Opnd(Positioned::new(
//                                 Ident(String::from("v")),
//                                 Position::new(10, 18),
//                                 Position::new(10, 19)
//                             )),
//                             Position::new(10, 18),
//                             Position::new(10, 19)
//                         ),
//                     },
//                     Position::new(10, 12),
//                     Position::new(10, 20)
//                 )
//             ],
//             &[][..],
//             45
//         )),
//         parse(&tokenize(r#"
//             print "Give a number"; 
//             var n : int;
//             read n;
//             var v : int := 1;
//             var i : int;
//             for i in 1..n do
//                 v := v * i;
//             end for;
//             print "The result is: ";
//             print v;
//         "#).unwrap().0)
//     );
// }