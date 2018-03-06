use parsco::common::{Err2, Err3};
use parsco::*;

use Ident;
use lexer::tokens::{Tok, Position};
use lexer::tokens::Token::*;
use lexer::tokens::Punctuation::*;
use lexer::tokens::Side::*;
use lexer::tokens::Keyword::*;
use lexer::tokens::Operator::*;
use lexer::tokens::Literal::*;
use self::ast::{Positioned, Stmt, Expr, Type, Opnd, BinOp, UnaOp, ParseError};
use self::ast::OpndError::*;
use self::ast::ParseError::*;
use self::ast::TypeError::*;
use self::ast::ExprError::*;

type Result<'a, T> = ::parsco::Result<&'a [Tok], T, ParseError>;

pub mod ast;
#[cfg(test)]
mod tests;

pub fn parse(ts: &[Tok]) -> Result<Vec<Positioned<Stmt>>> {
    many1(
        map(
            flat_map_err(
                (
                    fun(stmt),
                    sym(Punctuation(Semicolon))
                ),
                |err, rest, pos| match err {
                    Err2::V1(e) => Err((e, pos)),
                    Err2::V2((stmt, _)) => {
                        let statement = Positioned::new(
                            Stmt::ErrStmt(MissingSemicolon),
                            stmt.to.clone(),
                            stmt.to
                        );
                        let pos = pos.end - 1;
                        let (from, to) = (statement.from.clone(), statement.to.clone());
                        Ok((
                            (
                                statement,
                                Tok::new(Punctuation(Semicolon), from, to)
                            ),
                            &rest[pos..],
                            pos
                        ))
                    },
                }
            ),
            |(mut statement, semicolon), _, _| {
                statement.to = semicolon.to;
                statement
            }
        )
    ).parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
}

pub fn stmt(ts: &[Tok]) -> Result<Positioned<Stmt>> {
    (alt()
        | map(
            (
                (sym(Keyword(Var)), fun(ident)),
                flat_map_err(
                    preceded(
                        sym(Punctuation(Colon)), fun(ty)
                    ),
                    |err, rest, pos| match err {
                        Err2::V1(Err2::V1(t)) => {
                            let pos = pos.end - 1;
                            Ok((
                                Positioned::new(
                                    Type::TypeErr(NoTypeAnnotation),
                                    t.from,
                                    t.to
                                ),
                                &rest[pos..],
                                pos
                            ))
                        },
                        Err2::V1(_) => {
                            let pos = pos.end - 1;
                            Ok((
                                Positioned::new(
                                    Type::TypeErr(NoTypeAnnotation),
                                    Position::new(0, 0),
                                    Position::new(0, 0)
                                ),
                                &rest[pos..],
                                pos
                            ))
                        }
                        Err2::V2(e) => Err((e, pos)),
                    }
                ),
                opt(preceded(
                    sym(Operator(Assignment)), fun(expr)
                ))
            ),
            |((var, ident), ty, value), _, _| {
                let to = value.clone().map(|expr| expr.to)
                    .unwrap_or(ty.to);
                Positioned::new(
                    Stmt::Declaration {
                        ident: ident.data,
                        ty: ty.data,
                        value
                    },
                    var.from,
                    to
                )
            }
        )
        | map(
            (
                fun(ident),
                preceded(
                    sym(Operator(Assignment)),
                    fun(expr)
                )
            ),
            |(ident, value), _, _| {
                let (from, to) = (ident.from, value.to.clone());
                Positioned::new(
                    Stmt::Assignment {
                        ident: ident.data,
                        value
                    },
                    from,
                    to
                )
            }
        )
        | map(
            (
                (
                    sym(Keyword(For)),
                    fun(ident),
                    sym(Keyword(In))
                ),
                fun(expr),
                delimited(
                    sym(Operator(Range)),
                    fun(expr),
                    sym(Keyword(Do))
                ),
                (
                    fun(parse),
                    (sym(Keyword(End)), sym(Keyword(For)))
                )
            ),
            |((for_, ident, _), range_from, range_to, (stmts, (_, end))), _, _| {
                let (from, to) = (for_.from, end.to);
                Positioned::new(
                    Stmt::Loop {
                        ident: ident.data,
                        from: range_from,
                        to: range_to,
                        stmts
                    },
                    from,
                    to
                )
            }
        )
        | map(
            (
                sym(Keyword(Read)),
                fun(ident)
            ),
            |(read, ident), _, _| Positioned::new(
                Stmt::Read {
                    ident: ident.data
                },
                read.from,
                ident.to
            )
        )
        | map(
            (
                sym(Keyword(Print)),
                fun(expr)
            ),
            |(print, expr), _, _| {
                let (from, to) = (print.from, expr.to.clone());
                Positioned::new(
                    Stmt::Print {
                        expr
                    },
                    from,
                    to,
                )
            }
        )
        | map(
            (
                sym(Keyword(Assert)),
                flat_map_err(
                    delimited(
                        sym(Punctuation(Parenthesis(Open))),
                        fun(expr),
                        sym(Punctuation(Parenthesis(Close))),
                    ),
                    |err, rest, pos| {
                        use self::Err3::*;
                        let pos_before = pos.end - 1;
                        match err {
                            V1(_) => {
                                let (from, to) = rest.first()
                                    .map(|l| (l.from.clone(), l.to.clone()))
                                    .unwrap_or((
                                        Position::new(0, 0),
                                        Position::new(0, 0)
                                    ));
                                    
                                let pos = pos_before + take_until::<_, &[Tok]>(sym(Punctuation(Semicolon)))
                                    .parse(rest)
                                    .map(|(_, _, pos2)| pos2 - 1)
                                    .unwrap_or(0);
                                Ok((
                                    Positioned::new(
                                        Expr::ErrExpr(MissingParenthesis(Open)),
                                        from,
                                        to
                                    ),
                                    &rest[pos..],
                                    pos
                                ))
                            },
                            V2(e) => Err((e, pos)),
                            V3(_) => Ok((
                                Positioned::new(
                                    Expr::ErrExpr(MissingParenthesis(Close)),
                                    Position::new(0, 0),
                                    Position::new(0, 0)
                                ),
                                &rest[pos_before..],
                                pos_before
                            )),
                        }
                    }
                )
            ),
            |(assert, expr), _, _| {
                let (from, to) = (assert.from, expr.to.clone());
                Positioned::new(
                    Stmt::Assert {
                        expr
                    },
                    from,
                    to,
                )
            }
        )
    ).parse(ts)
        .map_err(|(err, pos)| (
            match err {
                Err2::V2((_, (_, e))) => FromErr::from(e),
                _ => ParseError::Unknown,
            },
            pos
        ))
}

pub fn expr(ts: &[Tok]) -> Result<Positioned<Expr>> {
    (alt()
        | map(
            (fun(opnd), fun(binop), fun(opnd)),
            |(lhs, op, rhs), _, _| {
                let (from, to) = (lhs.from.clone(), rhs.to.clone());
                Positioned::new(
                    Expr::BinOper {
                        lhs,
                        op,
                        rhs
                    },
                    from,
                    to
                )
            }
        )
        | map(
            (fun(unaop), fun(opnd)),
            |(op, rhs), _, _| {
                let (from, to) = (op.from, rhs.to.clone());
                Positioned::new(
                    Expr::UnaOper {
                        op: op.data,
                        rhs
                    },
                    from,
                    to
                )
            }
        )
        | map(
            fun(opnd),
            |o, _, _|  {
                let (from, to) = (o.from.clone(), o.to.clone());
                Positioned::new(
                    Expr::Opnd(o),
                    from,
                    to
                )
            }
        )
    ).parse(ts)
}

pub fn opnd(ts: &[Tok]) -> Result<Positioned<Opnd>> {
    (alt()
        | fun(int)
        | fun(string)
        | map(fun(ident), |i, _, _| Positioned::new(
                Opnd::Ident(i.data),
                i.from,
                i.to
            )
        )
        | flat_map_err(
            map((
                sym(Punctuation(Parenthesis(Open))),
                fun(expr),
                sym(Punctuation(Parenthesis(Close)))
            ),
            |(opening, expr, ending), _, _| Positioned::new(
                Opnd::Expr(Box::new(expr)),
                opening.from,
                ending.to,
            )),
            |err, rest, pos| match err {
                // TODO: We want to return Err here, but what is its value?
                Err3::V1(_) => Err((Unknown, pos)),
                Err3::V2((_, e)) => Err((e, pos)),
                Err3::V3((_, expr, _)) => Ok((
                    Positioned::new(
                        Opnd::OpndErr(MissingEndParenthesis),
                        expr.to.clone(),
                        expr.to
                    ),
                    &rest[(pos.end - 1)..],
                    pos.end - 1
                ))
            }
        )
        | map(
            constant(()),
            |_, rest: &[Tok], _| {
                let (from, to) = rest.last()
                    .map(|l| (l.from.clone(), l.to.clone()))
                    .unwrap_or((
                        Position::new(0, 0),
                        Position::new(0, 0)
                    ));
                Positioned::new(
                    Opnd::OpndErr(InvalidOperand),
                    from,
                    to
                )
            }
        )
    ).parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
}

pub fn ident(ts: &[Tok]) -> Result<Positioned<Ident>> {
    fst().parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
        .and_then(|(t, s, p)| if let Identifier(ref tok) = *t.sym() {
            Ok((
                Positioned::new(
                    tok.clone(),
                    t.from.clone(),
                    t.to.clone()
                ),
                s,
                p
            ))
        } else {
            Err((Unknown, 0..p))
        })
}

pub fn ty(ts: &[Tok]) -> Result<Positioned<Type>> {
    fst().parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
        .and_then(|(t, s, p)| {
            let (from, to) = (t.from.clone(), t.to.clone());
            if let Keyword(ref tok) = *t.sym() {
                Ok((
                    Positioned::new(
                        match *tok {
                            Int => Type::Integer,
                            Bool => Type::Bool,
                            Str => Type::Str,
                            ref k => Type::TypeErr(KeywordNotType(k.clone())),
                        },
                        from,
                        to
                    ),
                    s,
                    p
                ))
            } else {
                match *t.sym() {
                    Identifier(ref i) => Ok((
                        Positioned::new(
                            Type::TypeErr(UnknownType(i.clone())),
                            from,
                            to
                        ),
                        s,
                        p
                    )),
                    _ => Err((Unknown, 0..p))   
                }
            }
        })
}

pub fn int(ts: &[Tok]) -> Result<Positioned<Opnd>> {
    fst().parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
        .and_then(|(t, s, p)| if let Literal(Integer(ref i)) = *t.sym() {
            Ok((
                Positioned::new(
                    Opnd::Int(i.clone()),
                    t.from.clone(),
                    t.to.clone()
                ),
                s,
                p
            ))
        } else {
            Err((Unknown, 0..p))
        })
}

pub fn string(ts: &[Tok]) -> Result<Positioned<Opnd>> {
    fst().parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
        .and_then(|(t, s, p)| if let Literal(StringLit(ref tok)) = *t.sym() {
            Ok((
                Positioned::new(
                    Opnd::StrLit(tok.clone()),
                    t.from.clone(),
                    t.to.clone()
                ),
                s,
                p
            ))
        } else {
            Err((Unknown, 0..p))
        })
}

pub fn binop(ts: &[Tok]) -> Result<BinOp> {
    fst().parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
        .and_then(|(t, s, p)| if let Operator(ref o) = *t.sym() {
            Ok((BinOp::from_oper(o).ok_or((Unknown, 0..p))?, s, p))
        } else {
            Err((Unknown, 0..p))
        })
}

pub fn unaop(ts: &[Tok]) -> Result<Positioned<UnaOp>> {
    fst().parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
        .and_then(|(t, s, p)| if let Operator(ref o) = *t.sym() {
            Ok((
                Positioned::new(
                    UnaOp::from_oper(o)
                        .ok_or((Unknown, 0..p))?,
                    t.from.clone(),
                    t.to.clone()
                ),
                s,
                p
            ))
        } else {
            Err((Unknown, 0..p))
        })
}