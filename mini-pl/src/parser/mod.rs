use parsco::common::{Err2, Err3, Void};
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
                    Err2::V2(s) => {
                        let statement = match s {
                            Err2::V1(s) => Positioned::new(
                                Stmt::ErrStmt(MissingSemicolon),
                                s.from,
                                s.to
                            ),
                            Err2::V2(_) => Positioned::new(
                                Stmt::ErrStmt(MissingSemicolon),
                                Position::new(0, 0),
                                Position::new(0, 0)
                            ),
                        };
                        let pos = pos.end - 1;
                        let (from, to) = (statement.from.clone(), statement.to.clone());
                        Ok(((statement, Tok::new(Punctuation(Semicolon), from, to)), &rest[pos..], pos))
                    },
                }
            ),
            |(mut stmt, semi), _, _| {
                stmt.to = semi.to;
                stmt
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
                let to = if let Some(ref expr) = value {
                    expr.to.clone()
                } else {
                    ty.to
                };
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
                delimited(
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
            |(ident, range_from, range_to, (stmts, (_, end))), _, _| {
                let (from, to) = (ident.from, end.to);
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
                                match take_until::<_, &[Tok]>(sym(Punctuation(Semicolon))).parse(rest) {
                                    Ok(((_, tok), _, pos2)) => {
                                        let pos = pos_before + pos2 - 1;
                                        Ok((
                                            Positioned::new(
                                                Expr::ErrExpr(MissingParenthesis(Open)),
                                                Position::new(0, 0),
                                                tok.to
                                            ),
                                            &rest[pos..],
                                            pos
                                        ))
                                    },
                                    Err(_) => Ok((
                                        Positioned::new(
                                            Expr::ErrExpr(MissingParenthesis(Open)),
                                            Position::new(0, 0),
                                            Position::new(0, 0)
                                        ),
                                        &rest[pos_before..],
                                        pos_before
                                    ))
                                }
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
                Err2::V2(e) => FromErr::from(e),
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
        | map((
            sym(Punctuation(Parenthesis(Open))),
            flat_map_err(
                (
                    map(
                        fun(expr),
                        |expr, _, _| Opnd::Expr(Box::new(expr))
                    ),
                    sym(Punctuation(Parenthesis(Close)))
                ),
                |err, rest, pos| match err {
                    Err2::V1(e) => Err((e, pos)),
                    Err2::V2(_) => Ok((
                        // TODO: The Tok in the second parameter is useless...
                        (Opnd::OpndErr(MissingEndParenthesis), Tok::new(Punctuation(Parenthesis(Close)), Position::new(0, 0), Position::new(0, 0))), 
                        &rest[(pos.end - 1)..],
                        pos.end - 1
                    )),
                }
            )
        ), |(opening, (expr, ending)), _, _| Positioned::new(
            expr,
            opening.from,
            ending.to,
        ))
        | constant(Positioned::new(
            Opnd::OpndErr(InvalidOperand),
            Position::new(0, 0),
            Position::new(0, 0)
        ))
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