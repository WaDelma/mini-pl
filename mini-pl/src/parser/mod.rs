use parsco::common::{Err2, Err3, Void};
use parsco::*;

use Ident;
use lexer::tokens::Tok;
use lexer::tokens::Token::*;
use lexer::tokens::Punctuation::*;
use lexer::tokens::Side::*;
use lexer::tokens::Keyword::*;
use lexer::tokens::Operator::*;
use lexer::tokens::Literal::*;
use self::ast::{Statement, Stmt, Expr, Type, Opnd, BinOp, UnaOp, ParseError};
use self::ast::OpndError::*;
use self::ast::ParseError::*;
use self::ast::TypeError::*;
use self::ast::ExprError::*;

type Result<'a, T> = ::parsco::Result<&'a [Tok], T, ParseError>;

pub mod ast;
#[cfg(test)]
mod tests;

pub fn parse(ts: &[Tok]) -> Result<Vec<Stmt>> {
    many1(
        flat_map_err(
            terminated(
                fun(stmt),
                sym(Punctuation(Semicolon))
            ),
            |err, rest, pos| match err {
                Err2::V1(e) => Err((e, pos)),
                Err2::V2(_) => Ok((Stmt::ErrStmt(MissingSemicolon), &rest[(pos.end - 1)..], pos.end - 1)),
            }
        )
    ).parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
}

pub fn stmt(ts: &[Tok]) -> Result<Stmt> {
    (alt()
        | map(
            (
                preceded(
                    sym(Keyword(Var)), fun(ident)
                ),
                flat_map_err(
                    preceded(
                        sym(Punctuation(Colon)), fun(ty)
                    ),
                    |err, rest, pos| match err {
                        Err2::V1(_) => Ok((Type::TypeErr(NoTypeAnnotation), &rest[(pos.end - 1)..], pos.end - 1)),
                        Err2::V2(e) => Err((e, pos)),
                    }
                ),
                opt(preceded(
                    sym(Operator(Assignment)), fun(expr)
                ))
            ),
            |(ident, ty, value), _, _| Stmt::Declaration {
                ident,
                ty,
                value
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
            |(ident, value), _, _| Stmt::Assignment {
                ident,
                value
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
                terminated(
                    fun(parse),
                    (sym(Keyword(End)), sym(Keyword(For)))
                )
            ),
            |(ident, from, to, stmts), _, _| Stmt::Loop {
                ident,
                from,
                to,
                stmts
            }
        )
        | map(
            preceded(
                sym(Keyword(Read)),
                fun(ident)
            ),
            |ident, _, _| Stmt::Read {
                ident
            }
        )
        | map(
            preceded(
                sym(Keyword(Print)),
                fun(expr)
            ),
            |expr, _, _| Stmt::Print {
                expr
            }
        )
        | map(
            preceded(
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
                                    Ok((_, _, pos2)) => {
                                        let pos = pos_before + pos2 - 1;
                                        Ok((Expr::ErrExpr(MissingParenthesis(Open)), &rest[pos..], pos))
                                    },
                                    Err(_) => Ok((Expr::ErrExpr(MissingParenthesis(Open)), &rest[pos_before..], pos_before))
                                }
                            },
                            V2(e) => Err((e, pos)),
                            V3(_) => Ok((Expr::ErrExpr(MissingParenthesis(Close)), &rest[pos_before..], pos_before)),
                        }
                    }
                )
            ),
            |expr, _, _| Stmt::Assert {
                expr
            }
        )
    ).parse(ts)
        .map_err(|(err, pos)| (match err {
                Err2::V2(e) => FromErr::from(e),
                _ => ParseError::Unknown,
            }, pos))
}

pub fn expr(ts: &[Tok]) -> Result<Expr> {
    (alt()
        | map(
            (fun(opnd), fun(binop), fun(opnd)),
            |(lhs, op, rhs), _, _| Expr::BinOper {
                lhs,
                op,
                rhs
            }
        )
        | map(
            (fun(unaop), fun(opnd)),
            |(op, rhs), _, _| Expr::UnaOper {
                op,
                rhs
            }
        )
        | map(
            fun(opnd),
            |o, _, _| Expr::Opnd(o)
        )
    ).parse(ts)
}

pub fn opnd(ts: &[Tok]) -> Result<Opnd> {
    (alt()
        | fun(int)
        | fun(string)
        | map(fun(ident), |i, _, _| Opnd::Ident(i))
        | preceded(
            sym(Punctuation(Parenthesis(Open))),
            flat_map_err(
                terminated(
                    map(
                        fun(expr),
                        |expr, _, _| Opnd::Expr(Box::new(expr))
                    ),
                    sym(Punctuation(Parenthesis(Close)))
                ),
                |err, rest, pos| match err {
                    Err2::V1(e) => Err((e, pos)),
                    Err2::V2(_) => Ok((Opnd::OpndErr(MissingEndParenthesis), &rest[(pos.end - 1)..], pos.end - 1)),
                }
            )
        )
        | constant(Opnd::OpndErr(InvalidOperand))
    ).parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
}

pub fn ident(ts: &[Tok]) -> Result<Ident> {
    fst().parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
        .and_then(|(t, s, p)| if let Identifier(ref t) = *t.sym() {
            Ok((t.clone(), s, p))
        } else {
            Err((Unknown, 0..p))
        })
}

pub fn ty(ts: &[Tok]) -> Result<Type> {
    fst().parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
        .and_then(|(t, s, p)| if let Keyword(ref t) = *t.sym() {
            Ok((match *t {
                Int => Type::Integer,
                Bool => Type::Bool,
                Str => Type::Str,
                ref k => Type::TypeErr(KeywordNotType(k.clone())),
            }, s, p))
        } else {
            match *t.sym() {
                Identifier(ref i) => Ok((Type::TypeErr(UnknownType(i.clone())), s, p)),
                _ => Err((Unknown, 0..p))   
            }
        })
}

pub fn int(ts: &[Tok]) -> Result<Opnd> {
    fst().parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
        .and_then(|(t, s, p)| if let Literal(Integer(ref i)) = *t.sym() {
            Ok((Opnd::Int(i.clone()), s, p))
        } else {
            Err((Unknown, 0..p))
        })
}

pub fn string(ts: &[Tok]) -> Result<Opnd> {
    fst().parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
        .and_then(|(t, s, p)| if let Literal(StringLit(ref t)) = *t.sym() {
            Ok((Opnd::StrLit(t.clone()), s, p))
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

pub fn unaop(ts: &[Tok]) -> Result<UnaOp> {
    fst().parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
        .and_then(|(t, s, p)| if let Operator(ref o) = *t.sym() {
            Ok((UnaOp::from_oper(o).ok_or((Unknown, 0..p))?, s, p))
        } else {
            Err((Unknown, 0..p))
        })
}