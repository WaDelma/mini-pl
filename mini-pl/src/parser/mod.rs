use parsco::{Parser, FromErr, Sym, many1, preceded, terminated, delimited, fun, sym, alt, opt, map, fst};

use Ident;
use lexer::tokens::Tok;
use lexer::tokens::Token::*;
use lexer::tokens::Punctuation::*;
use lexer::tokens::Side::*;
use lexer::tokens::Keyword::*;
use lexer::tokens::Operator::*;
use lexer::tokens::Literal::*;
use self::ast::{Statement, Stmt, Expr, Type, Opnd, BinOp, UnaOp};
use self::ParseError::*;

type Result<'a, T> = ::parsco::Result<&'a [Tok], T, ParseError>;

pub mod ast;
#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq)]
pub enum ParseError {
    Unknown,
}

impl FromErr<()> for ParseError {
    fn from(_: ()) -> Self {
        ParseError::Unknown
    }
}

impl FromErr<ParseError> for ParseError {
    fn from(l: ParseError) -> Self {
        l
    }
}

pub fn parse(ts: &[Tok]) -> Result<Vec<Stmt>> {
    many1(
        terminated(
            fun(stmt),
            sym(Punctuation(Semicolon))
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
                preceded(
                    sym(Punctuation(Colon)), fun(ty)
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
                delimited(
                    sym(Punctuation(Parenthesis(Open))),
                    fun(expr),
                    sym(Punctuation(Parenthesis(Close))),
                )
            ),
            |expr, _, _| Stmt::Assert {
                expr
            }
        )
    ).parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
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
        | delimited(
            sym(Punctuation(Parenthesis(Open))),
            map(
                fun(expr),
                |expr, _, _| Opnd::Expr(Box::new(expr))
            ),
            sym(Punctuation(Parenthesis(Close)))
        )
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
                _ => Err((Unknown, 0..p))?,
            }, s, p))
        } else {
            Err((Unknown, 0..p))
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