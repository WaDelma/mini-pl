use parsco::{Parser, FromErr, many1, preceded, terminated, delimited, fun, sym, alt, opt, map, fst};

use Ident;
use lexer::tokens::Token::*;
use lexer::tokens::Punctuation::*;
use lexer::tokens::Side::*;
use lexer::tokens::Keyword::*;
use lexer::tokens::Operator::*;
use lexer::tokens::Literal::*;
use lexer::tokens::Token;
use self::ast::{Stmt, Expr, Type, Opnd, BinOp, UnaOp};
use self::ParseError::*;

type Result<'a, T> = ::parsco::Result<&'a [Token], T, ParseError>;

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

pub fn parse(ts: &[Token]) -> Result<Vec<Stmt>> {
    many1(
        terminated(
            fun(stmt),
            sym(Punctuation(Semicolon))
        )
    ).parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
}

pub fn stmt(ts: &[Token]) -> Result<Stmt> {
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
            |(ident, ty, value)| Stmt::Declaration {
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
            |(ident, value)| Stmt::Assignment {
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
            |(ident, from, to, stmts)| Stmt::Loop {
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
            |ident| Stmt::Read {
                ident
            }
        )
        | map(
            preceded(
                sym(Keyword(Print)),
                fun(expr)
            ),
            |expr| Stmt::Print {
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
            |expr| Stmt::Assert {
                expr
            }
        )
    ).parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
}

pub fn expr(ts: &[Token]) -> Result<Expr> {
    (alt()
        | map(
            (fun(opnd), fun(binop), fun(opnd)),
            |(lhs, op, rhs)| Expr::BinOper {
                lhs,
                op,
                rhs
            }
        )
        | map(
            (fun(unaop), fun(opnd)),
            |(op, rhs)| Expr::UnaOper {
                op,
                rhs
            }
        )
        | map(
            fun(opnd),
            Expr::Opnd
        )
    ).parse(ts)
}

pub fn opnd(ts: &[Token]) -> Result<Opnd> {
    (alt()
        | fun(int)
        | fun(string)
        | map(fun(ident), Opnd::Ident)
        | delimited(
            sym(Punctuation(Parenthesis(Open))),
            map(
                fun(expr),
                |expr| Opnd::Expr(Box::new(expr))
            ),
            sym(Punctuation(Parenthesis(Close)))
        )
    ).parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
}

pub fn ident(ts: &[Token]) -> Result<Ident> {
    fst().parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
        .and_then(|(t, s, p)| if let Identifier(ref t) = t {
            Ok((t.clone(), s, p))
        } else {
            Err((Unknown, 0..p))
        })
}

pub fn ty(ts: &[Token]) -> Result<Type> {
    fst().parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
        .and_then(|(t, s, p)| if let Keyword(ref t) = t {
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

pub fn int(ts: &[Token]) -> Result<Opnd> {
    fst().parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
        .and_then(|(t, s, p)| if let Literal(Integer(ref i)) = t {
            Ok((Opnd::Int(i.clone()), s, p))
        } else {
            Err((Unknown, 0..p))
        })
}

pub fn string(ts: &[Token]) -> Result<Opnd> {
    fst().parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
        .and_then(|(t, s, p)| if let Literal(StringLit(ref t)) = t {
            Ok((Opnd::StrLit(t.clone()), s, p))
        } else {
            Err((Unknown, 0..p))
        })
}

pub fn binop(ts: &[Token]) -> Result<BinOp> {
    fst().parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
        .and_then(|(t, s, p)| if let Operator(ref o) = t {
            Ok((BinOp::from_oper(o).ok_or((Unknown, 0..p))?, s, p))
        } else {
            Err((Unknown, 0..p))
        })
}

pub fn unaop(ts: &[Token]) -> Result<UnaOp> {
    fst().parse(ts)
        .map_err(|(e, r)| (FromErr::from(e), r))
        .and_then(|(t, s, p)| if let Operator(ref o) = t {
            Ok((UnaOp::from_oper(o).ok_or((Unknown, 0..p))?, s, p))
        } else {
            Err((Unknown, 0..p))
        })
}