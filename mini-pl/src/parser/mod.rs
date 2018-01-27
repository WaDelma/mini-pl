use parsco::{Parser, FromErr, many1, preceded, terminated, delimited, fun, one, alt, opt, map, fst};

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
            one(Punctuation(Semicolon))
        )
    ).parse(ts)
        .map_err(FromErr::from)
}

pub fn stmt(ts: &[Token]) -> Result<Stmt> {
    (alt()
        | map(
            (
                preceded(
                    one(Keyword(Var)), fun(ident)
                ),
                preceded(
                    one(Punctuation(Colon)), fun(ty)
                ),
                opt(preceded(
                    one(Operator(Assignment)), fun(expr)
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
                    one(Operator(Assignment)),
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
                    one(Keyword(For)),
                    fun(ident),
                    one(Keyword(In))
                ),
                fun(expr),
                delimited(
                    one(Operator(Range)),
                    fun(expr),
                    one(Keyword(Do))
                ),
                terminated(
                    fun(parse),
                    (one(Keyword(End)), one(Keyword(For)))
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
                one(Keyword(Read)),
                fun(ident)
            ),
            |ident| Stmt::Read {
                ident
            }
        )
        | map(
            preceded(
                one(Keyword(Print)),
                fun(expr)
            ),
            |expr| Stmt::Print {
                expr
            }
        )
        | map(
            preceded(
                one(Keyword(Assert)),
                delimited(
                    one(Punctuation(Parenthesis(Open))),
                    fun(expr),
                    one(Punctuation(Parenthesis(Close))),
                )
            ),
            |expr| Stmt::Assert {
                expr
            }
        )
    ).parse(ts)
        .map_err(FromErr::from)
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
            one(Punctuation(Parenthesis(Open))),
            map(
                fun(expr),
                |expr| Opnd::Expr(Box::new(expr))
            ),
            one(Punctuation(Parenthesis(Close)))
        )
    ).parse(ts)
        .map_err(FromErr::from)
}

pub fn ident(ts: &[Token]) -> Result<Ident> {
    fst().parse(ts)
        .map_err(FromErr::from)
        .and_then(|(t, s)| if let Identifier(ref t) = t {
            Ok((t.clone(), s))
        } else {
            Err(Unknown)
        })
}

pub fn ty(ts: &[Token]) -> Result<Type> {
    fst().parse(ts)
        .map_err(FromErr::from)
        .and_then(|(t, s)| if let Keyword(ref t) = t {
            Ok((match *t {
                Int => Type::Integer,
                Bool => Type::Bool,
                Str => Type::Str,
                _ => Err(Unknown)?,
            }, s))
        } else {
            Err(Unknown)
        })
}

pub fn int(ts: &[Token]) -> Result<Opnd> {
    fst().parse(ts)
        .map_err(FromErr::from)
        .and_then(|(t, s)| if let Literal(Integer(ref i)) = t {
            Ok((Opnd::Int(i.clone()), s))
        } else {
            Err(Unknown)
        })
}

pub fn string(ts: &[Token]) -> Result<Opnd> {
    fst().parse(ts)
        .map_err(FromErr::from)
        .and_then(|(t, s)| if let Literal(StringLit(ref t)) = t {
            Ok((Opnd::StrLit(t.clone()), s))
        } else {
            Err(Unknown)
        })
}

pub fn binop(ts: &[Token]) -> Result<BinOp> {
    fst().parse(ts)
        .map_err(FromErr::from)
        .and_then(|(t, s)| if let Operator(ref o) = t {
            Ok((BinOp::from_oper(o).ok_or(Unknown)?, s))
        } else {
            Err(Unknown)
        })
}

pub fn unaop(ts: &[Token]) -> Result<UnaOp> {
    fst().parse(ts)
        .map_err(FromErr::from)
        .and_then(|(t, s)| if let Operator(ref o) = t {
            Ok((UnaOp::from_oper(o).ok_or(Unknown)?, s))
        } else {
            Err(Unknown)
        })
}