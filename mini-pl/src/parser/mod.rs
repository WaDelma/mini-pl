use std::ops::Range;

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

pub fn parse(tokens: &[Tok]) -> Result<Vec<Positioned<Stmt>>> {
    many1(
        map(
            flat_map_err(
                (
                    fun(stmt),
                    sym(Punctuation(Semicolon))
                ),
                handle_semicolon_error
            ),
            // Make statements end to be the semicolon
            |(mut statement, semicolon), _, _| {
                statement.to = semicolon.to;
                statement
            }
        )
    ).parse(tokens)
        .map_err(|(e, r)| (FromErr::from(e), r))
}

fn handle_semicolon_error(
    err: Err2<ParseError, (Positioned<Stmt>, Err2<Tok, ()>)>,
    rest: &[Tok],
    pos: Range<usize>
) -> Result<(Positioned<Stmt>, Tok)> {
    match err {
        Err2::V1(err) => Err((err, pos)),
        Err2::V2((stmt, _)) => {
            let (from, to) = (stmt.to.clone(), stmt.to);
            let statement = Positioned::new(
                Stmt::ErrStmt(MissingSemicolon),
                from.clone(),
                to.clone()
            );
            let pos = pos.end - 1;
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
}

pub fn stmt(tokens: &[Tok]) -> Result<Positioned<Stmt>> {
    (alt()
        | fun(declaration)
        | fun(assigment)
        | fun(for_loop)
        | fun(read)
        | fun(print)
        | fun(assert)
    ).parse(tokens)
}

pub fn declaration(tokens: &[Tok]) -> Result<Positioned<Stmt>> {
    map(
        (
            (sym(Keyword(Var)), fun(ident)),
            flat_map_err(
                preceded(
                    sym(Punctuation(Colon)), fun(ty)
                ),
                handle_type_annotation_error
            ),
            opt(preceded(
                sym(Operator(Assignment)), fun(expr)
            ))
        ),
        |((var, ident), ty, value), _, _| {
            let to = value.clone()
                .map(|expr| expr.to)
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
    ).parse(tokens)
        .map_err(|(err, pos)| (ParseError::Unknown, pos)) // TODO: Better error
}

fn handle_type_annotation_error(
    err: Err2<
        Err2<Tok, ()>,
        (Tok, ParseError)
    >,
    rest: &[Tok],
    pos: Range<usize>
) -> Result<Positioned<Type>> {
    match err {
        Err2::V1(Err2::V1(token)) => {
            let pos = pos.end - 1;
            Ok((
                Positioned::new(
                    Type::TypeErr(NoTypeAnnotation),
                    token.from,
                    token.to
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
        Err2::V2((_, e)) => Err((e, pos)),
    }
}

pub fn assigment(tokens: &[Tok]) -> Result<Positioned<Stmt>> {
    map(
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
                Stmt::Assignment { ident: ident.data, value },
                from,
                to
            )
        }
    ).parse(tokens)
        .map_err(|(err, pos)| (ParseError::Unknown, pos)) // TODO: Better error
}

pub fn for_loop(tokens: &[Tok]) -> Result<Positioned<Stmt>> {
    map(
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
        |((for_keyword, ident, _), range_from, range_to, (stmts, (_, end))), _, _| {
            let (from, to) = (for_keyword.from, end.to);
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
    ).parse(tokens)
        .map_err(|(err, pos)| (ParseError::Unknown, pos)) // TODO: Better error
}

pub fn read(tokens: &[Tok]) -> Result<Positioned<Stmt>> {
    map(
        (
            sym(Keyword(Read)),
            fun(ident)
        ),
        |(read_keyword, ident), _, _| Positioned::new(
            Stmt::Read {
                ident: ident.data
            },
            read_keyword.from,
            ident.to
        )
    ).parse(tokens)
        .map_err(|(err, pos)| (
            match err {
                Err2::V2((_, e)) => FromErr::from(e),
                _ => ParseError::Unknown,
            },
            pos
        ))
}

pub fn print(tokens: &[Tok]) -> Result<Positioned<Stmt>> {
    map(
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
    ).parse(tokens)
        .map_err(|(err, pos)| (
            match err {
                Err2::V2((_, err)) => FromErr::from(err),
                _ => ParseError::Unknown,
            },
            pos
        ))
}

pub fn assert(tokens: &[Tok]) -> Result<Positioned<Stmt>> {
    map(
        (
            sym(Keyword(Assert)),
            flat_map_err(
                delimited(
                    sym(Punctuation(Parenthesis(Open))),
                    fun(expr),
                    sym(Punctuation(Parenthesis(Close))),
                ),
                handle_parenthesis_missing_error
            )
        ),
        |(assert, expr), _, _| {
            let (from, to) = (assert.from, expr.to.clone());
            Positioned::new(
                Stmt::Assert { expr },
                from,
                to,
            )
        }
    ).parse(tokens)
        .map_err(|(err, pos)| (
            match err {
                Err2::V2((_, err)) => FromErr::from(err),
                _ => ParseError::Unknown,
            },
            pos
        ))
}

pub fn handle_parenthesis_missing_error(
    err: Err3<
        Err2<Tok, ()>,
        (Tok, ParseError),
        (Tok, Positioned<Expr>, Err2<Tok, ()>)
    >,
    rest: &[Tok],
    pos: Range<usize>
) -> Result<Positioned<Expr>> {
    use self::Err3::*;
    let pos_before = pos.end - 1;
    match err {
        V1(_) => {
            let (from, to) = rest.first()
                .map(|tok| (tok.from.clone(), tok.to.clone()))
                .unwrap_or((
                    Position::new(0, 0),
                    Position::new(0, 0)
                ));
                
            let pos = pos_before + take_until::<_, &[Tok]>(sym(Punctuation(Semicolon)))
                .parse(rest)
                .map(|(_, _, pos)| pos - 1)
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
        V2((_, err)) => Err((err, pos)),
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

pub fn expr(tokens: &[Tok]) -> Result<Positioned<Expr>> {
    (alt()
        | map(
            (fun(opnd), fun(binop), fun(opnd)),
            |(lhs, op, rhs), _, _| {
                let (from, to) = (lhs.from.clone(), rhs.to.clone());
                Positioned::new(
                    Expr::BinOper { lhs, op, rhs },
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
                    Expr::UnaOper { op: op.data, rhs },
                    from,
                    to
                )
            }
        )
        | map(
            fun(opnd),
            |opnd, _, _|  {
                let (from, to) = (opnd.from.clone(), opnd.to.clone());
                Positioned::new(Expr::Opnd(opnd), from, to)
            }
        )
    ).parse(tokens)
}

pub fn opnd(tokens: &[Tok]) -> Result<Positioned<Opnd>> {
    (alt()
        | fun(int)
        | fun(string)
        | map(
            fun(ident), |ident, _, _| Positioned::new(
                Opnd::Ident(ident.data),
                ident.from,
                ident.to
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
                Err3::V2((_, err)) => Err((err, pos)),
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
                    .map(|last| (last.from.clone(), last.to.clone()))
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
    ).parse(tokens)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
}

pub fn ident(tokens: &[Tok]) -> Result<Positioned<Ident>> {
    fst().parse(tokens)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
        .and_then(|(fst, rest, pos)| if let Identifier(ref tok) = *fst.sym() {
            Ok((
                Positioned::new(
                    tok.clone(),
                    fst.from.clone(),
                    fst.to.clone()
                ),
                rest,
                pos
            ))
        } else {
            Err((Unknown, 0..pos))
        })
}

pub fn ty(tokens: &[Tok]) -> Result<Positioned<Type>> {
    fst().parse(tokens)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
        .and_then(|(fst, rest, pos)| {
            let (from, to) = (fst.from.clone(), fst.to.clone());
            Ok((
                Positioned::new(Type::from_token(&fst.token), from, to),
                rest,
                pos
            ))
        })
}

pub fn int(tokens: &[Tok]) -> Result<Positioned<Opnd>> {
    fst().parse(tokens)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
        .and_then(|(fst, rest, pos)| if let Literal(Integer(ref int)) = *fst.sym() {
            Ok((
                Positioned::new(
                    Opnd::Int(int.clone()),
                    fst.from.clone(),
                    fst.to.clone()
                ),
                rest,
                pos
            ))
        } else {
            Err((Unknown, 0..pos))
        })
}

pub fn string(tokens: &[Tok]) -> Result<Positioned<Opnd>> {
    fst().parse(tokens)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
        .and_then(|(fst, rest, p)| if let Literal(StringLit(ref str_lit)) = *fst.sym() {
            Ok((
                Positioned::new(
                    Opnd::StrLit(str_lit.clone()),
                    fst.from.clone(),
                    fst.to.clone()
                ),
                rest,
                p
            ))
        } else {
            Err((Unknown, 0..p))
        })
}

pub fn binop(tokens: &[Tok]) -> Result<BinOp> {
    fst().parse(tokens)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
        .and_then(|(fst, rest, pos)| if let Operator(ref oper) = *fst.sym() {
            Ok((BinOp::from_oper(oper).ok_or((Unknown, 0..pos))?, rest, pos))
        } else {
            Err((Unknown, 0..pos))
        })
}

pub fn unaop(tokens: &[Tok]) -> Result<Positioned<UnaOp>> {
    fst().parse(tokens)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
        .and_then(|(fst, rest, pos)| if let Operator(ref oper) = *fst.sym() {
            Ok((
                Positioned::new(
                    UnaOp::from_oper(oper)
                        .ok_or((Unknown, 0..pos))?,
                    fst.from.clone(),
                    fst.to.clone()
                ),
                rest,
                pos
            ))
        } else {
            Err((Unknown, 0..pos))
        })
}