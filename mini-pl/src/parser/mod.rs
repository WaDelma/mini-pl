//! Turns vector of tokens to vector of statements which is the top level structure for the ast.
//!
//! Parsing also records the line and column positions of the ast nodes in the original code using
//! information provided by the lexer.
//! Handled parsing errors are represented as special nodes in the ast and
//! unhandled parsing errors will bubble out as `Err` variant of the result.
//! 
//! There shouldn't be any panics while parsing.
use parser::ast::Program;
use parser::ast::Parameter;
use parser::ast::Function;
use std::ops::Range;

use parsco::common::{Err2, Err3};
use parsco::*;

use Ident;
use util::{Positioned, Position};
use lexer::tokens::Token;
use lexer::tokens::Token::*;
use lexer::tokens::Punctuation::*;
use lexer::tokens::Side::*;
use lexer::tokens::Keyword::*;
use lexer::tokens::Operator::*;
use lexer::tokens::Literal::*;
use self::ast::{Stmt, Expr, Type, Opnd, BinOp, UnaOp, StmtError, AccessBy};
use self::ast::OpndError::*;
use self::ast::StmtError::*;
use self::ast::TypeError::*;
use self::ast::ExprError::*;

type ParseResult<'a, T> = ::parsco::Result<&'a [Positioned<Token>], T, StmtError>;

pub mod ast;
#[cfg(test)]
mod tests;

/// Parses given list of tokens to ast
pub fn parse(tokens: &[Positioned<Token>]) -> ParseResult<Positioned<Program>> {
    map(
        (
            sym(Keyword(Program)),
            fun(ident),
            sym(Punctuation(Semicolon)),
            many0(
                alt()
                    | fun(procedure)
                    | fun(function)
            ),
            fun(block),
            sym(Punctuation(Dot))
        ),
        |(program, name, _, functions, stmts, dot), _, _| Positioned::new(
            Program {
                name,
                functions,
                stmts
            },
            program.from,
            dot.to
        )
    ).parse(tokens)
        .map_err(|(e, r)| (FromErr::from(e), r))
    // many1(
    //     map(
    //         flat_map_err(
    //             (
    //                 fun(stmt),
    //                 sym(Punctuation(Semicolon))
    //             ),
    //             handle_semicolon_error
    //         ),
    //         // Make thereported ending position of statement to be after the semicolon
    //         |(mut statement, semicolon), _, _| {
    //             statement.to = semicolon.to;
    //             statement
    //         }
    //     )
    // ).parse(tokens)
    //     .map_err(|(e, r)| (FromErr::from(e), r))
}

pub fn procedure(tokens: &[Positioned<Token>]) -> ParseResult<Positioned<Function>> {
    map(
        (
            sym(Keyword(Procedure)),
            fun(ident),
            sym(Punctuation(Parenthesis(Open))),
            fun(parameters),
            sym(Punctuation(Parenthesis(Close))),
            sym(Punctuation(Semicolon)),
            fun(block),
            sym(Punctuation(Semicolon))
        ),
        |(procedure, name, _, params, _, _, stmts, semicolon), _, _| Positioned::new(
            Function {
                name,
                params,
                result: None,
                stmts,
            },
            procedure.from,
            semicolon.to
        )   
    ).parse(tokens)
        .map_err(|(e, r)| (FromErr::from(e), r))
}

pub fn function(tokens: &[Positioned<Token>]) -> ParseResult<Positioned<Function>> {
    map(
        (
            sym(Keyword(Function)),
            fun(ident),
            sym(Punctuation(Parenthesis(Open))),
            fun(parameters),
            sym(Punctuation(Parenthesis(Close))),
            sym(Punctuation(Colon)),
            fun(ty),
            sym(Punctuation(Semicolon)),
            fun(block),
            sym(Punctuation(Semicolon))
        ),
        |(function, name, _, params, _, _, result, _, stmts, semicolon), _, _| Positioned::new(
            Function {
                name,
                params,
                result: Some(result),
                stmts,
            },
            function.from,
            semicolon.to
        ) 
    ).parse(tokens)
        .map_err(|(e, r)| (FromErr::from(e), r))
}

pub fn parameters(tokens: &[Positioned<Token>]) -> ParseResult<Vec<Positioned<Parameter>>> {
    list0(
        map(
            (
                opt(sym(Keyword(Var))),
                fun(ident),
                sym(Punctuation(Colon)),
                fun(ty),
            ),
            |(var, name, _, ty), _, _| Positioned::new(
                Parameter {
                    by: var.map(|_| AccessBy::Reference).unwrap_or(AccessBy::Value),
                    name,
                    ty
                },
                var.map(|v| v.from).unwrap_or(name.from),
                ty.to
            )
        ),
        sym(Punctuation(Comma))
    ).parse(tokens)
        .map_err(|(e, r)| (FromErr::from(e), r))
}

pub fn block(tokens: &[Positioned<Token>]) -> ParseResult<Vec<Positioned<Stmt>>> {
    (
        sym(Keyword(Begin)),
        list0(
            fun(stmt),
            sym(Punctuation(Semicolon))
        ),
        sym(Keyword(End)),
    ).parse(tokens)
        .map_err(|(e, r)| (FromErr::from(e), r))
}

/// Handles missing semicolon at the end of statement
pub fn handle_semicolon_error(
    err: Err2<StmtError, (Positioned<Stmt>, Err2<Positioned<Token>, ()>)>,
    rest: &[Positioned<Token>],
    pos: Range<usize>
) -> ParseResult<(Positioned<Stmt>, Positioned<Token>)> {
    match err {
        Err2::V1(err) => Err((err, pos)),
        Err2::V2((stmt, _)) => {
            let pos = pos.end - 1;
            let token = Positioned::new(
                Punctuation(Semicolon),
                stmt.to.clone(),
                stmt.to
            );
            Ok((
                (
                    token.clone_with_data(Stmt::ErrStmt(MissingSemicolon(Box::new(stmt.data)))),
                    token
                ),
                &rest[pos..],
                pos
            ))
        },
    }
}

/// Parses single statement
pub fn stmt(tokens: &[Positioned<Token>]) -> ParseResult<Positioned<Stmt>> {
    (alt()
        | fun(declaration)
        | fun(assigment)
        | fun(for_loop)
        | fun(read)
        | fun(print)
        | fun(assert)
    ).parse(tokens)
}

/// Parses single variable declaration
pub fn declaration(tokens: &[Positioned<Token>]) -> ParseResult<Positioned<Stmt>> {
    map(
        (
            (sym(Keyword(Var)), fun(ident)),
            flat_map_err(
                preceded(
                    // TODO: Handle missing colon
                    sym(Punctuation(Colon)), fun(ty)
                ),
                handle_type_annotation_error
            ),
            // TODO: Handle missing assignment operator
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
        .map_err(|(_err, pos)| (StmtError::Unknown, pos)) // TODO: Better error
}

/// Handles error in the type annotation in variable definition
pub fn handle_type_annotation_error(
    err: Err2<
        Err2<Positioned<Token>, ()>,
        (Positioned<Token>, StmtError)
    >,
    rest: &[Positioned<Token>],
    pos: Range<usize>
) -> ParseResult<Positioned<Type>> {
    let end_pos = pos.end - 1;
    let no_type_annotation = |from, to| {
        Ok((
            Positioned::new(
                Type::TypeErr(NoTypeAnnotation),
                from,
                to
            ),
            &rest[end_pos..],
            end_pos
        ))
    };
    match err {
        Err2::V1(Err2::V1(token)) => no_type_annotation(token.from, token.to),
        Err2::V1(_) => no_type_annotation(Position::new(0, 0), Position::new(0, 0)),
        Err2::V2((_, e)) => Err((e, pos)),
    }
}

/// Parses single variable assignment
pub fn assigment(tokens: &[Positioned<Token>]) -> ParseResult<Positioned<Stmt>> {
    flat_map_err(
        map(
            (
                fun(ident),
                sym(Operator(Assignment)),
                fun(expr)
            ),
            |(ident, _, value), _, _| {
                let (from, to) = (ident.from, value.to.clone());
                Positioned::new(
                    Stmt::Assignment { ident: ident.data, value },
                    from,
                    to
                )
            }
        ),
        handle_missing_assignment_operator_error
    ).parse(tokens)
}

/// Handles missing assignment operator when assigning to a variable
pub fn handle_missing_assignment_operator_error(
    err: Err3<
        StmtError,
        (
            Positioned<String>,
            Err2<Positioned<Token>, ()>
        ),
        (
            Positioned<String>,
            Positioned<Token>,
            StmtError
        )
    >,
    rest: &[Positioned<Token>],
    pos: Range<usize>
) -> ParseResult<Positioned<Stmt>> {
    match err {
        Err3::V2((_, Err2::V1(e))) => {
            let statement = Positioned::new(
                Stmt::ErrStmt(InvalidAssignment),
                e.to.clone(),
                e.to
            );
            let pos = take_until::<_, &[Positioned<_>]>(sym(Punctuation(Semicolon)))
                .parse(rest)
                .map(|(_, _, pos)| pos - 1)
                .unwrap_or(pos.end);
            Ok((
                statement,
                &rest[pos..],
                pos
            ))
        },
        _ => Err((StmtError::Unknown, pos)), // TODO: Better error
    }
}

/// Parses single for-loop
pub fn for_loop(tokens: &[Positioned<Token>]) -> ParseResult<Positioned<Stmt>> {
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
            Positioned::new(
                Stmt::Loop {
                    ident: ident.data,
                    from: range_from,
                    to: range_to,
                    stmts
                },
                for_keyword.from,
                end.to
            )
        }
    ).parse(tokens)
        .map_err(|(_err, pos)| (StmtError::Unknown, pos)) // TODO: Better error
}

/// Parses single read statement
pub fn read(tokens: &[Positioned<Token>]) -> ParseResult<Positioned<Stmt>> {
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
                _ => StmtError::Unknown,
            },
            pos
        ))
}

/// Parses single print statement
pub fn print(tokens: &[Positioned<Token>]) -> ParseResult<Positioned<Stmt>> {
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
                _ => StmtError::Unknown,
            },
            pos
        ))
}

/// Parses single assert statement
pub fn assert(tokens: &[Positioned<Token>]) -> ParseResult<Positioned<Stmt>> {
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
                _ => StmtError::Unknown,
            },
            pos
        ))
}

/// Handles missing parenthesis in assert statement
pub fn handle_parenthesis_missing_error(
    err: Err3<
        Err2<Positioned<Token>, ()>,
        (Positioned<Token>, StmtError),
        (Positioned<Token>, Positioned<Expr>, Err2<Positioned<Token>, ()>)
    >,
    rest: &[Positioned<Token>],
    pos: Range<usize>
) -> ParseResult<Positioned<Expr>> {
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
                
            let pos = pos_before + take_until::<_, &[Positioned<_>]>(sym(Punctuation(Semicolon)))
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

/// Parses single expression
pub fn expr(tokens: &[Positioned<Token>]) -> ParseResult<Positioned<Expr>> {
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

/// Parses single operand
pub fn opnd(tokens: &[Positioned<Token>]) -> ParseResult<Positioned<Opnd>> {
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
            |_, rest: &[Positioned<_>], _| {
                let (from, to) = rest.last()
                    .map(|last| (last.from.clone(), last.to.clone()))
                    .unwrap_or((
                        Position::new(0, 0),
                        Position::new(0, 0)
                    ));
                Positioned::new(Opnd::OpndErr(InvalidOperand), from, to)
            }
        )
    ).parse(tokens)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
}

/// Parses single identifier
pub fn ident(tokens: &[Positioned<Token>]) -> ParseResult<Positioned<Ident>> {
    fst().parse(tokens)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
        .and_then(|(fst, rest, pos)| if let Identifier(ref tok) = *fst.sym() {
            Ok((fst.clone_with_data(tok.clone()), rest, pos))
        } else {
            Err((Unknown, 0..pos))
        })
}

/// Parses single type
pub fn ty(tokens: &[Positioned<Token>]) -> ParseResult<Positioned<Type>> {
    fst().parse(tokens)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
        .and_then(|(fst, rest, pos)| Ok((
            fst.clone_with_data(Type::from_token(&fst.data)),
            rest,
            pos
        )))
}

/// Parses single integer literal
pub fn int(tokens: &[Positioned<Token>]) -> ParseResult<Positioned<Opnd>> {
    fst().parse(tokens)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
        .and_then(|(fst, rest, pos)| if let Literal(Integer(ref int)) = *fst.sym() {
            Ok((fst.clone_with_data(Opnd::Int(int.clone())), rest, pos))
        } else {
            Err((Unknown, 0..pos))
        })
}

/// Parses single string literal
pub fn string(tokens: &[Positioned<Token>]) -> ParseResult<Positioned<Opnd>> {
    fst().parse(tokens)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
        .and_then(|(fst, rest, p)| if let Literal(StringLit(ref str_lit)) = *fst.sym() {
            Ok((fst.clone_with_data(Opnd::StrLit(str_lit.clone())), rest, p))
        } else {
            Err((Unknown, 0..p))
        })
}

/// Parses single binary operator
pub fn binop(tokens: &[Positioned<Token>]) -> ParseResult<BinOp> {
    fst().parse(tokens)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
        .and_then(|(fst, rest, pos)| if let Operator(ref oper) = *fst.sym() {
            Ok((BinOp::from_oper(oper).ok_or((Unknown, 0..pos))?, rest, pos))
        } else {
            Err((Unknown, 0..pos))
        })
}

/// Parses single unary operator
pub fn unaop(tokens: &[Positioned<Token>]) -> ParseResult<Positioned<UnaOp>> {
    fst().parse(tokens)
        .map_err(|(err, pos)| (FromErr::from(err), pos))
        .and_then(|(fst, rest, pos)| if let Operator(ref oper) = *fst.sym() {
            Ok((
                fst.clone_with_data(
                    UnaOp::from_oper(oper).ok_or((Unknown, 0..pos))?
                ),
                rest,
                pos
            ))
        } else {
            Err((Unknown, 0..pos))
        })
}