//! Performs static analysis on the ast.
//! 
//! Ensures that:
//!     - Right types are used in right place
//!     - Variables are declared before use
//!     - Variables aren't redeclared
//!     - Loop control variable is not modified
//! 
//! The static analysis will also gather all the errors that happened while parsing from the ast.
//! 
//! Static analysis should only panic on programming errors in it.

use util::Positioned;

use Ident;
use parser::ast::{Stmt, Expr, Opnd, BinOp, UnaOp, ParseError, OpndError, ExprError, TypeError};
use parser::ast::Type as AstType;
use interpreter::context::Context;

use self::AnalysisError::*;

#[cfg(test)]
mod tests;

#[derive(Debug, PartialEq)]
pub enum AnalysisError {
    UnknownVariable(Ident),
    DublicateDeclaration(Ident),
    MutationOfImmutable(Ident),
    TypeMismatch(Type, Type),
    IOMismatch(Type),
    UnableToBinOp(Type, BinOp),
    UnableToUnaOp(Type, UnaOp),
    ParseErr(ParseError),
    ExprErr(ExprError),
    OpndErr(OpndError),
    TypeErr(TypeError),
}

impl From<ParseError> for AnalysisError {
    fn from(e: ParseError) -> Self {
        AnalysisError::ParseErr(e)
    }
}

impl From<ExprError> for AnalysisError {
    fn from(e: ExprError) -> Self {
        AnalysisError::ExprErr(e)
    }
}

impl From<OpndError> for AnalysisError {
    fn from(e: OpndError) -> Self {
        AnalysisError::OpndErr(e)
    }
}

impl From<TypeError> for AnalysisError {
    fn from(e: TypeError) -> Self {
        AnalysisError::TypeErr(e)
    }
}

#[derive(Clone, PartialEq)]
pub enum Mutability {
    Mutable,
    Immutable,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Type {
    Bottom,
    Integer,
    Str,
    Bool
}

impl Type {
    fn is_compatible(&self, ty: &Type) -> bool {
        use self::Type::*;
        match (*self, *ty) {
            (Bottom, _) | (_, Bottom) => true,
            (Integer, Integer) => true,
            (Str, Str) => true,
            (Bool, Bool) => true,
            _ => false,
        }
    }
}

pub fn analyze(ast: &[Positioned<Stmt>], ctx: &mut Context<(Type, Mutability)>) -> Vec<Positioned<AnalysisError>> {
    let mut errors = vec![];
    analyze_stmts(ast, ctx, &mut errors);
    errors
}

fn analyze_stmts(stmts: &[Positioned<Stmt>], ctx: &mut Context<(Type, Mutability)>, errors: &mut Vec<Positioned<AnalysisError>>) {
    for stmt in stmts {
        analyze_stmt(stmt, ctx, errors);
    }
}

fn analyze_stmt(stmt: &Positioned<Stmt>, ctx: &mut Context<(Type, Mutability)>, errors: &mut Vec<Positioned<AnalysisError>>) {
    use self::Stmt::*;
    match stmt.data {
        ErrStmt(ref e) => errors.push(stmt.clone_with_data(e.clone().into())),
        Declaration {
            ref ident,
            ref ty,
            ref value,
        } => {
            if let Some(_) = ctx.get(ident) {
                errors.push(stmt.clone_with_data(DublicateDeclaration(ident.clone())));
            }
            let ty = match *ty {
                AstType::TypeErr(ref e) => {
                    errors.push(stmt.clone_with_data(e.clone().into()));
                    Type::Bottom
                },
                AstType::Integer => Type::Integer,
                AstType::Str => Type::Str,
                AstType::Bool => Type::Bool,
            };
            ctx.create(ident.clone(), (ty.clone(), Mutability::Mutable));
            if let Some(ref value) = *value {
                let expr_ty = analyze_expr(value, ctx, errors);
                if !ty.is_compatible(&expr_ty) {
                    errors.push(stmt.clone_with_data(TypeMismatch(ty.clone(), expr_ty)));
                }
            }
        },
        Assignment {
            ref ident,
            ref value,
        } => if let Some((ty, mutability)) = ctx.get(ident).cloned() {
            if mutability == Mutability::Immutable {
                errors.push(stmt.clone_with_data(MutationOfImmutable(ident.clone())));
            }
            let expr_ty = analyze_expr(value, ctx, errors);
            if !ty.is_compatible(&expr_ty) {
                errors.push(stmt.clone_with_data(TypeMismatch(ty, expr_ty)));
            }
        } else {
            errors.push(stmt.clone_with_data(UnknownVariable(ident.clone())));
        },
        Loop {
            ref ident,
            ref from,
            ref to,
            ref stmts,
        } => {
            if let Some(&mut (ref ty, ref mut mutability)) = ctx.get_mut(ident) {
                if !Type::Integer.is_compatible(&ty) {
                    errors.push(stmt.clone_with_data(TypeMismatch(Type::Integer, *ty)));
                }
                if mutability == &Mutability::Immutable {
                    errors.push(stmt.clone_with_data(MutationOfImmutable(ident.clone())));
                }
                *mutability = Mutability::Immutable;
            } else {
                errors.push(stmt.clone_with_data(UnknownVariable(ident.clone())));
            }
            ctx.create(ident.clone(), (Type::Integer, Mutability::Immutable));
            let from = analyze_expr(from, ctx, errors);
            if !Type::Integer.is_compatible(&from) {
                errors.push(stmt.clone_with_data(TypeMismatch(Type::Integer, from)));
            }
            let to = analyze_expr(to, ctx, errors);
            if !Type::Integer.is_compatible(&to) {
                errors.push(stmt.clone_with_data(TypeMismatch(Type::Integer, to)));
            }
            analyze_stmts(stmts, ctx, errors);
            if let Some(&mut (ref ty, ref mut mutability)) = ctx.get_mut(ident) {
                if !Type::Integer.is_compatible(&ty) {
                    // Because variable cannot be redeclared this shouldn't happen.
                    panic!("Type of loop control variable changed");
                }
                // NOTE: If there is two concecutive for-loops inside a for-loop each using
                // the same control variable, then only error for the first of the inner for-loops
                // will cause error.
                *mutability = Mutability::Mutable;
            } else {
                // There is no way to undeclare variable, so this should be impossible.
                panic!("Loop control variable disappeared");
            }
        },
        Read {
            ref ident,
        } => if let Some((ty, mutability)) = ctx.get(ident).cloned() {
            if mutability == Mutability::Immutable {
                errors.push(stmt.clone_with_data(MutationOfImmutable(ident.clone())));
            }
            if !(ty.is_compatible(&Type::Integer) || ty.is_compatible(&Type::Str)) {
                errors.push(stmt.clone_with_data(IOMismatch(ty)));
            }
        } else {
            errors.push(stmt.clone_with_data(UnknownVariable(ident.clone())));
        },
        Print {
            ref expr,
        } => {
            let ty = analyze_expr(expr, ctx, errors);
            if !(ty.is_compatible(&Type::Integer) || ty.is_compatible(&Type::Str)) {
                errors.push(stmt.clone_with_data(IOMismatch(ty)));
            }
        },
        Assert {
            ref expr,
        } => {
            let ty = analyze_expr(expr, ctx, errors);
            if !Type::Bool.is_compatible(&ty) {
                errors.push(stmt.clone_with_data(TypeMismatch(Type::Bool, ty)));
            }
        },
    }
}

fn analyze_expr(expr: &Positioned<Expr>, ctx: &mut Context<(Type, Mutability)>, errors: &mut Vec<Positioned<AnalysisError>>) -> Type {
    use self::Expr::*;
    use parser::ast::BinOp::*;
    use parser::ast::UnaOp::*;
    let handle_compatibility = |lhs: Type, rhs, result, errors: &mut Vec<_>| if lhs.is_compatible(&rhs) {
        result
    } else {
        errors.push(expr.clone_with_data(TypeMismatch(lhs, rhs)));
        Type::Bottom
    };
    match expr.data {
        ErrExpr(ref e) => {
            errors.push(expr.clone_with_data(e.clone().into()));
            Type::Bottom
        },
        BinOper {
            ref lhs,
            ref op,
            ref rhs,
        } => {
            let lhs = analyze_opnd(&lhs, ctx, errors);
            let rhs = analyze_opnd(&rhs, ctx, errors);
            match *op {
                Equality | LessThan => handle_compatibility(lhs, rhs, Type::Bool, errors),
                Addition => if Type::Integer.is_compatible(&lhs) || Type::Str.is_compatible(&lhs) {
                    let output = if Type::Bottom == lhs {
                        rhs
                    } else {
                        lhs
                    };
                    handle_compatibility(lhs, rhs, output, errors)
                } else {
                    errors.push(expr.clone_with_data(UnableToBinOp(lhs, Addition)));
                    Type::Bottom
                },
                Substraction | Multiplication | Division => if Type::Integer.is_compatible(&lhs) {
                    handle_compatibility(lhs, rhs, Type::Integer, errors)
                } else {
                    errors.push(expr.clone_with_data(UnableToBinOp(lhs, op.clone())));
                    Type::Bottom
                },
                And => if Type::Bool.is_compatible(&lhs) {
                    handle_compatibility(lhs, rhs, Type::Bool, errors)
                } else {
                    errors.push(expr.clone_with_data(UnableToBinOp(lhs, And)));
                    Type::Bottom
                },
            }
        },
        UnaOper {
            ref op,
            ref rhs,
        } => {
            let rhs = analyze_opnd(&rhs, ctx, errors);
            match *op {
                Not => if Type::Bool.is_compatible(&rhs) {
                    Type::Bool
                } else {
                    errors.push(expr.clone_with_data(UnableToUnaOp(rhs, Not)));
                    Type::Bottom
                },
            }
        },
        Opnd(ref opnd) => analyze_opnd(&opnd, ctx, errors),
    }
}

fn analyze_opnd(opnd: &Positioned<Opnd>, ctx: &mut Context<(Type, Mutability)>, errors: &mut Vec<Positioned<AnalysisError>>) -> Type {
    use self::Opnd::*;
    match opnd.data {
        OpndErr(ref e) => {
            errors.push(opnd.clone_with_data(e.clone().into()));
            Type::Bottom
        },
        Int(_) => Type::Integer,
        StrLit(_) => Type::Str,
        Ident(ref ident) => if let Some(ty) = ctx.get(ident) {
            ty.0
        } else {
            errors.push(opnd.clone_with_data(UnknownVariable(ident.clone())));
            Type::Bottom
        },
        Expr(ref expr) => analyze_expr(expr, ctx, errors),
    }
}