
use parser::ast::{Stmt, Expr};
use self::context::Context;
use self::repr::{Value, Ty, TypedValue};
use self::repr::Value::*;

mod context;
mod repr;
#[cfg(tests)]
mod tests;

pub fn interpret(stmts: &[Stmt], ctx: &mut Context<TypedValue>) {
    for stmt in stmts {
        interpret_stmt(stmt, ctx);
    }
}

fn interpret_stmt(stmt: &Stmt, ctx: &mut Context<TypedValue>) {
    use self::Stmt::*;
    match *stmt {
        Declaration {
            ref ident,
            ref ty,
            ref value,
        } => {
            let mut decl = TypedValue::new(Unknown, Ty::from(ty.clone())).expect("Unknown should be valid value for any type.");
            if let Some(v) = value.as_ref().map(|e| interpret_expr(e, ctx)) {
                if !decl.set_typed(v) {
                    panic!("Invalid type");
                }
            }
            ctx.set(ident.clone(), decl);
        },
        Assignment {
            ref ident,
            ref value,
        } => {
            let value = interpret_expr(value, ctx);
            let error = &format!("Tried to assign to non-existent variable ´{}´ value ´{}´.", ident, value);
            ctx.set(ident.clone(), value).expect(error);
        },
        Loop {
            ref ident,
            ref from,
            ref to,
            ref stmts,
        } => {
            unimplemented!();
        },
        Read {
            ref ident,
        } => {
            unimplemented!();
        },
        Print {
            ref expr,
        } => {
            unimplemented!();
        },
        Assert {
            ref expr,
        } => {
            unimplemented!();
        },
    }
}

fn interpret_expr(expr: &Expr, ctx: &mut Context<TypedValue>) -> TypedValue {
    unimplemented!()
}