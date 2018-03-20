use num_bigint::BigInt;

use util::Positioned;
use parser::ast::{Stmt, Expr, Opnd};
use self::context::{Context, Io};
use self::repr::{Ty, Value, TypedValue};
use self::repr::Value::*;

pub mod context;
pub mod repr;
#[cfg(test)]
mod tests;

pub fn interpret<IO: Io>(stmts: &[Positioned<Stmt>], ctx: &mut Context<TypedValue>, stdio: &mut IO) {
    for stmt in stmts {
        interpret_stmt(stmt, ctx, stdio);
    }
}

// TODO: Use stdio for printing errors. Also test error reporting. Also row and column numbers for errors.
fn interpret_stmt<IO: Io>(stmt: &Positioned<Stmt>, ctx: &mut Context<TypedValue>, stdio: &mut IO) {
    use self::Stmt::*;
    match stmt.data {
        ErrStmt(ref e) => panic!("Error while parsing: {:?}", e),
        Declaration {
            ref ident,
            ref ty,
            ref value,
        } => {
            let mut decl = TypedValue::new(Unknown, Ty::from(ty.clone())).expect("Unknown should be valid value for any type.");
            if let Some(v) = value.as_ref().map(|e| interpret_expr(&e.data, ctx)) {
                if !decl.set_typed(v) {
                    panic!("Invalid type");
                }
            }
            ctx.create(ident.clone(), decl);
        },
        Assignment {
            ref ident,
            ref value,
        } => {
            let value = interpret_expr(&value.data, ctx);
            let error = &format!("Tried to assign to non-existent variable ´{}´ value ´{}´.", ident, value);
            ctx.get_mut(ident)
                .expect(error)
                .set_typed(value);
        },
        Loop {
            ref ident,
            ref from,
            ref to,
            ref stmts,
        } => {
            let mut from = interpret_expr(&from.data, ctx).integer().clone();
            let to = interpret_expr(&to.data, ctx).integer().clone();
            ctx.get_mut(ident)
                .expect("Non-existent control variable")
                .set(Integer(from.clone()));
            ctx.freeze(ident);
            while from <= to {
                interpret(stmts, ctx, stdio);
                from = from + &BigInt::from(1);
                ctx.thaw(ident);
                ctx.get_mut(ident)
                    .expect("Non-existent control variable")
                    .set(Integer(from.clone()));
                ctx.freeze(ident);
            }
            ctx.thaw(ident);
        },
        Read {
            ref ident,
        } => {
            let tv = ctx.get_mut(ident).expect("Tried to read to a non-existent variable");
            match tv.ty().clone() {
                Ty::Integer => {
                    let res = stdio.read_to_whitespace()
                        .parse()
                        .unwrap();
                    tv.set(Integer(res));
                },
                Ty::Str => {
                    let res = stdio.read_to_whitespace()
                        .parse()
                        .unwrap();
                    tv.set(Str(res));
                }
                ty => panic!("Cannot read into variable with type `{}`.", ty),
            }
        },
        Print {
            ref expr,
        } => {
            let value = interpret_expr(&expr.data, ctx);
            match *value.value() {
                Integer(ref i) => stdio.write(&i.to_string()),
                Str(ref s) => stdio.write(s),
                Unknown => panic!("Use of uninitialized value."),
                ref v => panic!("Use of wrong type of value {}", v),
            }
        },
        Assert {
            ref expr,
        } => {
            let value = interpret_expr(&expr.data, ctx);
            match *value.value() {
                Bool(ref b) => if !b {
                    panic!("Assertion failed for `{}` resolved as `{}`", expr.data, expr.data.pretty_print(ctx));   
                },
                Unknown => panic!("Use of uninitialized value."),
                ref v => panic!("Use of wrong type of value {}", v),
            }
        },
    }
}

fn interpret_expr(expr: &Expr, ctx: &mut Context<TypedValue>) -> TypedValue {
    use self::Expr::*;
    use parser::ast::BinOp::*;
    use parser::ast::UnaOp::*;
    match *expr {
        ErrExpr(ref e) => panic!("Invalid expression: {:?}", e),
        BinOper {
            ref lhs,
            ref op,
            ref rhs,
        } => {
            let lhs = interpret_opnd(&lhs.data, ctx);
            let rhs = interpret_opnd(&rhs.data, ctx);
            TypedValue::from_value(match *op {
                Equality => Bool(match (lhs.value(), rhs.value()) {
                    (&Integer(ref i1), &Integer(ref i2)) => i1 == i2,
                    (&Bool(ref b1), &Bool(ref b2)) => b1 == b2,
                    (&Str(ref s1), &Str(ref s2)) => s1 == s2,
                    (lhs, rhs) => panic!("`{}` cannot be compared with `{}` for equality.", lhs, rhs),
                }),
                LessThan => Bool(match (lhs.value(), rhs.value()) {
                    (&Integer(ref i1), &Integer(ref i2)) => i1 < i2,
                    (&Bool(ref b1), &Bool(ref b2)) => b1 < b2,
                    (&Str(ref s1), &Str(ref s2)) => s1 < s2,
                    (lhs, rhs) => panic!("`{}` cannot be compared with `{}` for ordering.", lhs, rhs),
                }),
                Addition => match *lhs.ty() {
                    Ty::Integer => Integer(lhs.integer() + rhs.integer()),
                    Ty::Str => Str(lhs.string().to_owned() + rhs.string()),
                    ref t => panic!("Cannot add type: `{}`.", t),
                }
                Substraction => Integer(lhs.integer() - rhs.integer()),
                Multiplication => Integer(lhs.integer() * rhs.integer()),
                Division => Integer(lhs.integer() / rhs.integer()),
                And => Bool(lhs.boolean() & rhs.boolean()),
            })
        },
        UnaOper {
            ref op,
            ref rhs,
        } => {
            let rhs = interpret_opnd(&rhs.data, ctx);
            match *op {
                Not => TypedValue::from_value(Value::Bool(!rhs.boolean())),
            }
        },
        Opnd(ref opnd) => interpret_opnd(&opnd.data, ctx),
    }
}

fn interpret_opnd(opnd: &Opnd, ctx: &mut Context<TypedValue>) -> TypedValue {
    use self::Opnd::*;
    match *opnd {
        OpndErr(ref e) => panic!("Error: {:?}", e),
        Int(ref i) => TypedValue::from_value(Value::Integer(i.clone())),
        StrLit(ref s) => TypedValue::from_value(Value::Str(s.clone())),
        Ident(ref ident) => ctx.get(ident).expect(&format!("Tried to use undeclared variable: `{}`.", ident)).clone(),
        Expr(ref expr) => interpret_expr(&expr.data, ctx),
    }
}