use num_bigint::BigInt;

use std::fmt;

use Ident;
use lexer::tokens::Operator;

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Declaration {
        ident: Ident,
        ty: Type,
        value: Option<Expr>,
    },
    Assignment {
        ident: Ident,
        value: Expr,
    },
    Loop {
        ident: Ident,
        from: Expr,
        to: Expr,
        stmts: Vec<Stmt>,
    },
    Read {
        ident: Ident,
    },
    Print {
        expr: Expr,
    },
    Assert {
        expr: Expr,
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    BinOper {
        lhs: Opnd,
        op: BinOp,
        rhs: Opnd,
    },
    UnaOper {
        op: UnaOp,
        rhs: Opnd,
    },
    Opnd(Opnd),
}

#[derive(Clone, PartialEq)]
pub enum Opnd {
    Int(BigInt),
    StrLit(String),
    Ident(Ident),
    Expr(Box<Expr>),
}

impl fmt::Debug for Opnd {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::Opnd::*;
        match *self {
            Int(ref i) => {
                fmt.write_str("Int(")?;
                i.fmt(fmt)?;
                fmt.write_str(")")
            },
            StrLit(ref s) => {
                fmt.write_str("StrLit(")?;
                s.fmt(fmt)?;
                fmt.write_str(")")
            },
            Ident(ref i) => {
                fmt.write_str("Ident(")?;
                i.fmt(fmt)?;
                fmt.write_str(")")
            },
            Expr(ref e) => {
                fmt.write_str("Expr(")?;
                e.fmt(fmt)?;
                fmt.write_str(")")
            },
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinOp {
    Equality,
    LessThan,
    Addition,
    Substraction,
    Multiplication,
    Division,
    And,
}

impl BinOp {
    pub fn from_oper(o: &Operator) -> Option<Self> {
        use self::Operator::*;
        Some(match *o {
            Equality => BinOp::Equality,
            LessThan => BinOp::LessThan,
            Addition => BinOp::Addition,
            Substraction => BinOp::Substraction,
            Multiplication => BinOp::Multiplication,
            Division => BinOp::Division,
            And => BinOp::And,
            _ => None?,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaOp {
    Not,
}

impl UnaOp {
    pub fn from_oper(o: &Operator) -> Option<Self> {
        use self::Operator::*;
        Some(match *o {
            Not => UnaOp::Not,
            _ => None?,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Integer,
    Str,
    Bool
}