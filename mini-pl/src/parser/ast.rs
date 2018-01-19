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
    BinOp {
        lhs: Opnd,
        op: Op,
        rhs: Opnd,
    },
    UnaOp {
        op: Op,
        rhs: Opnd,
    },
    Opnd(Opnd),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Opnd {
    Int(i64),
    StrLit(String),
    Ident(Ident),
    Expr(Box<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Op {
    Multiplication,
    Addition,
    Substraction,
    Equality,
}

impl Op {
    pub fn from_oper(o: &Operator) -> Option<Op> {
        use self::Operator::*;
        Some(match *o {
            Addition => Op::Addition,
            Multiplication => Op::Multiplication,
            Substraction => Op::Substraction,
            Equality => Op::Equality,
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