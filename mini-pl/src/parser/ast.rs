use num_bigint::BigInt;

use parsco::FromErr;

use std::fmt;

use Ident;
use lexer::tokens::{Operator, Position};

#[derive(Clone, Debug, PartialEq)]
pub struct Statement {
    stmt: Stmt,
    from: Position,
    to: Position,
}

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

impl fmt::Display for Expr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::Expr::*;
        fmt.write_str(&match *self {
            BinOper {
                ref lhs,
                ref op,
                ref rhs,
            } => {
                format!("{} {} {}", lhs, op, rhs)
            },
            UnaOper {
                ref op,
                ref rhs,
            } => {
                format!("{}{}", op, rhs)
            },
            Opnd(ref opnd) => format!("({})", opnd),
        })
    }
}

#[derive(Clone, PartialEq)]
pub enum Opnd {
    Err(OpndError),
    Int(BigInt),
    StrLit(String),
    Ident(Ident),
    Expr(Box<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum OpndError {
    MissingEndParenthesis,
    InvalidOperand,
}

impl fmt::Display for Opnd {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::Opnd::*;
        fmt.write_str(&match *self {
            Err(ref e) => format!("{:?}", e), // TODO: Proper error message?
            Int(ref i) => i.to_string(),
            StrLit(ref s) => s.to_string(),
            Ident(ref i) => i.to_string(),
            Expr(ref expr) => format!("{}", expr),
        })
    }
}

impl fmt::Debug for Opnd {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::Opnd::*;
        match *self {
            Err(ref e) => {
                fmt.write_str("Int(")?;
                e.fmt(fmt)?;
                fmt.write_str(")")
            }
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

impl fmt::Display for BinOp {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::BinOp::*;
        fmt.write_str(match *self {
            Equality => "=",
            LessThan => "<",
            Addition => "+",
            Substraction => "-",
            Multiplication => "*",
            Division => "/",
            And => "&",
        })
    }
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

impl fmt::Display for UnaOp {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::UnaOp::*;
        fmt.write_str(match *self {
            Not => "!",
        })
    }
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

impl FromErr<::parsco::common::Void> for ParseError {
    fn from(_: ::parsco::common::Void) -> Self {
        unreachable!("Void is never type.")
    }
}