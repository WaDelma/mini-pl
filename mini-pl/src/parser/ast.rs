use num_bigint::BigInt;

use parsco::FromErr;

use std::fmt;

use Ident;
use lexer::tokens::{Operator, Position, Keyword, Side};

#[derive(Clone, PartialEq)]
pub struct Positioned<T> {
    pub data: T,
    pub from: Position,
    pub to: Position,
}

impl<T: fmt::Debug> fmt::Debug for Positioned<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        // TODO: self.data should use formating args...
        write!(fmt, "{:#?} at {:?}..{:?}", self.data, self.from, self.to)
    }
}

impl<T> Positioned<T> {
    pub fn new(data: T, from: Position, to: Position) -> Self {
        Positioned {
            data,
            from,
            to
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    ErrStmt(ParseError),
    Declaration {
        ident: Ident,
        ty: Type,
        value: Option<Positioned<Expr>>,
    },
    Assignment {
        ident: Ident,
        value: Positioned<Expr>,
    },
    Loop {
        ident: Ident,
        from: Positioned<Expr>,
        to: Positioned<Expr>,
        stmts: Vec<Positioned<Stmt>>,
    },
    Read {
        ident: Ident,
    },
    Print {
        expr: Positioned<Expr>,
    },
    Assert {
        expr: Positioned<Expr>,
    }
}

pub struct Expression {

}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    ErrExpr(ExprError),
    BinOper {
        lhs: Positioned<Opnd>,
        op: BinOp,
        rhs: Positioned<Opnd>,
    },
    UnaOper {
        op: UnaOp,
        rhs: Positioned<Opnd>,
    },
    Opnd(Positioned<Opnd>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprError {
    MissingParenthesis(Side),
}

impl fmt::Display for Expr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::Expr::*;
        fmt.write_str(&match *self {
            ErrExpr(ref e) => panic!("Invalid expression: {:?}", e),
            BinOper {
                ref lhs,
                ref op,
                ref rhs,
            } => {
                format!("{} {} {}", lhs.data, op, rhs.data)
            },
            UnaOper {
                ref op,
                ref rhs,
            } => {
                format!("{}{}", op, rhs.data)
            },
            Opnd(ref opnd) => format!("({})", opnd.data),
        })
    }
}

#[derive(Clone, PartialEq)]
pub enum Opnd {
    OpndErr(OpndError),
    Int(BigInt),
    StrLit(String),
    Ident(Ident),
    Expr(Box<Positioned<Expr>>),
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
            OpndErr(ref e) => format!("{:?}", e), // TODO: Proper error message?
            Int(ref i) => i.to_string(),
            StrLit(ref s) => s.to_string(),
            Ident(ref i) => i.to_string(),
            Expr(ref expr) => format!("{}", expr.data),
        })
    }
}

impl fmt::Debug for Opnd {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::Opnd::*;
        match *self {
            OpndErr(ref e) => {
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
    TypeErr(TypeError),
    Integer,
    Str,
    Bool
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeError {
    UnknownType(String),
    KeywordNotType(Keyword),
    NoTypeAnnotation
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    Unknown,
    MissingSemicolon,
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