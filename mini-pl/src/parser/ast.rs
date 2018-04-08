//! Ast of mini-pl
//! 
//! Ast nodes are grouped to different enums to give more structured types instead of one flat one.
//! This grouping is especially useful for defining expression sub-trees.
use num_bigint::BigInt;

use parsco::FromErr;

use std::fmt;

use Ident;
use lexer::tokens::{Token, Operator, Keyword, Side};
use util::Positioned;

pub struct Program {
    name: Positioned<Ident>,
    functions: Vec<Positioned<Function>>,
    stmts: Vec<Positioned<Stmt>>,
}

pub struct Function {
    name: Positioned<Ident>,
    params: Vec<Positioned<Parameter>>,
    result: Option<Positioned<Type>>,
    stmts: Vec<Positioned<Stmt>>,
}

pub struct Parameter {
    by: AccessBy,
    name: Positioned<Ident>,
    ty: Positioned<Type>,
}

pub enum AccessBy {
    Value,
    Reference,
}

/// Statement nodes
/// 
/// Also contains node for errors that can happen while parsing statement
#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    /// An error that can happen while parsing statements
    ErrStmt(StmtError),
    /// Variable declaration
    Declaration {
        /// Name of the variable
        ident: Ident,
        /// Type of the variable
        ty: Type,
        /// Declaration may or might not assign expression as it's value
        value: Option<Positioned<Expr>>,
    },
    /// Variable assignment
    Assignment {
        /// Name of the variable
        ident: Ident,
        /// Expression which value will be assigned to the variable
        value: Positioned<Expr>,
    },
    /// For-loop
    Loop {
        /// Name of the loop control variable
        ident: Ident,
        /// Expression that determines the starting point of range iteration
        from: Positioned<Expr>,
        /// Expression that determines the ending point of range iteration
        to: Positioned<Expr>,
        /// The body of the for-loop
        stmts: Vec<Positioned<Stmt>>,
    },
    /// Read from stdin
    Read {
        /// Variable that the value is read to
        ident: Ident,
    },
    /// Print to stdout
    Print {
        /// Expression which value will be printed out
        expr: Positioned<Expr>,
    },
    /// Condition assertion
    Assert {
        /// Expression which value will be asserted
        expr: Positioned<Expr>,
    }
}

/// Errors that can happen while parsing statement
#[derive(Debug, Clone, PartialEq)]
pub enum StmtError {
    /// Unknown error
    Unknown,
    /// Invalid assignment operator
    InvalidAssignment,
    /// Semicolon missing at the end of statement
    // TODO: Remove statement from this and handle other errors overlapping with missing semicolon one better.
    MissingSemicolon(Box<Stmt>),
}

impl FromErr<()> for StmtError {
    fn from(_: ()) -> Self {
        StmtError::Unknown
    }
}

impl FromErr<StmtError> for StmtError {
    fn from(l: StmtError) -> Self {
        l
    }
}

impl FromErr<::parsco::common::Void> for StmtError {
    fn from(_: ::parsco::common::Void) -> Self {
        unreachable!("Void is never type.")
    }
}

/// Expression nodes
/// 
/// Also contains node for errors that can happen while parsing expression
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    /// Errors that can happen while parsing expressions
    ErrExpr(ExprError),
    /// Binary operation
    BinOper {
        /// Left-hand side of the operator
        lhs: Positioned<Opnd>,
        /// Binary operator
        op: BinOp,
        /// Right-hand side of the operator
        rhs: Positioned<Opnd>,
    },
    /// Unary operation
    UnaOper {
        /// Unary operator
        op: UnaOp,
        /// Right-hand side of the operator
        rhs: Positioned<Opnd>,
    },
    /// Free standing operand
    Opnd(Positioned<Opnd>),
}

/// Errors that can happen while parsing expressions
#[derive(Clone, Debug, PartialEq)]
pub enum ExprError {
    /// Expression is missing parenthesis
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

/// Operand nodes
/// 
/// Also contains node for errors that can happen while parsing operand
#[derive(Clone, PartialEq)]
pub enum Opnd {
    /// Errors that can happen while parsing operands
    OpndErr(OpndError),
    /// Integer literal
    Int(BigInt),
    /// String literal
    StrLit(String),
    /// Variable identifier
    Ident(Ident),
    /// Operand can contain recursively expressions
    Expr(Box<Positioned<Expr>>),
}

/// Errors that can happen while parsing operands
#[derive(Clone, Debug, PartialEq)]
pub enum OpndError {
    /// Operand is missing end parenthesis
    MissingEndParenthesis,
    /// Operand is invalid
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
                fmt.write_str("Err(")?;
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

/// Binary operators
#[derive(Clone, Debug, PartialEq)]
pub enum BinOp {
    /// Equality comparison
    Equality,
    /// Less-than comparison
    LessThan,
    /// Addition/concanation operator
    Addition,
    /// Substraction operator
    Substraction,
    /// Multiplication operator
    Multiplication,
    /// Division operator
    Division,
    /// And operator
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
    /// Create binary operator from operator token.
    /// 
    /// Returns `None` if token wasn't binary operator
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

/// Unary operators
#[derive(Clone, Debug, PartialEq)]
pub enum UnaOp {
    /// Negation operator
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
    /// Create unary operator from operator token.
    /// 
    /// Returns `None` if token wasn't unary operator
    pub fn from_oper(o: &Operator) -> Option<Self> {
        use self::Operator::*;
        Some(match *o {
            Not => UnaOp::Not,
            _ => None?,
        })
    }
}

/// Possible types
/// 
/// Also contains node for errors that can happen while parsing types
#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    /// Errors that can happen while parsing type
    TypeErr(TypeError),
    /// Integer type
    Integer,
    /// String type
    Str,
    /// Boolean type
    Bool
}

impl Type {
    /// Create type from token
    /// 
    /// If the token wasn't type, returns type error token
    pub fn from_token(tok: &Token) -> Self {
        use self::Keyword::*;
        use self::TypeError::*;
        if let Token::Keyword(ref tok) = *tok {
            match *tok {
                Int => Type::Integer,
                Bool => Type::Bool,
                Str => Type::Str,
                ref k => Type::TypeErr(KeywordNotType(k.clone())),
            }
        } else {
            match *tok {
                Token::Identifier(ref i) => Type::TypeErr(UnknownType(i.clone())),
                _ => Type::TypeErr(InvalidToken(tok.clone())),
            }
        }
        
    }
}

/// Errors that can happen while parsing type
#[derive(Clone, Debug, PartialEq)]
pub enum TypeError {
    /// Invalid token while parsing type
    InvalidToken(Token),
    /// Unknown type parsed
    UnknownType(String),
    /// Keyword that isn't type
    KeywordNotType(Keyword),
    /// No type annotation present
    NoTypeAnnotation
}