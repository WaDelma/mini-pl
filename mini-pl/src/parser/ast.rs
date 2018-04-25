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

/// The whole program
#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    /// Name of the program
    pub name: Positioned<Ident>,
    /// List of defined functions
    pub functions: Vec<Positioned<Function>>,
    /// Statements run when starting the program
    pub stmts: Positioned<Stmt>,
}

/// A single function
#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    /// Name of the function
    pub name: Positioned<Ident>,
    /// List of function parameters
    pub params: Vec<Positioned<Parameter>>,
    /// Result type of the function
    /// 
    /// If None then the function was a procedure
    pub result: Option<Positioned<Type>>,
    /// Body of the function
    pub stmts: Positioned<Stmt>,
}

/// A single parameter
#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
    /// Way that the parameter is refered to
    pub by: AccessBy,
    /// Name of the parameter
    pub name: Positioned<Ident>,
    /// Type of the parameter
    pub ty: Positioned<Type>,
}

/// Way of refering function parameters
#[derive(Clone, Debug, PartialEq)]
pub enum AccessBy {
    /// The parameter is accessed by value
    /// 
    /// This means that a deep copy is made of it.
    Value,
    /// The parameter is accessed by a reference
    /// 
    /// This means parameter will be just a pointer to the actual value.
    Reference,
}

/// Statement nodes
/// 
/// Also contains node for errors that can happen while parsing statement
#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    /// An error that can happen while parsing statements
    ErrStmt(StmtError),
    /// Declaration of variables
    Declaration {
        /// Name of variables
        idents: Vec<Positioned<Ident>>,
        /// Type of the variables
        ty: Type,
    },
    /// Variable assignment
    Assignment {
        /// Name of the variable
        ident: Ident,
        /// Expression which value will be assigned to the variable
        value: Positioned<Expr>,
    },
    /// While-loop
    Loop {
        /// Condition that determines if the loop body is executed
        cond: Positioned<Expr>,
        /// Loop body
        body: Box<Positioned<Stmt>>,
    },
    /// Scoped block
    Block {
        /// Statements of the block
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
    /// Real type
    Real,
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
        if let Token::Identifier(ref tok) = *tok {
            match &**tok {
                "integer" => Type::Integer,
                "Boolean" => Type::Bool,
                "string" => Type::Str,
                "real" => Type::Real,
                _ => Type::TypeErr(UnknownType(tok.clone())),
            }
        } else {
            Type::TypeErr(InvalidToken(tok.clone()))
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