//! Interpretation time representation of mini-pl

use num_bigint::BigInt;

use std::fmt;

use parser::ast::{Type, Expr, Opnd};
use util::context::Context;

impl Expr {
    /// Pretty prints expression in given interpretion time context
    pub fn pretty_print(&self, ctx: &Context<TypedValue>) -> String {
        use self::Expr::*;
        match *self {
            ErrExpr(ref e) => panic!("Invalid expression: {:?}", e),
            BinOper { ref lhs, ref op, ref rhs } => {
                format!("{} {} {}", lhs.data.pretty_print(ctx), op, rhs.data.pretty_print(ctx))
            },
            UnaOper { ref op, ref rhs } => {
                format!("{}{}", op, rhs.data.pretty_print(ctx))
            },
            Opnd(ref opnd) => format!("({})", opnd.data.pretty_print(ctx)),
        }
    }
}

impl Opnd {
    /// Pretty prints operand in given interpretion time context
    pub fn pretty_print(&self, ctx: &Context<TypedValue>) -> String {
        use self::Opnd::*;
        match *self {
            OpndErr(ref e) => panic!("Error: {:?}", e),
            Int(ref i) => i.to_string(),
            StrLit(ref s) => s.to_string(),
            Ident(ref i) => format!("{}", ctx.get(i).unwrap().value()),
            Expr(ref expr) => expr.data.pretty_print(ctx),
        }
    }
}

/// Value with it's type
#[derive(Clone, PartialEq, Debug)]
pub struct TypedValue {
    value: Value,
    ty: Ty,
}

impl TypedValue {
    /// Creates new typed value that has types default value.
    /// 
    /// Default values:
    /// 
    ///   - Integer: 0
    ///   - String: ""
    ///   - Boolean: false
    pub fn default(ty: Ty) -> Self {
        use self::Ty::*;
        TypedValue {
            value: match ty {
                Integer => Value::Integer(0.into()),
                Str => Value::Str("".into()),
                Bool => Value::Bool(false),
            },
            ty,
        }
    }

    /// Creates new typed value
    /// 
    /// `None` is returned if value is not compatible with the type
    pub fn new(value: Value, ty: Ty) -> Option<Self> {
        let mut result = TypedValue {
            value: Value::Unknown,
            ty,
        };
        if result.set(value) {
            Some(result)
        } else {
            None
        }
    }

    /// Creates typed value from value
    /// 
    /// If type cannot be guessed, panics instead
    pub fn from_value(value: Value) -> Self {
        use self::Value::*;
        TypedValue {
            ty: match value {
                Integer(_) => Ty::Integer,
                Bool(_) => Ty::Bool,
                Str(_) => Ty::Str,
                Unknown => panic!("Cannot determine type of unkown value."),
            },
            value: value,
        }
    }

    /// Sets value from another typed value
    /// 
    /// Return true if types matched and setting was possible
    pub fn set_typed(&mut self, value: TypedValue) -> bool {
        if self.ty == value.ty {
            self.value = value.value;
            true
        } else {
            false
        }
    }

    /// Sets the value
    ///
    /// If type of value wasn't compatible, returns false
    pub fn set(&mut self, value: Value) -> bool {
        use self::Value as V;
        if match (&value, &self.ty) {
            (&V::Integer(_), &Ty::Integer) => true,
            (&V::Str(_), &Ty::Str) => true,
            (&V::Bool(_), &Ty::Bool) => true,
            (&V::Unknown, _) => true,
            _ => false,
        } {
            self.value = value;
            true
        } else {
            false
        }
    }

    /// Immutable getter for the type
    pub fn ty(&self) -> &Ty {
        &self.ty
    }

    /// Immutable getter for the value
    pub fn value(&self) -> &Value {
        &self.value
    }

    /// Gets integer value or panics
    pub fn integer(&self) -> &BigInt {
        use self::Value::*;
        match self.value {
            Integer(ref i) => i,
            Unknown => panic!("Use of uninitialized value."),
            ref v => panic!("Use of wrong type of value `{}`.", v),
        }
    }

    /// Gets boolean value or panics
    pub fn boolean(&self) -> &bool {
        use self::Value::*;
        match self.value {
            Bool(ref b) => b,
            Unknown => panic!("Use of uninitialized value."),
            ref v => panic!("Use of wrong type of value `{}`.", v),
        }
    }

    /// Gets string value or panics
    pub fn string(&self) -> &str {
        use self::Value::*;
        match self.value {
            Str(ref s) => s,
            Unknown => panic!("Use of uninitialized value."),
            ref v => panic!("Use of wrong type of value `{}`.", v),
        }
    }
}

impl fmt::Display for TypedValue {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::Ty::*;
        match self.ty {
            Integer => fmt.write_str(&format!("{}: Integer", self.value)),
            Str => fmt.write_str(&format!("{}: Str", self.value)),
            Bool => fmt.write_str(&format!("{}: Bool", self.value)),
        }
    }
}

/// Internal interpreter representation of value
#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    /// Unknown value
    Unknown,
    /// Integer value
    Integer(BigInt),
    /// String value
    Str(String),
    /// Boolean value
    Bool(bool),
}

impl fmt::Display for Value {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::Value::*;
        match *self {
            Unknown => fmt.write_str("<unknown>"),
            Integer(ref i) => fmt.write_str(&format!("{}", i)),
            Str(ref s) => fmt.write_str(&format!("{}", s)),
            Bool(ref b) => fmt.write_str(&format!("{}", b)),
        }
    }
}

/// Interpreter representation of type
#[derive(Clone, PartialEq, Debug)]
pub enum Ty {
    /// Integer type
    Integer,
    /// String type
    Str,
    /// Boolean type
    Bool,
}

impl fmt::Display for Ty {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::Ty::*;
        match *self {
            Integer => fmt.write_str("int"),
            Str => fmt.write_str("string"),
            Bool => fmt.write_str("bool"),
        }
    }
}

impl From<Type> for Ty {
    fn from(ty: Type) -> Self {
        use self::Type::*;
        match ty {
            TypeErr(e) => panic!("Unknown type: {:?}", e),
            Integer => Ty::Integer,
            Str => Ty::Str,
            Bool => Ty::Bool,
        }
    }
}