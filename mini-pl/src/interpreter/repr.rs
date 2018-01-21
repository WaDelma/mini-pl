use num_bigint::BigInt;

use std::fmt;

use parser::ast::{Type, Expr, Opnd};
use super::context::Context;

impl Expr {
    pub fn pretty_print(&self, ctx: &Context<TypedValue>) -> String {
        use self::Expr::*;
        match *self {
            BinOper {
                ref lhs,
                ref op,
                ref rhs,
            } => {
                format!("{} {} {}", lhs.pretty_print(ctx), op, rhs.pretty_print(ctx))
            },
            UnaOper {
                ref op,
                ref rhs,
            } => {
                format!("{}{}", op, rhs.pretty_print(ctx))
            },
            Opnd(ref opnd) => format!("({})", opnd.pretty_print(ctx)),
        }
    }
}

impl Opnd {
    pub fn pretty_print(&self, ctx: &Context<TypedValue>) -> String {
        use self::Opnd::*;
        match *self {
            Int(ref i) => i.to_string(),
            StrLit(ref s) => s.to_string(),
            Ident(ref i) => format!("{}", ctx.get(i).unwrap().value()),
            Expr(ref expr) => expr.pretty_print(ctx),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct TypedValue {
    value: Value,
    ty: Ty,
}

impl TypedValue {
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

    pub fn set_typed(&mut self, value: TypedValue) -> bool {
        if self.ty == value.ty {
            self.value = value.value;
            true
        } else {
            false
        }
    }

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

    pub fn ty(&self) -> &Ty {
        &self.ty
    }

    pub fn value(&self) -> &Value {
        &self.value
    }

    pub fn integer(&self) -> &BigInt {
        use self::Value::*;
        match self.value {
            Integer(ref i) => i,
            Unknown => panic!("Use of uninitialized value."),
            ref v => panic!("Use of wrong type of value `{}`.", v),
        }
    }

    pub fn boolean(&self) -> &bool {
        use self::Value::*;
        match self.value {
            Bool(ref b) => b,
            Unknown => panic!("Use of uninitialized value."),
            ref v => panic!("Use of wrong type of value `{}`.", v),
        }
    }

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

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    Unknown,
    Integer(BigInt),
    Str(String),
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

#[derive(Clone, PartialEq, Debug)]
pub enum Ty {
    Integer,
    Str,
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
            Integer => Ty::Integer,
            Str => Ty::Str,
            Bool => Ty::Bool,
        }
    }
}