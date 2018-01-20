use std::fmt;

use parser::ast::Type;

#[derive(Clone, PartialEq)]
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

#[derive(Clone, PartialEq)]
pub enum Value {
    Unknown,
    Integer(i64),
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

#[derive(Clone, PartialEq)]
pub enum Ty {
    Integer,
    Str,
    Bool,
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