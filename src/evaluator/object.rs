use std::collections::BTreeMap;
use std::fmt::{Display,Formatter};
use crate::ast::{BlockStatement, Expression};
use anyhow::{Result, Error, format_err};

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum ObjectType {
    Int,
    Bool,
    String,
    Null,
    Fun,
    Builtin,
    Array,
    Hashmap,
}

impl Display for ObjectType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ObjectType::Int => "INTEGER",
            ObjectType::Bool => "BOOLEAN",
            ObjectType::String => "STRING",
            ObjectType::Null => "NULL",
            ObjectType::Fun => "FUNCTION",
            ObjectType::Builtin => "BUILTIN",
            ObjectType::Array => "ARRAY",
            ObjectType::Hashmap => "HASHMAP"
        };
        f.write_str(s)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum WrappedValue {
    IntValue(i64),
    BoolValue(bool),
    StrValue(String),
    NullValue(NullValue),
    FunValue(Function),
    BuiltinValue(BuiltinFunction),
    ArrayValue(Vec<Value>),
    HashMapValue(BTreeMap<Value, Value>),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct NullValue;

impl Display for NullValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("NULL")
    }
}


#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Function {
    pub parameters: Vec<Expression>,
    pub body: BlockStatement,
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let arguments = self.parameters.iter().map(|e| e.to_string()).collect::<Vec<_>>().join(",");
        let body = self.body.statements.iter().map(|e| e.to_string()).collect::<String>();
        f.write_str(&format!("fn({}){{{}}}", arguments, body))
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct BuiltinFunction(pub String);

impl Display for BuiltinFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Value {
    pub value: WrappedValue,
    pub is_return: bool,
    pub type_of: ObjectType,
}

impl Value {
    pub fn set_return(&mut self) {
        self.is_return = true
    }

    pub fn inspect(&self) -> String {
        match &self.value {
            WrappedValue::IntValue(v) => format!("{}", v),
            WrappedValue::BoolValue(v) => format!("{}", v),
            WrappedValue::StrValue(v) => format!("{}", v),
            WrappedValue::NullValue(v) => format!("{}", v),
            WrappedValue::FunValue(v) => format!("{}", v),
            WrappedValue::BuiltinValue(v) => format!("{}", v),
            WrappedValue::ArrayValue(v) => {
                let s = v.iter().map(|e| e.inspect()).collect::<Vec<_>>().join(",");
                format!("[{}]", s)
            }
            WrappedValue::HashMapValue(v) => {
                let s = v.iter()
                    .map(|(key, value)| format!("{} : {}", key.inspect(), value.inspect()))
                    .collect::<Vec<_>>()
                    .join(",");
                format!("{{{}}}", s)
            }
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self.type_of {
            ObjectType::Bool => bool::try_from(self).unwrap(),
            ObjectType::Null => false,
            _ => true,
        }
    }
}

impl From<i64> for Value {
    fn from(v: i64) -> Self {
        Self { value: WrappedValue::IntValue(v), is_return: false, type_of: ObjectType::Int }
    }
}

impl From<bool> for Value {
    fn from(v: bool) -> Self {
        Self { value: WrappedValue::BoolValue(v), is_return: false, type_of: ObjectType::Bool }
    }
}

impl From<String> for Value {
    fn from(v: String) -> Self {
        Self { value: WrappedValue::StrValue(v), is_return: false, type_of: ObjectType::String }
    }
}

impl From<&str> for Value {
    fn from(v: &str) -> Self {
        Self { value: WrappedValue::StrValue(v.to_string()), is_return: false, type_of: ObjectType::String }
    }
}

impl From<NullValue> for Value {
    fn from(v: NullValue) -> Self {
        Self { value: WrappedValue::NullValue(v), is_return: false, type_of: ObjectType::Null }
    }
}

impl From<Function> for Value {
    fn from(v: Function) -> Self {
        Self { value: WrappedValue::FunValue(v), is_return: false, type_of: ObjectType::Fun }
    }
}

impl From<BuiltinFunction> for Value {
    fn from(v: BuiltinFunction) -> Self {
        Self { value: WrappedValue::BuiltinValue(v), is_return: false, type_of: ObjectType::Builtin }
    }
}

impl From<Vec<Value>> for Value {
    fn from(v: Vec<Value>) -> Self {
        Self { value: WrappedValue::ArrayValue(v), is_return: false, type_of: ObjectType::Array }
    }
}

impl From<BTreeMap<Value, Value>> for Value {
    fn from(v: BTreeMap<Value, Value>) -> Self {
        Self {
            value: WrappedValue::HashMapValue(v),
            is_return: false,
            type_of: ObjectType::Hashmap,
        }
    }
}

impl TryFrom<&Value> for i64 {
    type Error = Error;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        if let WrappedValue::IntValue(v) = value.value {
            Ok(v)
        } else {
            Err(format_err!("can not convert `{:?}` to `i64`", value))
        }
    }
}

impl TryFrom<&Value> for bool {
    type Error = Error;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        if let WrappedValue::BoolValue(v) = value.value {
            Ok(v)
        } else {
            Err(format_err!("can not convert `{:?}` to `bool`", value))
        }
    }
}

impl TryFrom<&Value> for String {
    type Error = Error;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        if let WrappedValue::StrValue(v) = value.value.clone() {
            Ok(v)
        } else {
            Err(format_err!("can not convert `{:?}` to `String`", value))
        }
    }
}

impl TryFrom<&Value> for NullValue {
    type Error = Error;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        if let WrappedValue::NullValue(v) = value.value.clone() {
            Ok(v)
        } else {
            Err(format_err!("can not convert `{:?}` to `NullValue`", value))
        }
    }
}

impl TryFrom<&Value> for Function {
    type Error = Error;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        if let WrappedValue::FunValue(v) = value.value.clone() {
            Ok(v)
        } else {
            Err(format_err!("can not convert `{:?}` to `Function`", value))
        }
    }
}

impl TryFrom<&Value> for BuiltinFunction {
    type Error = Error;

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        if let WrappedValue::BuiltinValue(v) = value.value.clone() {
            Ok(v)
        } else {
            Err(format_err!("can not convert `{:?}` to `BuiltinFunction`", value))
        }
    }
}

impl TryFrom<&Value> for Vec<Value> {
    type Error = Error;

    fn try_from(value: &Value) -> std::result::Result<Self, Self::Error> {
        if let WrappedValue::ArrayValue(v) = value.value.clone() {
            Ok(v)
        } else {
            Err(format_err!("can not convert `{:?}` to `Array`", value))
        }
    }
}

impl TryFrom<&Value> for BTreeMap<Value, Value> {
    type Error = Error;

    fn try_from(value: &Value) -> std::result::Result<Self, Self::Error> {
        if let WrappedValue::HashMapValue(v) = value.value.clone(){
            Ok(v)
        }else{
            Err(format_err!("can not convert `{:?}` to `Hashmap`", value))
        }
    }
}