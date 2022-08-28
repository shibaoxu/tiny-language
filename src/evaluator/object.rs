use std::any::Any;
use std::fmt::{Debug, Display, Formatter};

use crate::evaluator::object::ObjectType::{Boolean, Error, Integer, Null};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ObjectType {
    Integer,
    Boolean,
    Null,
    Error,
}

impl Display for ObjectType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self{
            Integer => "INTEGER",
            Boolean => "BOOLEAN",
            Null => "NULL",
            Error => "ERROR"
        })
    }
}

pub trait Object: Any {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
    fn is_return(&self) -> bool;
    fn set_return(&mut self);
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct IntegerObject {
    pub value: i64,
    is_return: bool,
}

impl Object for IntegerObject {
    fn object_type(&self) -> ObjectType {
        ObjectType::Integer
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }

    fn is_return(&self) -> bool {
        self.is_return
    }

    fn set_return(&mut self) {
        self.is_return = true;
    }
}

impl IntegerObject {
    pub fn from(value: i64) -> Self {
        Self {
            value,
            is_return: false,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct BooleanObject {
    pub value: bool,
    pub is_return: bool,
}

impl Object for BooleanObject {
    fn object_type(&self) -> ObjectType {
        Boolean
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }

    fn is_return(&self) -> bool {
        self.is_return
    }

    fn set_return(&mut self) {
        self.is_return = true;
    }
}

impl BooleanObject {
    pub fn from(value: bool) -> Self {
        Self { value, is_return: false }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct NullObject(pub bool);

impl Object for NullObject {
    fn object_type(&self) -> ObjectType {
        Null
    }

    fn inspect(&self) -> String {
        "null".to_string()
    }

    fn is_return(&self) -> bool {
        self.0
    }

    fn set_return(&mut self) {
        self.0 = true;
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ErrorObject{
    pub message: String,
}

impl Object for ErrorObject {
    fn object_type(&self) -> ObjectType {
        ObjectType::Error
    }

    fn inspect(&self) -> String {
        format!("{}", self.message)
    }

    fn is_return(&self) -> bool {
        true
    }

    fn set_return(&mut self) {}
}

impl ErrorObject {
    pub fn from(message: &str) ->Self{
        Self{
            message: message.to_string()
        }
    }
}