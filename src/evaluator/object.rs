use std::any::{Any, TypeId};
use std::fmt::{Debug, Display, format, Formatter};
use std::marker::PhantomData;
use crate::evaluator::object::ObjectType::{Boolean, Null};
use crate::TokenType;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ObjectType {
    Integer,
    Boolean,
    Null,
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