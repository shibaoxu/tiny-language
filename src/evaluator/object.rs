use std::any::{Any, TypeId};
use std::fmt::{Display, format};
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
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct IntegerObject {
    pub value: i64,
}

impl Object for IntegerObject {
    fn object_type(&self) -> ObjectType {
        ObjectType::Integer
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

impl IntegerObject {
    pub fn from(value: i64) -> Self {
        Self {
            value
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct BooleanObject {
    pub value: bool,
}

impl Object for BooleanObject {
    fn object_type(&self) -> ObjectType {
        Boolean
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

impl BooleanObject {
    pub fn from(value: bool) -> Self {
        Self { value }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct NullObject;

impl Object for NullObject {
    fn object_type(&self) -> ObjectType {
        Null
    }

    fn inspect(&self) -> String {
        "null".to_string()
    }
}


