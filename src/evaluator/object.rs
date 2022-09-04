use std::any::Any;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;
use crate::ast::{BlockStatement, FunctionLiteral, Identifier};
use crate::evaluator::environment::Environment;

use crate::evaluator::object::ObjectType::{Boolean, Error, Integer, Null};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ObjectType {
    Integer,
    Boolean,
    Null,
    Error,
    Function,
}

impl Display for ObjectType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Integer => "INTEGER",
            Boolean => "BOOLEAN",
            Null => "NULL",
            Error => "ERROR",
            ObjectType::Function => "FUNCTION"
        })
    }
}

pub trait Object: Any {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
    fn is_return(&self) -> bool;
    fn set_return(&mut self);
}

#[derive(Debug, Clone, Eq, PartialEq)]
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

#[derive(Debug, Clone, Eq, PartialEq)]
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

#[derive(Debug, Clone, Eq, PartialEq)]
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ErrorObject {
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
    pub fn from(message: &str) -> Self {
        Self {
            message: message.to_string()
        }
    }
}

pub struct FunctionObject<'a> {
    pub parameters: Vec<Identifier>,
    pub body: Rc<BlockStatement>,
    pub env: Environment<'a>,
    pub is_return: bool,
}

impl Object for FunctionObject {
    fn object_type(&self) -> ObjectType {
        ObjectType::Function
    }

    fn inspect(&self) -> String {
        let mut value = String::new();
        value.push_str("fn");
        let para = self.parameters.iter().map(|e| e.token.literal.clone()).collect::<Vec<_>>().join(",");
        value.push_str(&format!("({})", para));
        value.push_str("{");
        for stmt in self.body.statements.iter() {
            value.push_str(&stmt.to_string());
        }
        value.push_str("}");
        value
    }

    fn is_return(&self) -> bool {
        self.is_return
    }

    fn set_return(&mut self) {
        self.is_return = true
    }
}

// impl From<FunctionLiteral> for FunctionObject{
//     fn from(literal: FunctionLiteral) -> Self {
//         let params = literal.parameters.clone();
//     }
// }

pub fn clone_box(input: &Box<dyn Object>) -> Box<dyn Object> {
    let value_any = input.as_ref() as &dyn Any;
    match input.object_type() {
        Integer => {
            let actual = value_any.downcast_ref::<IntegerObject>().unwrap();
            Box::new(actual.clone())
        }
        Boolean => {
            let actual = value_any.downcast_ref::<BooleanObject>().unwrap();
            Box::new(actual.clone())
        }
        Null => {
            let actual = value_any.downcast_ref::<NullObject>().unwrap();
            Box::new(actual.clone())
        }
        Error => {
            let actual = value_any.downcast_ref::<ErrorObject>().unwrap();
            Box::new(actual.clone())
        }
        ObjectType::Function => {
            panic!("can not clone function");
            // let actual = value_any.downcast_ref::<FunctionObject>().unwrap();
            // Box::new(actual.clone())
        }
    }
}