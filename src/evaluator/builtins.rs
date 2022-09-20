use std::fmt::{Debug, Display, Formatter};
use crate::evaluator::object::{ObjectType, Value, WrappedValue};
use anyhow::{Result, format_err};

pub trait Builtin: Debug + Display {
    fn name(&self) -> String {
        self.to_string()
    }
    fn execute(&self, args: &Vec<Value>) -> Result<Value>;
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Len;

impl Builtin for Len {
    fn execute(&self, args: &Vec<Value>) -> Result<Value> {
        if args.len() != 1 {
            return Err(format_err!("wrong number of arguments. got=`{}`, want=`1`", args.len()));
        }
        let arg = args.get(0).unwrap();
        let result = match arg.type_of {
            ObjectType::String => {
                let arg = String::try_from(arg)?;
                Value::from(arg.len() as i64)
            }
            ObjectType::Array => {
                let arg = Vec::try_from(arg)?;
                Value::from(arg.len() as i64)
            }
            _ => return Err(format_err!("argument to `len` not supported, got `{}`", arg.type_of))
        };

        Ok(result)
    }
}

impl Display for Len {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Push;

impl Builtin for Push {
    fn execute(&self, args: &Vec<Value>) -> Result<Value> {
        if args.len() != 2 {
            return Err(format_err!("wrong number of arguments. got=`{}`, want=`2`", args.len()));
        }
        let arg = &args[0];
        let element = &args[1];
        if let WrappedValue::ArrayValue(v) = &arg.value {
            let mut result = v.clone();
            result.push(element.clone());
            Ok(Value::from(result))
        } else {
            Err(format_err!("arg 0 must be `ARRAY`"))
        }
    }
}

impl Display for Push {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct First;

impl Builtin for First {
    fn execute(&self, args: &Vec<Value>) -> Result<Value> {
        if args.len() != 1 {
            return Err(format_err!("wrong number of arguments. got=`{}`, want=`1`", args.len()));
        }

        let arg = args[0].clone();
        if arg.type_of != ObjectType::Array {
            return Err(format_err!("mismatch type. got=`{}`, want=`{}`", arg.type_of, ObjectType::Array));
        }

        let arg = Vec::try_from(&arg)?;
        arg.first().ok_or(format_err!("index `0` out of bound `0`")).map(Clone::clone)
    }
}

impl Display for First {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("builtin/first")
    }
}


pub struct Builtins;

impl Builtins {
    pub fn lookup(name: &str) -> Option<Box<dyn Builtin>> {
        match name {
            "len" => Some(Box::new(Len)),
            "push" => Some(Box::new(Push)),
            "first" => Some(Box::new(First)),
            _ => None,
        }
    }
}