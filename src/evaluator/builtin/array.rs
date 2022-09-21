use std::fmt::{Display, Formatter};
use anyhow::{format_err, Result};
use crate::evaluator::builtin::{Builtin, check_argument_number, check_argument_type};
use crate::evaluator::object::{ObjectType, Value};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Len;

impl Builtin for Len {
    fn execute(&self, args: &Vec<Value>) -> Result<Value> {
        check_argument_number(self, args, 1)?;
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
            _ => return Err(format_err!("[builtin/len]: mismatch type. got=`{}`, want=`ARRAY` or `STRING`", arg.type_of))
        };

        Ok(result)
    }
}

impl Display for Len {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("builtin/len")
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Push;

impl Builtin for Push {
    fn execute(&self, args: &Vec<Value>) -> Result<Value> {

        check_argument_number(self, args,2)?;

        let target = &args[0];
        check_argument_type(self, target, ObjectType::Array)?;

        let value = &args[1];

        let mut target = Vec::try_from(target)?.clone();
        // let mut target = target.clone();
        target.push(value.clone());

        Ok(Value::from(target))
    }
}

impl Display for Push {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("builtin/push")
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct First;

impl Builtin for First {
    fn execute(&self, args: &Vec<Value>) -> Result<Value> {
        check_argument_number(self, args, 1)?;
        let arg = args[0].clone();
        check_argument_type(self, &arg, ObjectType::Array)?;
        let arg = Vec::try_from(&arg)?;
        arg.first().ok_or(format_err!("index `0` out of bound `0`")).map(Clone::clone)
    }
}

impl Display for First {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("builtin/first")
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Last;

impl Builtin for Last {
    fn execute(&self, args: &Vec<Value>) -> Result<Value> {
        check_argument_number(self, args, 1)?;
        let arg = args[0].clone();
        check_argument_type(self, &arg, ObjectType::Array)?;
        let arg = Vec::try_from(&arg)?;
        arg.last().ok_or(format_err!("index `0` out of bound `0`")).map(Clone::clone)
    }
}

impl Display for Last {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("builtin/last")
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Rest;

impl Builtin for Rest {
    fn execute(&self, args: &Vec<Value>) -> Result<Value> {
        check_argument_number(self, args, 1)?;
        let arg = args[0].clone();
        check_argument_type(self, &arg, ObjectType::Array)?;
        let arg = Vec::try_from(&arg)?;
        if arg.len() < 1 {
            return Err(format_err!("[builtin/rest]: len of arg must greater than `1`"));
        }
        let v = arg.as_slice()[1..].to_vec();
        Ok(Value::from(v))
    }
}

impl Display for Rest {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("builtin/rest")
    }
}


