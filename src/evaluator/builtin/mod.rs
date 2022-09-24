use std::fmt::{Debug, Display};
use crate::evaluator::object::{ObjectType, Value};
use anyhow::{format_err, Result};
use crate::evaluator::builtin::array::{First, Last, Len, Push, Rest};

mod array;
mod io;

pub trait Builtin: Debug + Display {
    fn name(&self) -> String {
        self.to_string()
    }
    fn execute(&self, args: &Vec<Value>) -> Result<Value>;
}

pub struct Builtins;

impl Builtins {
    pub fn lookup(name: &str) -> Option<Box<dyn Builtin>> {
        match name {
            "len" => Some(Box::new(Len)),
            "push" => Some(Box::new(Push)),
            "first" => Some(Box::new(First)),
            "last" => Some(Box::new(Last)),
            "rest" => Some(Box::new(Rest)),
            _ => None,
        }
    }
}

fn check_argument_number(f: &dyn Builtin, args: &Vec<Value>, num: usize) -> Result<()> {
    if args.len() != num {
        return Err(format_err!("[{}]: wrong number of arguments. got=`{}`, want=`{}`",f.name(), args.len(), num));
    }
    Ok(())
}

fn check_argument_type(f: &dyn Builtin, arg: &Value, typ: ObjectType) -> Result<()> {
    if arg.type_of != typ {
        return Err(format_err!("[{}]: mismatch type. got=`{}`, want=`{}`", f.name(), arg.type_of, ObjectType::Array));
    }
    Ok(())
}