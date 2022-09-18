use std::fmt::{Debug, Display, Formatter};
use crate::evaluator::object::{ObjectType, Value};
use anyhow::{Result, format_err};

pub trait Builtin: Debug {
    fn execute(&self, args: &Vec<Value>) -> Result<Value>;
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Len;

impl Builtin for Len {
    fn execute(&self, args: &Vec<Value>) -> Result<Value> {
        if args.len() != 1 {
            return Err(format_err!("wrong number of arguments. got={}, want=1", args.len()));
        }
        let arg = args.get(0).unwrap();
        if arg.type_of != ObjectType::String {
            return Err(format_err!("argument to `len` not supported, got {}", arg.type_of));
        }

        let arg = String::try_from(arg)?;
        Ok(Value::from(arg.len() as i64))
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BuiltinFunction {
    Len(Len),
}

impl BuiltinFunction {
    pub fn lookup(name: &str) -> Option<BuiltinFunction> {
        match name {
            "len" => Some(BuiltinFunction::Len(Len)),
            _ => None,
        }
    }

    pub fn execute(&self, args: &Vec<Value>) -> Result<Value> {
        match self {
            BuiltinFunction::Len(f) => f.execute(args)
        }
    }
}

impl Display for BuiltinFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let r = match self {
            BuiltinFunction::Len(_) => "builtin/len",
        };
        f.write_str(r)
    }
}
