use std::fmt::{Display, Formatter};
use crate::evaluator::builtin::Builtin;
use crate::evaluator::object::{NullValue, Value};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Put;

impl Display for Put {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("[builtin/put]")
    }
}

impl Builtin for Put {
    fn execute(&self, args: &Vec<Value>) -> anyhow::Result<Value> {
        for v in args{
            println!("{}", v.inspect());
        }

        Ok(Value::from(NullValue))
    }
}
