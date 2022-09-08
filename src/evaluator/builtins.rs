use std::fmt::{Debug, Display, Formatter};
use crate::evaluator::object::EvalResult;

pub trait Builtin: Debug {
    fn execute(&self, args: &Vec<EvalResult>) -> EvalResult;
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Len;
impl Builtin for Len {
    fn execute(&self, args: &Vec<EvalResult>) -> EvalResult {
        if args.len() != 1 {
            return EvalResult::new_error_object(&format!("wrong number of arguments. got={}, want=1", args.len()));
        }
        let arg = args.get(0).unwrap();
        if arg.get_type() != "STRING" {
            return EvalResult::new_error_object(&format!("argument to `len` not supported, got {}", arg.get_type()));
        }

        let arg = arg.convert_to_string().unwrap();
        EvalResult::new_int_object(arg.len() as i64)
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

    pub fn execute(&self, args: &Vec<EvalResult>)->EvalResult{
        match self {
            BuiltinFunction::Len(f) => f.execute(args)
        }
    }
}

impl Display for BuiltinFunction{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let r = match self {
            BuiltinFunction::Len(_) => "builtin/len",
        };
        f.write_str(r)
    }
}
