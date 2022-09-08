use crate::ast::{BlockStatement, Identifier};
use crate::evaluator::builtins::{BuiltinFunction};

use crate::evaluator::object::EvalResult::{BoolObj, BuiltinObj, ErrorObj, FunObj, IntObj, NullObj, StrObj};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum EvalResult {
    IntObj(IntegerObject),
    BoolObj(BooleanObject),
    StrObj(StringObject),
    NullObj(NullObject),
    ErrorObj(ErrorObject),
    FunObj(FunctionObject),
    BuiltinObj(BuiltinObject),
}

impl EvalValue for EvalResult {
    fn inspect(&self) -> String {
        match self {
            IntObj(v) => v.inspect(),
            BoolObj(v) => v.inspect(),
            StrObj(v) => v.inspect(),
            NullObj(v) => v.inspect(),
            ErrorObj(v) => v.inspect(),
            FunObj(v) => v.inspect(),
            BuiltinObj(v) => v.inspect(),
        }
    }

    fn is_return(&self) -> bool {
        match self {
            IntObj(v) => v.is_return(),
            BoolObj(v) => v.is_return(),
            StrObj(v) => v.is_return(),
            NullObj(v) => v.is_return(),
            ErrorObj(v) => v.is_return(),
            FunObj(v) => v.is_return(),
            BuiltinObj(v) => v.is_return(),
        }
    }

    fn set_return(&mut self) {
        match self {
            IntObj(v) => v.set_return(),
            BoolObj(v) => v.set_return(),
            StrObj(v) => v.set_return(),
            NullObj(v) => v.set_return(),
            ErrorObj(v) => v.set_return(),
            FunObj(v) => v.set_return(),
            BuiltinObj(v) => v.set_return(),
        }
    }
}

impl EvalResult {
    pub fn new_int_object(value: i64) -> EvalResult {
        IntObj(IntegerObject { value, is_return: false })
    }
    pub fn new_string_object(value: &str) -> EvalResult {
        StrObj(StringObject { value: value.to_string(), is_return: false })
    }
    pub fn new_int_object_with_return(value: i64) -> EvalResult {
        IntObj(IntegerObject { value, is_return: true })
    }

    pub fn new_bool_object(value: bool) -> EvalResult {
        BoolObj(BooleanObject { value, is_return: false })
    }

    pub fn new_bool_object_with_return(value: bool) -> EvalResult {
        BoolObj(BooleanObject { value, is_return: true })
    }

    pub fn new_null_object() -> EvalResult {
        NullObj(NullObject(false))
    }

    pub fn new_error_object(message: &str) -> EvalResult {
        ErrorObj(ErrorObject { message: message.into() })
    }

    pub fn new_function_object(parameters: Vec<Identifier>, body: BlockStatement) -> EvalResult {
        FunObj(FunctionObject {
            parameters,
            body,
            is_return: false,
        })
    }

    pub fn new_builtin_object(func: BuiltinFunction) -> EvalResult {
        BuiltinObj(BuiltinObject { function: func, is_return: false })
    }

    pub fn is_error(&self) -> bool {
        if let ErrorObj(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            IntObj(_) => true,
            BoolObj(v) => v.value,
            StrObj(_) => true,
            NullObj(_) => false,
            ErrorObj(_) => true,
            FunObj(_) => true,
            BuiltinObj(_) => true,
        }
    }

    pub fn is_builtin(&self) -> bool{
        match self {
            BuiltinObj(_) => true,
            _ => false,
        }
    }

    pub fn convert_to_int(&self) -> Result<i64, String> {
        if let IntObj(v) = self {
            Ok(v.value)
        } else {
            Err(format!("can not convert {} to i64", &self.inspect()))
        }
    }

    pub fn convert_to_bool(&self) -> Result<bool, String> {
        if let BoolObj(v) = self {
            Ok(v.value)
        } else {
            Err(format!("can not convert {} to i64", &self.inspect()))
        }
    }

    pub fn convert_to_string(&self) -> Result<String, String> {
        if let StrObj(v) = self {
            Ok(v.value.clone())
        } else {
            Err(format!("can not convert {} to string", &self.inspect()))
        }
    }

    pub fn convert_to_func(self) -> Result<FunctionObject, String> {
        if let FunObj(v) = self {
            Ok(v)
        } else {
            Err(format!("{} is not a valid function", self.inspect()))
        }
    }

    pub fn convert_to_err(&self) -> Result<ErrorObject, String> {
        if let ErrorObj(v) = self {
            Ok(v.clone())
        } else {
            Err(format!("{} is not a ErrorObject", self.inspect()))
        }
    }

    pub fn convert_to_builtin(&self) -> Result<BuiltinObject, String> {
        if let BuiltinObj(v) = self {
            Ok(v.clone())
        } else {
            Err(format!("{} is not a BuiltinObject", self.inspect()))
        }
    }
    pub fn get_type(&self) -> &'static str {
        match self {
            IntObj(_) => "INTEGER",
            BoolObj(_) => "BOOLEAN",
            StrObj(_) => "STRING",
            NullObj(_) => "NULL",
            ErrorObj(_) => "ERROR",
            FunObj(_) => "FUNCTION",
            BuiltinObj(_) => "BUILTIN",
        }
    }
}

pub trait EvalValue {
    fn inspect(&self) -> String;
    fn is_return(&self) -> bool;
    fn set_return(&mut self);
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct IntegerObject {
    pub value: i64,
    is_return: bool,
}

impl EvalValue for IntegerObject {
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StringObject {
    pub value: String,
    is_return: bool,
}

impl EvalValue for StringObject {
    fn inspect(&self) -> String {
        self.value.clone()
    }

    fn is_return(&self) -> bool {
        self.is_return
    }

    fn set_return(&mut self) {
        self.is_return = true
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BooleanObject {
    pub value: bool,
    pub is_return: bool,
}

impl EvalValue for BooleanObject {
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct NullObject(pub bool);

impl EvalValue for NullObject {
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

impl EvalValue for ErrorObject {
    fn inspect(&self) -> String {
        format!("{}", self.message)
    }

    fn is_return(&self) -> bool {
        true
    }

    fn set_return(&mut self) {}
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionObject {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub is_return: bool,
}

impl EvalValue for FunctionObject {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BuiltinObject {
    pub function: BuiltinFunction,
    pub is_return: bool,
}

impl EvalValue for BuiltinObject {
    fn inspect(&self) -> String {
        self.function.to_string()
    }

    fn is_return(&self) -> bool {
        false
    }

    fn set_return(&mut self) {
        self.is_return = true;
    }
}
