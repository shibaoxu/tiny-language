use std::collections::HashMap;
use crate::evaluator::builtins::BuiltinFunction;
use crate::evaluator::object::EvalResult;

#[derive(Debug, Clone)]
pub struct Environment<'a> {
    store: HashMap<String, EvalResult>,
    outer: Option<&'a Environment<'a>>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Environment<'a> {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_closure(&'a self) -> Environment<'a> {
        Self {
            store: HashMap::new(),
            outer: Some(self),
        }
    }

    pub fn get(&self, name: &str) -> Option<EvalResult> {
        let result = match self.store.get(name) {
            Some(value) => Some(value.clone()),
            None => {
                match self.outer{
                    None => None,
                    Some(outer) => outer.get(name)
                }
            }
        };

        // builtin function
        result.or_else(||{
            BuiltinFunction::lookup(name).map(|e| EvalResult::new_builtin_object(e))
        })
    }

    pub fn set(&mut self, name: &str, value: EvalResult) -> Option<EvalResult> {
        self.store.insert(name.into(), value)
    }
}
#[cfg(test)]
mod tests {
    use crate::evaluator::environment::Environment;
    use crate::evaluator::object::{EvalResult, EvalValue};

    #[test]
    fn test_env_work() {
        let mut env0 = Environment::new();
        env0.set("foo", EvalResult::new_int_object(1));
        env0.set("bar", EvalResult::new_int_object(2));

        let mut env1 = env0.new_closure();
        env1.set("foo1", EvalResult::new_int_object(11));
        env1.set("bar1", EvalResult::new_int_object(12));
        env1.set("foo", EvalResult::new_int_object(100));
        env1.set("bar", EvalResult::new_int_object(200));

        // let a = env1.get("foo1").unwrap();
        // eprintln!("{}",a.inspect());

        assert_eq!(env1.get("foo1").unwrap().inspect(), "11");
        assert_eq!(env1.get("bar1").unwrap().inspect(), "12");
        assert_eq!(env1.get("foo").unwrap().inspect(), "100");
        assert_eq!(env1.get("bar").unwrap().inspect(), "200");
        assert!(env1.get("bar2").is_none());
    }
}