use std::collections::HashMap;
use crate::evaluator::builtin::Builtins;
use crate::evaluator::object::{BuiltinFunction, Value};

#[derive(Debug, Clone)]
pub struct Environment<'a> {
    store: HashMap<String, Value>,
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

    pub fn get(&self, name: &str) -> Option<Value> {
        let result = match self.store.get(name) {
            Some(value) => Some(value.clone()),
            None => {
                match self.outer {
                    None => None,
                    Some(outer) => outer.get(name)
                }
            }
        };

        // builtin function
        result.or_else(|| {
            Builtins::lookup(name).map(|_| Value::from(BuiltinFunction(name.to_string())))
        })
    }

    pub fn set(&mut self, name: &str, value: Value) -> Option<Value> {
        self.store.insert(name.into(), value)
    }
}

#[cfg(test)]
mod tests {
    use crate::evaluator::environment::Environment;
    use crate::evaluator::object::Value;

    #[test]
    fn test_env_work() {
        let mut env0 = Environment::new();
        env0.set("foo", Value::from(1));
        env0.set("bar", Value::from(2));

        let mut env1 = env0.new_closure();
        env1.set("foo1", Value::from(11));
        env1.set("bar1", Value::from(12));
        env1.set("foo", Value::from(100));
        env1.set("bar", Value::from(200));

        assert_eq!(env1.get("foo1").unwrap().inspect(), "11");
        assert_eq!(env1.get("bar1").unwrap().inspect(), "12");
        assert_eq!(env1.get("foo").unwrap().inspect(), "100");
        assert_eq!(env1.get("bar").unwrap().inspect(), "200");
        assert!(env1.get("bar2").is_none());
    }
}