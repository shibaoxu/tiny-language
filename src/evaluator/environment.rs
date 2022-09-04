use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use crate::evaluator::object::{clone_box, Object};

pub struct Environment<'a> {
    store: HashMap<String, Box<dyn Object>>,
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

    pub fn get(&self, name: &str) -> Option<&Box<dyn Object>> {
        match self.store.get(name) {
            Some(value) => Some(value),
            None => {
                match self.outer{
                    None => None,
                    Some(outer) => outer.get(name)
                }
            }
        }
    }

    pub fn set(&mut self, name: &str, value: Box<dyn Object>) -> Option<Box<dyn Object>> {
        self.store.insert(name.into(), value)
    }
}
//
// impl Clone for Environment {
//     fn clone(&self) -> Self {
//         let mut store = HashMap::new();
//         for (key, value) in self.store.iter() {
//             store.insert(key.to_string(), clone_box(value));
//         }
//         Self {
//             store,
//             outer: self.outer.clone(),
//         }
//     }
// }


#[cfg(test)]
mod tests {
    use crate::evaluator::environment::Environment;
    use crate::evaluator::object::IntegerObject;

    #[test]
    fn test_env_work() {
        let mut env0 = Environment::new();
        env0.set("foo", Box::new(IntegerObject::from(1)));
        env0.set("bar", Box::new(IntegerObject::from(2)));

        let mut env1 = env0.new_closure();
        env1.set("foo1", Box::new(IntegerObject::from(11)));
        env1.set("bar1", Box::new(IntegerObject::from(21)));

        assert_eq!(env1.get("foo1").unwrap().inspect(), "11");
        assert_eq!(env1.get("bar1").unwrap().inspect(), "21");
        assert_eq!(env1.get("foo").unwrap().inspect(), "1");
        assert_eq!(env1.get("bar").unwrap().inspect(), "2");
        assert!(env1.get("bar2").is_none());
    }
}