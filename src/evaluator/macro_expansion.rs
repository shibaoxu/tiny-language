use crate::ast::{CallExpression, Node};
use crate::ast::Expression;
use crate::evaluator::environment::Environment;
use crate::evaluator::object::{Macro, Value};

fn is_macro_call(call_expr: &CallExpression, env: &Environment) -> (Option<Macro>, bool) {
    if let Expression::IdentExpr(identity) =  &call_expr.function{
        match env.get(&identity.token_literal()){
            None => panic!("can not find macro definition"),
            Some(v) => return (Some(Macro::try_from(&v).unwrap()), true)
        }
    }
    (None, false)
}