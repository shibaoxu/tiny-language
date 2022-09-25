use crate::ast::{BooleanLiteral, Expression, IntegerLiteral, modify, Node, StringLiteral};
use crate::evaluator::environment::Environment;
use crate::evaluator::object::{Value, WrappedValue};
use crate::evaluator::eval_expression;
use crate::lexer::token::Token;

pub fn quote(expr: &Expression, env: &Environment) -> Value {
    let node = eval_unquote_calls(&expr, env);
    Value::from(node)
}

fn modifier(expr: &Expression, env: &Environment) -> Expression {
    if !is_unquote_call(&expr) {
        return expr.clone();
    }

    if let Expression::CallExpr(call) = &expr {
        if call.arguments.len() != 1 {
            return expr.clone();
        }

        let value = eval_expression(call.arguments[0].as_ref(), env);
        return convert_value_to_ast(value.unwrap());
    }
    expr.clone()
}

pub fn eval_unquote_calls(quoted: &Expression, env: &Environment) -> Expression {
    modify(quoted, env, modifier)
}

fn is_unquote_call(node: &Expression) -> bool {
    if let Expression::CallExpr(expr) = node {
        expr.function.token_literal() == "unquote"
    } else {
        false
    }
}

fn convert_value_to_ast(value: Value) -> Expression {
    match value.value {
        WrappedValue::IntValue(v) => Expression::IntExpr(IntegerLiteral { token: Token::Int(v), value: v }),
        WrappedValue::BoolValue(v) => {
            let token = if v {
                Token::True
            } else {
                Token::False
            };
            Expression::BoolExpr(BooleanLiteral { token, value: v })
        }
        WrappedValue::StrValue(v) => Expression::StrExpr(StringLiteral { token: Token::String(v.clone()), value: v }),
        WrappedValue::Quote(expr) => expr,
        _ => panic!("unquote only support `String`, `Boolean`, `Integer`")
        // WrappedValue::NullValue(_) => panic!("unexpected null")
        // WrappedValue::FunValue(_) => {}
        // WrappedValue::BuiltinValue(_) => {}
        // WrappedValue::ArrayValue(_) => {}
        // WrappedValue::HashMapValue(_) => {}
    }
}

#[cfg(test)]
mod tests{
    use crate::evaluator::environment::Environment;
    use crate::evaluator::eval;
    use crate::parser::Parser;

    #[test]
    fn test_quote(){
        let cases = vec![
            ("quote(5)", "5"),
            ("quote(5+8)", "(5 + 8)"),
            ("quote(foobar)", "foobar"),
            ("quote(foobar+barfoo)", "(foobar + barfoo)")
        ];
        run_cases(cases);
    }

    #[test]
    fn test_unquote(){
        let cases = vec![
            ("quote(unquote(4))", "4"),
            ("quote(unquote(4+4))", "8"),
            ("quote(8+unquote(4+4))", "(8 + 8)"),
            ("quote(unquote(4+4)+8)", "(8 + 8)"),
            ("let foobar = 8; quote(foobar)", "foobar"),
            ("let foobar = 8; quote(unquote(foobar))", "8"),
            ("quote(unquote(true))", "true"),
            ("quote(unquote(true == false))", "false"),
            ("quote(unquote(quote(4+4)))", "(4 + 4)"),
            ("let quoteInfixExpression = quote(4+4); quote(unquote(4+4) + unquote(quoteInfixExpression))", "(8 + (4 + 4))")
        ];
        run_cases(cases);
    }
    fn run_cases(cases: Vec<(&str, &str)>){
        for (no, &case) in cases.iter().enumerate() {
            let mut parser = Parser::from_string(case.0);
            let program = parser.parse().unwrap();
            assert_eq!(parser.errors.len(), 0);
            let mut env = Environment::new();
            let result = eval(&program, &mut env).unwrap();
            // println!("{:?}", result);
            assert_eq!(result.inspect(), case.1, "{}: actual is {:?}, expected is {:?}", no, result, case.1);
        }

    }
}