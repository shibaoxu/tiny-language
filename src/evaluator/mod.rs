use std::any::{Any, TypeId};
use crate::ast::{BlockStatement, BooleanLiteral, CallExpression, Expression, ExpressionStatement, FunctionLiteral, Identifier, IfExpression, InfixExpression, IntegerLiteral, LetStatement, Node, PrefixExpression, Program, ReturnStatement, Statement};
use crate::evaluator::environment::Environment;
use crate::evaluator::object::{BooleanObject, clone_box, ErrorObject, FunctionObject, IntegerObject, NullObject, Object, ObjectType};
use crate::evaluator::object::ObjectType::Boolean;
use crate::TokenType;

mod object;
pub mod environment;

pub fn eval(node: &dyn Node, env: &mut Environment) -> Box<dyn Object> {
    let value_any = node as &dyn Any;
    let type_id = value_any.type_id();
    return if type_id == TypeId::of::<Program>() {
        let program = value_any.downcast_ref::<Program>().unwrap();
        eval_statements(&program.statements, env)
    } else if type_id == TypeId::of::<ExpressionStatement>() {
        let exp_statement = value_any.downcast_ref::<ExpressionStatement>().unwrap();
        eval(exp_statement.expression.as_ref(), env)
    } else if type_id == TypeId::of::<IntegerLiteral>() {
        let integer_literal = value_any.downcast_ref::<IntegerLiteral>().unwrap();
        Box::new(IntegerObject::from(integer_literal.value))
    } else if type_id == TypeId::of::<BooleanLiteral>() {
        let bool_literal = value_any.downcast_ref::<BooleanLiteral>().unwrap();
        Box::new(BooleanObject::from(bool_literal.value))
    } else if type_id == TypeId::of::<PrefixExpression>() {
        let prefix_exp = value_any.downcast_ref::<PrefixExpression>().unwrap();
        let right = eval(prefix_exp.right.as_ref(), env);
        if right.object_type() == ObjectType::Error {
            return right;
        }
        match prefix_exp.token.token_type {
            TokenType::Bang => eval_bang_operator_expression(right, env),
            TokenType::Minus => eval_minus_operator_expression(right, env),
            _ => Box::new(ErrorObject::from(&format!("unknown operator: {}", prefix_exp.token.token_type)))
        }
    } else if type_id == TypeId::of::<InfixExpression>() {
        let infix_exp = value_any.downcast_ref::<InfixExpression>().unwrap();
        let left = eval(infix_exp.left.as_ref(), env);
        if left.object_type() == ObjectType::Error {
            return left;
        }
        let right = eval(infix_exp.right.as_ref(), env);
        if right.object_type() == ObjectType::Error {
            return right;
        }
        eval_infix_expression(infix_exp.token.token_type, left, right, env)
    } else if type_id == TypeId::of::<IfExpression>() {
        let if_exp = value_any.downcast_ref::<IfExpression>().unwrap();
        eval_if_expression(if_exp, env)
    } else if type_id == TypeId::of::<BlockStatement>() {
        let block = value_any.downcast_ref::<BlockStatement>().unwrap();
        eval_statements(&block.statements, env)
    } else if type_id == TypeId::of::<ReturnStatement>() {
        let ret = value_any.downcast_ref::<ReturnStatement>().unwrap();
        let mut val = eval(ret.value.as_ref(), env);
        val.set_return();
        val
    } else if type_id == TypeId::of::<LetStatement>() {
        let let_statement = value_any.downcast_ref::<LetStatement>().unwrap();
        let val = eval(let_statement.value.as_ref(), env);
        if val.object_type() == ObjectType::Error {
            return val;
        }
        env.set(&let_statement.name.token.literal, clone_box(&val));
        val
    } else if type_id == TypeId::of::<Identifier>() {
        let identifier = value_any.downcast_ref::<Identifier>().unwrap();
        eval_identifier(identifier, env)
    } else if type_id == TypeId::of::<FunctionLiteral>() {
        let func_literal = value_any.downcast_ref::<FunctionLiteral>().unwrap();
        Box::new(FunctionObject {
            parameters: func_literal.parameters.clone(),
            body: func_literal.body.clone(),
            env: env.clone(),
            is_return: false,
        })
    } else if type_id == TypeId::of::<CallExpression>() {
        let call_exp = value_any.downcast_ref::<CallExpression>().unwrap();
        let func = eval(call_exp.function.as_ref(), env);
        let args = eval_expressions(&call_exp.arguments,env);

        Box::new(ErrorObject::from(&format!("unknown syntax tree node {}", node.to_string())))
    }else {
        Box::new(ErrorObject::from(&format!("unknown syntax tree node {}", node.to_string())))
    };
}

fn eval_expressions(expressions: &Vec<Box<dyn Expression>>, env: &mut Environment) -> Vec<Box<dyn Object>>{
    let mut result = vec![];
    for exp in expressions.iter().map(|e| e.as_ref()){
        let arg = eval(exp, env);
        if arg.object_type() == ObjectType::Error{
            return vec![arg]
        }
        result.push(arg);
    }
    result
}

fn eval_statements(stmts: &Vec<Box<dyn Statement>>, env: &mut Environment) -> Box<dyn Object> {
    let mut result: Box<dyn Object> = Box::new(NullObject(false));
    for stmt in stmts.iter().map(|e| e.as_ref()) {
        result = eval(stmt, env);
        if result.is_return() {
            return result;
        }
    }
    result
}

fn eval_bang_operator_expression(right: Box<dyn Object>, env: &mut Environment) -> Box<dyn Object> {
    let right_any = right.as_ref() as &dyn Any;
    match right.object_type() {
        ObjectType::Integer => {
            let actual = right_any.downcast_ref::<IntegerObject>().unwrap();
            match actual.value {
                0 => Box::new(BooleanObject::from(true)),
                _ => Box::new(BooleanObject::from(false)),
            }
        }
        ObjectType::Boolean => {
            let actual = right_any.downcast_ref::<BooleanObject>().unwrap();
            Box::new(BooleanObject::from(!actual.value))
        }
        ObjectType::Null => Box::new(BooleanObject::from(true)),
        ObjectType::Function => Box::new(BooleanObject::from(false)),
        ObjectType::Error => right,
    }
}

fn eval_minus_operator_expression(right: Box<dyn Object>, _env: &mut Environment) -> Box<dyn Object> {
    let right_any = right.as_ref() as &dyn Any;
    match right.object_type() {
        ObjectType::Integer => {
            let actual = right_any.downcast_ref::<IntegerObject>().unwrap();
            Box::new(IntegerObject::from(-actual.value))
        }
        ObjectType::Error => right,
        _ => Box::new(ErrorObject::from(&format!("unknown operator: -{}", right.object_type())))
    }
}

fn eval_infix_expression(operator: TokenType, left: Box<dyn Object>, right: Box<dyn Object>, _env: &mut Environment) -> Box<dyn Object> {
    if left.object_type() != right.object_type() {
        return Box::new(ErrorObject::from(&format!("type mismatch: {} {} {}", left.object_type(), operator, right.object_type())));
    }

    let left_any = left.as_ref() as &dyn Any;
    let right_any = right.as_ref() as &dyn Any;
    match left.object_type() {
        ObjectType::Integer => {
            let left_int = left_any.downcast_ref::<IntegerObject>().unwrap();
            let right_int = right_any.downcast_ref::<IntegerObject>().unwrap();
            match operator {
                TokenType::Plus => Box::new(IntegerObject::from(left_int.value + right_int.value)) as Box<dyn Object>,
                TokenType::Minus => Box::new(IntegerObject::from(left_int.value - right_int.value)) as Box<dyn Object>,
                TokenType::Asterisk => Box::new(IntegerObject::from(left_int.value * right_int.value)) as Box<dyn Object>,
                TokenType::Slash => Box::new(IntegerObject::from(left_int.value / right_int.value)) as Box<dyn Object>,
                TokenType::GT => Box::new(BooleanObject::from(left_int.value > right_int.value)) as Box<dyn Object>,
                TokenType::GE => Box::new(BooleanObject::from(left_int.value >= right_int.value)) as Box<dyn Object>,
                TokenType::LT => Box::new(BooleanObject::from(left_int.value < right_int.value)) as Box<dyn Object>,
                TokenType::LE => Box::new(BooleanObject::from(left_int.value <= right_int.value)) as Box<dyn Object>,
                TokenType::EQ => Box::new(BooleanObject::from(left_int.value == right_int.value)) as Box<dyn Object>,
                TokenType::NotEq => Box::new(BooleanObject::from(left_int.value != right_int.value)) as Box<dyn Object>,
                _ => Box::new(ErrorObject::from(&format!("unknown operator: {} {} {}", left.object_type(), operator, right.object_type()))),
            }
        }
        ObjectType::Boolean => {
            let left_bool = left_any.downcast_ref::<BooleanObject>().unwrap();
            let right_bool = right_any.downcast_ref::<BooleanObject>().unwrap();
            match operator {
                TokenType::EQ => Box::new(BooleanObject::from(left_bool.value == right_bool.value)) as Box<dyn Object>,
                TokenType::NotEq => Box::new(BooleanObject::from(left_bool.value != right_bool.value)) as Box<dyn Object>,
                _ => Box::new(ErrorObject::from(&format!("unknown operator: {} {} {}", left.object_type(), operator, right.object_type()))),
            }
        }
        ObjectType::Null => Box::new(ErrorObject::from(&format!("unknown operator: {} {} {}", left.object_type(), operator, right.object_type()))) as Box<dyn Object>,
        ObjectType::Error => left,
        ObjectType::Function => Box::new(ErrorObject::from(&format!("unknown operator: {} {} {}", left.object_type(), operator, right.object_type()))) as Box<dyn Object>,
    }
}

fn eval_if_expression(if_exp: &IfExpression, env: &mut Environment) -> Box<dyn Object> {
    let condition = eval(if_exp.condition.as_ref(), env);
    if condition.object_type() == ObjectType::Error {
        return condition;
    }

    if is_truthy(&condition) {
        eval(&if_exp.consequence, env)
    } else {
        match &if_exp.alternative {
            Some(alt) => eval(alt, env),
            _ => Box::new(NullObject(false))
        }
    }
}

fn eval_identifier(identifier: &Identifier, env: &mut Environment) -> Box<dyn Object> {
    let token = identifier.token.clone();
    match env.get(&token.literal) {
        Some(value) => clone_box(value),
        _ => Box::new(ErrorObject::from(&format!("identifier not found: {}", token.literal))) as Box<dyn Object>
    }
}

fn is_truthy(condition: &Box<dyn Object>) -> bool {
    if condition.object_type() == Boolean {
        let condition_any = condition.as_ref() as &dyn Any;
        condition_any.downcast_ref::<BooleanObject>().unwrap().value
    } else if condition.object_type() == ObjectType::Null {
        false
    } else {
        true
    }
}
// }


#[cfg(test)]
mod tests {
    use std::any::Any;
    use std::fmt::Debug;
    use std::rc::Rc;
    use crate::ast::{BlockStatement, Identifier};
    use crate::evaluator::environment::Environment;
    use crate::evaluator::eval;
    use crate::evaluator::object::{BooleanObject, ErrorObject, FunctionObject, IntegerObject, NullObject, Object, ObjectType};
    use crate::evaluator::object::ObjectType::Boolean;
    use crate::lexer::token::Token;
    use crate::parser::Parser;
    use crate::TokenType;

    #[test]
    fn test_eval_integer_expression() {
        let cases = vec![
            ("5", IntegerObject::from(5)),
            ("10", IntegerObject::from(10)),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_eval_boolean_expression() {
        let cases = vec![
            ("true", BooleanObject::from(true)),
            ("false", BooleanObject::from(false)),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_eval_bang_expression() {
        let cases = vec![
            ("!true", BooleanObject::from(false)),
            ("!false", BooleanObject::from(true)),
            ("!5", BooleanObject::from(false)),
            ("!0", BooleanObject::from(true)),
            ("!!true", BooleanObject::from(true)),
            ("!!false", BooleanObject::from(false)),
            ("!!5", BooleanObject::from(true)),
            ("!!0", BooleanObject::from(false)),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_eval_integer_infix_expression() {
        let cases = vec![
            ("5", IntegerObject::from(5)),
            ("10", IntegerObject::from(10)),
            ("-5", IntegerObject::from(-5)),
            ("-10", IntegerObject::from(-10)),
            ("5 + 5 + 5 + 5 - 10", IntegerObject::from(10)),
            ("2 * 2 * 2 * 2 * 2", IntegerObject::from(32)),
            ("-50 + 100 + -50", IntegerObject::from(0)),
            ("5 * 2 + 10", IntegerObject::from(20)),
            ("5 + 2 * 10", IntegerObject::from(25)),
            ("20 + 2 * -10", IntegerObject::from(0)),
            ("50 / 2 * 2 + 10", IntegerObject::from(60)),
            ("2 * (5 + 10)", IntegerObject::from(30)),
            ("3 * 3 * 3 + 10", IntegerObject::from(37)),
            ("3 * (3 * 3) + 10", IntegerObject::from(37)),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", IntegerObject::from(50)),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_eval_bool_infix_expression() {
        let cases = vec![
            ("true", BooleanObject::from(true)),
            ("false", BooleanObject::from(false)),
            ("1 < 2", BooleanObject::from(true)),
            ("1 > 2", BooleanObject::from(false)),
            ("1 < 1", BooleanObject::from(false)),
            ("1 > 1", BooleanObject::from(false)),
            ("1 == 1", BooleanObject::from(true)),
            ("1 != 1", BooleanObject::from(false)),
            ("1 == 2", BooleanObject::from(false)),
            ("1 != 2", BooleanObject::from(true)),
            ("true == true", BooleanObject::from(true)),
            ("false == false", BooleanObject::from(true)),
            ("true == false", BooleanObject::from(false)),
            ("true != false", BooleanObject::from(true)),
            ("false != true", BooleanObject::from(true)),
            ("(1 < 2) == true", BooleanObject::from(true)),
            ("(1 < 2) == false", BooleanObject::from(false)),
            ("(1 > 2) == true", BooleanObject::from(false)),
            ("(1 > 2) == false", BooleanObject::from(true)),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_return_statements() {
        let cases = vec![
            ("return 10;", IntegerObject::from(10)),
            ("return 10;9;", IntegerObject::from(10)),
            ("return 2*5;9;", IntegerObject::from(10)),
            ("9;return 2*5;9;", IntegerObject::from(10)),
            ("if (10>1) {\
                 if (10>1) {\
                    return 10;\
                 }\
                 return 1;\
             }", IntegerObject::from(10)),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_if_expression() {
        let cases = vec![
            ("if (true) { 10 }", IntegerObject::from(10)),
            ("if (1) { 10 }", IntegerObject::from(10)),
            ("if (1 < 2) { 10 }", IntegerObject::from(10)),
            ("if (1 > 2) { 10 } else { 20 }", IntegerObject::from(20)),
            ("if (1 < 2) { 10 } else { 20 }", IntegerObject::from(10)),
        ];
        run_cases(&cases);
        let cases = vec![
            ("if (false) { 10 }", NullObject(false)),
            ("if (1 > 2) { 10 }", NullObject(false)),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_let_statements() {
        let cases = vec![
            ("let a = 5; a;", IntegerObject::from(5)),
            ("let a = 5 * 5; a;", IntegerObject::from(25)),
            ("let a = 5; let b = a; b;", IntegerObject::from(5)),
            ("let a = 5; let b = a; let c = a + b + 5; c;", IntegerObject::from(15)),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_error_handling() {
        let cases = vec![
            ("5 + true;", ErrorObject::from("type mismatch: INTEGER + BOOLEAN")),
            ("5 + true; 5;", ErrorObject::from("type mismatch: INTEGER + BOOLEAN")),
            ("-true", ErrorObject::from("unknown operator: -BOOLEAN")),
            ("true + false;", ErrorObject::from("unknown operator: BOOLEAN + BOOLEAN")),
            ("true + false + true + false;", ErrorObject::from("unknown operator: BOOLEAN + BOOLEAN")),
            ("5; true + false; 5", ErrorObject::from("unknown operator: BOOLEAN + BOOLEAN")),
            ("if (10 > 1) { true + false; }", ErrorObject::from("unknown operator: BOOLEAN + BOOLEAN")),
            ("if (10 > 1) {
                if (10 > 1) {
                  return true + false;
                }
                return 1;
             }", ErrorObject::from("unknown operator: BOOLEAN + BOOLEAN")),
            ("foobar", ErrorObject::from("identifier not found: foobar")),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_function_literal(){
        let cases = vec![
            ("fn(x) { x + 2}", "fn(x){(x + 2)}"),
        ];
        for (no, &case) in cases.iter().enumerate() {
            let mut parser = Parser::from_string(case.0);
            let program = parser.parse();
            assert_eq!(parser.errors.len(), 0);
            let mut env = Environment::new();
            let result = eval(&program, &mut env);
            assert_eq!(result.inspect(), case.1);
        }


    }

    fn run_cases<T: Object + Debug>(cases: &Vec<(&str, T)>) {
        for (no, case) in cases.iter().enumerate() {
            let mut parser = Parser::from_string(case.0);
            let program = parser.parse();
            assert_eq!(parser.errors.len(), 0);
            let mut env = Environment::new();
            let result = eval(&program, &mut env);
            assert!(equal(result.as_ref(), &case.1), "{}: actual is {:?}, expected is {:?}", no, result.inspect(), case.1)
        }
    }

    fn equal(left: &dyn Object, right: &dyn Object) -> bool {
        if left.object_type() != right.object_type() {
            return false;
        }
        let left_any = left as &dyn Any;
        let right_any = right as &dyn Any;

        match left.object_type() {
            ObjectType::Integer => left_any.downcast_ref::<IntegerObject>().unwrap().value == right_any.downcast_ref::<IntegerObject>().unwrap().value,
            Boolean => left_any.downcast_ref::<BooleanObject>().unwrap().value == right_any.downcast_ref::<BooleanObject>().unwrap().value,
            ObjectType::Null => true,
            ObjectType::Error => left_any.downcast_ref::<ErrorObject>().unwrap().message == right_any.downcast_ref::<ErrorObject>().unwrap().message,
            ObjectType::Function => false,
        }
    }
}