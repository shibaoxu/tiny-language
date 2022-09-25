use std::collections::BTreeMap;
use crate::ast::{IndexExpression, ArrayLiteral, BlockStatement, CallExpression, Expression, ExpressionStatement, FunctionLiteral, HashmapLiteral, Identifier, IfExpression, InfixExpression, LetStatement, Node, PrefixExpression, Program, ReturnStatement, Statement};
use crate::evaluator::environment::Environment;
use crate::evaluator::object::{BuiltinFunction, Function, NullValue, ObjectType, Value, WrappedValue};
use crate::lexer::token::Token;
use anyhow::{format_err, Result};
use crate::evaluator::builtin::Builtins;
use crate::evaluator::quote_unquote::quote;

pub mod object;
pub mod environment;
mod builtin;
mod quote_unquote;

pub fn eval(program: &Program, env: &mut Environment) -> Result<Value> {
    eval_statements(&program.statements, env)
}

fn eval_statements(statements: &Vec<Statement>, env: &mut Environment) -> Result<Value> {
    let mut result = Value::from(NullValue);
    for stmt in statements {
        result = eval_statement(stmt, env)?;
        if result.is_return {
            return Ok(result);
        }
    }
    Ok(result)
}

fn eval_statement(stmt: &Statement, env: &mut Environment) -> Result<Value> {
    match stmt {
        Statement::LetStmt(stmt) => eval_let_stmt(stmt, env),
        Statement::ReturnStmt(stmt) => eval_return_stmt(stmt, env),
        Statement::BlockStmt(stmt) => eval_block_statement(stmt, env),
        Statement::ExpStmt(stmt) => eval_expression_stmt(stmt, env),
    }
}

fn eval_let_stmt(stmt: &LetStatement, env: &mut Environment) -> Result<Value> {
    let val = eval_expression(&stmt.value, env)?;
    env.set(&stmt.name.to_string(), val.clone());
    Ok(val)
}

fn eval_return_stmt(stmt: &ReturnStatement, env: &Environment) -> Result<Value> {
    let mut val = eval_expression(&stmt.value, env)?;
    val.set_return();
    Ok(val)
}

fn eval_block_statement(stmt: &BlockStatement, env: &Environment) -> Result<Value> {
    let mut inner_env = env.new_closure();
    eval_statements(&stmt.statements, &mut inner_env)
}

fn eval_expression_stmt(stmt: &ExpressionStatement, env: &mut Environment) -> Result<Value> {
    eval_expression(&stmt.expression, env)
}

fn eval_expression(expression: &Expression, env: &Environment) -> Result<Value> {
    let val = match expression {
        Expression::IdentExpr(expr) => eval_identifier(expr, env)?,
        Expression::BoolExpr(expr) => Value::from(expr.value),
        Expression::IntExpr(expr) => Value::from(expr.value),
        Expression::StrExpr(expr) => Value::from(expr.value.clone()),
        Expression::PrefixExpr(expr) => eval_prefix_expression(expr, env)?,
        Expression::InfixExpr(expr) => eval_infix_expression(expr, env)?,
        Expression::IfExpr(expr) => eval_if_expression(expr, env)?,
        Expression::FuncExpr(expr) => eval_func_literal(expr, env)?,
        Expression::CallExpr(expr) => {
            if expr.function.token_literal() == "quote"{
                return Ok(quote(expr.arguments[0].as_ref(), env));
            }
            eval_call_function(expr, env)?
        },
        Expression::ArrayExpr(expr) => eval_array_literal(expr, env)?,
        Expression::IndexExpr(expr) => eval_index(expr, env)?,
        Expression::HashMapExpr(expr) => eval_hashmap_literal(expr, env)?,
    };
    Ok(val)
}

fn eval_identifier(identifier: &Identifier, env: &Environment) -> Result<Value> {
    if let Some(val) = env.get(&identifier.token_literal()) {
        Ok(val.clone())
    } else {
        Err(format_err!("identifier not found: {}", identifier.token_literal()))
    }
}

fn eval_prefix_expression(expr: &PrefixExpression, env: &Environment) -> Result<Value> {
    let right_val = eval_expression(expr.right.as_ref(), env)?;

    match expr.token.clone() {
        Token::Bang => {
            Ok(Value::from(!right_val.is_truthy()))
        }
        Token::Minus => {
            match right_val.value {
                WrappedValue::IntValue(v) => Ok(Value::from(-v)),
                _ => Err(format_err!("unknown operator: {}{}", expr.operator, right_val.type_of))
            }
        }
        _ => Err(format_err!("unknown operator: {}", expr.token))
    }
}

fn eval_infix_expression(expr: &InfixExpression, env: &Environment) -> Result<Value> {
    let left = eval_expression(expr.left.as_ref(), env)?;
    let right = eval_expression(expr.right.as_ref(), env)?;

    let operator = expr.token.clone();

    if left.type_of != right.type_of {
        return Err(format_err!("type mismatch: {} {} {}", left.type_of, operator, right.type_of));
    }

    let result = match left.value {
        WrappedValue::IntValue(_) => {
            // let lval: i64 = left.try_into()?;
            let lval = i64::try_from(&left)?;
            let rval = i64::try_from(&right)?;
            match operator {
                Token::Plus => Value::from(lval + rval),
                Token::Minus => Value::from(lval - rval),
                Token::Asterisk => Value::from(lval * rval),
                Token::Slash => Value::from(lval / rval),
                Token::LT => Value::from(lval < rval),
                Token::GT => Value::from(lval > rval),
                Token::EQ => Value::from(lval == rval),
                Token::NotEq => Value::from(lval != rval),
                Token::LE => Value::from(lval <= rval),
                Token::GE => Value::from(lval >= rval),
                _ => return Err(format_err!("unknown operator: {} {} {}", left.type_of, operator, right.type_of)),
            }
        }
        WrappedValue::BoolValue(_) => {
            let lval = bool::try_from(&left)?;
            let rval = bool::try_from(&right)?;
            match operator {
                Token::EQ => Value::from(lval == rval),
                Token::NotEq => Value::from(lval != rval),
                _ => return Err(format_err!("unknown operator: {} {} {}", left.type_of, operator, right.type_of)),
            }
        }
        WrappedValue::StrValue(_) => {
            let lval = String::try_from(&left)?;
            let rval = String::try_from(&right)?;
            match operator {
                Token::Plus => Value::from(format!("{}{}", lval, rval)),
                _ => return Err(format_err!("unknown operator: {} {} {}", left.type_of, operator, right.type_of))
            }
        }
        _ => return Err(format_err!("unknown operator: {} {} {}", left.type_of, operator, right.type_of)),
    };

    Ok(result)
}

fn eval_if_expression(expr: &IfExpression, env: &Environment) -> Result<Value> {
    let condition = eval_expression(expr.condition.as_ref(), env)?;

    if condition.is_truthy() {
        eval_block_statement(&expr.consequence, env)
    } else {
        expr.alternative.as_ref().map_or(
            Ok(Value::from(NullValue)),
            |stmts| eval_block_statement(stmts.as_ref(), env))
    }
}

fn eval_func_literal(expr: &FunctionLiteral, _env: &Environment) -> Result<Value> {
    Ok(
        Value::from(
            Function {
                parameters: expr.parameters.clone(),
                body: expr.body.clone(),
            }
        )
    )
}

fn eval_call_function(expr: &CallExpression, env: &Environment) -> Result<Value> {
    // 获得函数的定义
    let func_define = eval_expression(expr.function.as_ref(), env)?;
    let result = match func_define.value {
        WrappedValue::FunValue(func) => eval_apply_func(&func, &expr.arguments, env)?,
        WrappedValue::BuiltinValue(func) => eval_builtin_func(&func, &expr.arguments, env)?,
        _ => return Err(format_err!("{} is not a function", func_define.inspect())),
    };
    Ok(result)
}


fn eval_builtin_func(func: &BuiltinFunction, args: &Vec<Box<Expression>>, env: &Environment) -> Result<Value> {
    if let Some(builtin_func) = Builtins::lookup(&func.0) {
        let actual_args = eval_func_actual_arguments(args, env)?;
        builtin_func.execute(&actual_args)
    } else {
        Err(format_err!("{} is not a function", func.0))
    }
}

fn eval_apply_func(func: &Function, args: &Vec<Box<Expression>>, env: &Environment) -> Result<Value> {
    if func.parameters.len() != args.len() {
        return Err(format_err!("this function take {} parameters but {} parameter was supplied", func.parameters.len(), args.len()));
    }

    // compute the actual arguments
    let actual_args = eval_func_actual_arguments(args, env)?;

    // extend environment
    let mut inner_env = env.new_closure();
    for (i, arg) in func.parameters.iter().enumerate() {
        let val_of_arg = actual_args.get(i).unwrap();
        inner_env.set(&arg.to_string(), val_of_arg.clone());
    }

    eval_block_statement(&func.body, &inner_env)
}

fn eval_func_actual_arguments(exprs: &Vec<Box<Expression>>, env: &Environment) -> Result<Vec<Value>> {
    let mut result = vec![];
    for expr in exprs.iter().map(|e| e.as_ref()) {
        let val = eval_expression(expr, env)?;
        result.push(val);
    }
    Ok(result)
}

fn eval_array_literal(expr: &ArrayLiteral, env: &Environment) -> Result<Value> {
    let mut values = vec![];
    for e in &expr.elements {
        match eval_expression(e.as_ref(), env) {
            Ok(v) => values.push(v),
            Err(e) => return Err(e),
        }
    }
    Ok(Value::from(values))
}

fn eval_index(expr: &IndexExpression, env: &Environment) -> Result<Value> {
    let obj = eval_expression(expr.name.as_ref(), env)?;

    let index = eval_expression(expr.index.as_ref(), env)?;
    let result = match obj.type_of{
        ObjectType::Array => {
            let values = Vec::try_from(&obj)?;
            let index = i64::try_from(&index)?;
            if index + 1 > values.len() as i64 || index < 0 {
                return Err(format_err!("index `{}` out of bound `{}`", index, values.len()));
            }
            values[index.abs() as usize].clone()
        }
        ObjectType::Hashmap => {
            let values = BTreeMap::try_from(&obj)?;
            if let Some(v) = values.get(&index){
                v.clone()
            }else{
                Value::from(NullValue)
            }
        }
        _ => return Err(format_err!("`index` expected `ARRAY` or `HASHMAP`, got `{}`", obj.type_of)),
    };

    Ok(result)
}

fn eval_hashmap_literal(expr: &HashmapLiteral, env: &Environment) -> Result<Value> {
    let mut v = BTreeMap::new();
    for pair in &expr.pairs {
        let key = eval_expression(&pair.0, env)?;
        let value = eval_expression(&pair.1, env)?;
        v.insert(key, value);
    }

    Ok(Value::from(v))
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;
    use std::fs::File;
    use std::io::BufReader;
    use crate::evaluator::environment::Environment;
    use crate::evaluator::eval;
    use crate::evaluator::object::{NullValue, Value};
    use crate::parser::Parser;


    #[test]
    fn test_eval_integer_infix_expression() {
        let cases = vec![
            ("5", Value::from(5)),
            ("10", Value::from(10)),
            ("-5", Value::from(-5)),
            ("-10", Value::from(-10)),
            ("5 + 5 + 5 + 5 - 10", Value::from(10)),
            ("2 * 2 * 2 * 2 * 2", Value::from(32)),
            ("-50 + 100 + -50", Value::from(0)),
            ("5 * 2 + 10", Value::from(20)),
            ("5 + 2 * 10", Value::from(25)),
            ("20 + 2 * -10", Value::from(0)),
            ("50 / 2 * 2 + 10", Value::from(60)),
            ("2 * (5 + 10)", Value::from(30)),
            ("3 * 3 * 3 + 10", Value::from(37)),
            ("3 * (3 * 3) + 10", Value::from(37)),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Value::from(50)),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_eval_string_expression() {
        let cases = vec![
            ("\"foo\"", Value::from("foo".to_string())),
            ("\"foo bar\"", Value::from("foo bar".to_string())),
            ("\"\"", Value::from("".to_string())),
            ("\"foo\" + \"bar\"", Value::from("foobar".to_string())),
        ];
        run_cases(&cases);
    }


    #[test]
    fn test_eval_boolean_expression() {
        let cases = vec![
            ("true", Value::from(true)),
            ("false", Value::from(false)),
            ("1 < 2", Value::from(true)),
            ("1 > 2", Value::from(false)),
            ("1 < 1", Value::from(false)),
            ("1 > 1", Value::from(false)),
            ("1 == 1", Value::from(true)),
            ("1 != 1", Value::from(false)),
            ("1 == 2", Value::from(false)),
            ("1 != 2", Value::from(true)),
            ("true == true", Value::from(true)),
            ("false == false", Value::from(true)),
            ("true == false", Value::from(false)),
            ("true != false", Value::from(true)),
            ("false != true", Value::from(true)),
            ("(1 < 2) == true", Value::from(true)),
            ("(1 < 2) == false", Value::from(false)),
            ("(1 > 2) == true", Value::from(false)),
            ("(1 > 2) == false", Value::from(true)),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_eval_bang_expression() {
        let cases = vec![
            ("!true", Value::from(false)),
            ("!false", Value::from(true)),
            ("!5", Value::from(false)),
            ("!0", Value::from(false)),
            ("!!true", Value::from(true)),
            ("!!false", Value::from(false)),
            ("!!5", Value::from(true)),
            ("!!0", Value::from(true)),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_if_else_expression() {
        let cases = vec![
            ("if (true) { 10 }", Value::from(10)),
            ("if (false) { 10 }", Value::from(NullValue)),
            ("if (1) { 10 }", Value::from(10)),
            ("if (1 < 2) { 10 }", Value::from(10)),
            ("if (1 > 2) { 10 }", Value::from(NullValue)),
            ("if (1 > 2) { 10 } else { 20 }", Value::from(20)),
            ("if (1 < 2) { 10 } else { 20 }", Value::from(10)),
        ];
        run_cases(&cases);
    }


    #[test]
    fn test_if_expression() {
        let cases = vec![
            ("if (true) { 10 }", Value::from(10)),
            ("if (1) { 10 }", Value::from(10)),
            ("if (1 < 2) { 10 }", Value::from(10)),
            ("if (1 > 2) { 10 } else { 20 }", Value::from(20)),
            ("if (1 < 2) { 10 } else { 20 }", Value::from(10)),
            ("if (false) { 10 }", Value::from(NullValue)),
            ("if (1 > 2) { 10 }", Value::from(NullValue)),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_return_statements() {
        let mut v10 = Value::from(10);
        v10.set_return();
        let mut v20 = Value::from(20);
        v20.set_return();

        let cases = vec![
            ("return 10;", v10.clone()),
            ("return 10;9;", v10.clone()),
            ("return 2*5;9;", v10.clone()),
            ("9;return 2*5;9;", v10.clone()),
            ("if (10>1) {\
                 if (10>1) {\
                    return 10;\
                 }\
                 return 1;\
             }", v10.clone()),
            ("let f = fn(x) {\
                 return x;\
                 x + 10;\
              };\
              f(10);", v10.clone()),
            ("let f = fn(x) {\
                 let result = x + 10;\
                 return result;\
                 return 10;\
              };\
              f(10);", v20.clone()),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_error_handling() {
        let cases = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("true + false + true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            ("if (10 > 1) { true + false; }", "unknown operator: BOOLEAN + BOOLEAN"),
            ("if (10 > 1) {
                if (10 > 1) {
                  return true + false;
                }
                return 1;
             }", "unknown operator: BOOLEAN + BOOLEAN"),
            ("foobar", "identifier not found: foobar"),
            ("len(1)", "[builtin/len]: mismatch type. got=`INTEGER`, want=`ARRAY` or `STRING`"),
            ("len(\"one\", \"two\")", "[builtin/len]: wrong number of arguments. got=`2`, want=`1`"),
            ("[1,2,3][4]", "index `4` out of bound `3`"),
            ("first([])", "index `0` out of bound `0`"),
            ("first([], 1)", "[builtin/first]: wrong number of arguments. got=`2`, want=`1`"),
            ("first(1)", "[builtin/first]: mismatch type. got=`INTEGER`, want=`ARRAY`"),
            ("rest([])", "[builtin/rest]: len of arg must greater than `1`"),
            ("push([])", "[builtin/push]: wrong number of arguments. got=`1`, want=`2`"),
            ("push(1, 1)", "[builtin/push]: mismatch type. got=`INTEGER`, want=`ARRAY`"),
            ("true[1]", "`index` expected `ARRAY` or `HASHMAP`, got `BOOLEAN`"),
        ];
        for (no, case) in cases.iter().enumerate() {
            let mut parser = Parser::from_string(case.0);
            let program = parser.parse().unwrap();
            assert_eq!(parser.errors.len(), 0);
            let mut env = Environment::new();
            let result = eval(&program, &mut env);
            let err_message = result.err().unwrap().to_string();
            assert_eq!(&err_message, case.1, "{}: actual is {:?}, expected is {:?}", no, &err_message, case.1);
        }
    }

    #[test]
    fn test_let_statements() {
        let cases = vec![
            ("let a = 5; a;", Value::from(5)),
            ("let a = 5 * 5; a;", Value::from(25)),
            ("let a = 5; let b = a; b;", Value::from(5)),
            ("let a = 5; let b = a; let c = a + b + 5; c;", Value::from(15)),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_function_object() {
        let cases = vec![
            ("fn(x) { x + 2}", "fn(x){(x + 2)}"),
        ];
        for (_no, &case) in cases.iter().enumerate() {
            let mut parser = Parser::from_string(case.0);
            let program = parser.parse().unwrap();
            assert_eq!(parser.errors.len(), 0);
            let mut env = Environment::new();
            let result = eval(&program, &mut env).unwrap();
            assert_eq!(result.inspect(), case.1);
        }
    }

    #[test]
    fn test_function_application() {
        let mut v5 = Value::from(5);
        v5.set_return();
        let cases = vec![
            ("let identity = fn(x) { x; }; identity(5);", Value::from(5)),
            ("let identity = fn(x) { return x; }; identity(5);", v5),
            ("let double = fn(x) { x * 2; }; double(5);", Value::from(10)),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", Value::from(10)),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", Value::from(20)),
            ("fn(x) { x; }(5)", Value::from(5)),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_enclosing_environments() {
        let cases = vec![
            ("let first = 10;\
              let second = 10;\
              let third = 10;\
              let ourFunction = fn(first) {\
                  let second = 20;\
                  first + second + third;\
              };\
              ourFunction(20) + first + second;",
             Value::from(70))
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_builtin_function() {
        let cases = vec![
            ("len(\"\")", Value::from(0)),
            ("let s = \"\"; len(s);", Value::from(0)),
            ("len(\"four\")", Value::from(4)),
            ("len(\"hello world\")", Value::from(11)),
            ("first([3,2,1])", Value::from(3)),
            ("first([true,1])", Value::from(true)),
            ("last([3,2,1])", Value::from(1)),
            ("last([true,1, false])", Value::from(false)),
            ("rest([1,2,3])", Value::from(vec![Value::from(2), Value::from(3)])),
            ("rest([1])", Value::from(vec![])),
            ("push([], 1)", Value::from(vec![Value::from(1)])),
            ("push([1], 2)", Value::from(vec![Value::from(1), Value::from(2)])),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_array_literal() {
        let cases = vec![
            ("let a = 100;[1, 1+2, 4>3, a]", Value::from(vec![
                Value::from(1), Value::from(3), Value::from(true), Value::from(100),
            ])),
            ("[]", Value::from(vec![])),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_array_index() {
        let cases = vec![
            ("[1,2,3][0]", Value::from(1)),
            ("[1,2,3][1+1]", Value::from(3)),
            ("let i = 0; [1][i]", Value::from(1)),
            ("let myArray=[1,2,3];myArray[2]", Value::from(3)),
            ("let myArray=[1,2,3];myArray[0]+myArray[1]+myArray[2]", Value::from(6)),
            ("let myArray=[1,2,3];let i=myArray[0];myArray[i]", Value::from(2)),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_array_len() {
        let cases = vec![
            ("len([])", Value::from(0)),
            ("len([1,2,3])", Value::from(3)),
            ("let i = len([1,2,3]) + 1; i", Value::from(4)),
            ("let a = [1,2,3];len(a)", Value::from(3)),
            ("let a = [1,2,4];a[len(a)-1]", Value::from(4)),
        ];
        run_cases(&cases);
    }

    fn run_cases(cases: &Vec<(&str, Value)>) {
        for (no, case) in cases.iter().enumerate() {
            let mut parser = Parser::from_string(case.0);
            let program = parser.parse().unwrap();
            assert_eq!(parser.errors.len(), 0);
            let mut env = Environment::new();
            let result = eval(&program, &mut env).unwrap();
            assert_eq!(result, case.1, "{}: actual is {:?}, expected is {:?}", no, result, case.1);
        }
    }

    #[test]
    fn test_map_on_array() {
        let mut parser = Parser::from_buf_reader(BufReader::new(File::open("map_code").unwrap()));
        let program = parser.parse().unwrap();
        let mut env = Environment::new();
        let result = eval(&program, &mut env).unwrap();
        let result = Vec::try_from(&result).unwrap();
        assert_eq!(result, vec![Value::from(2), Value::from(4), Value::from(6), Value::from(8)]);
    }

    #[test]
    fn test_reduce_on_array() {
        let mut parser = Parser::from_string("\
            let reduce = fn(arr, initial, f) {\
                let iter = fn(arr, result) {\
                    if (len(arr) == 0) {\
                        result\
                    } else {\
                        iter(rest(arr), f(result, first(arr)));\
                    }\
                };\
                iter(arr, initial);\
            };\
            let sum = fn(arr) {\
                reduce(arr, 0, fn(initial, el) { initial + el});\
            };\
            \
            sum([1,2,3,4,5]);");

        let program = parser.parse().unwrap();
        let mut env = Environment::new();
        let result = eval(&program, &mut env).unwrap();
        assert_eq!(result, Value::from(15));
    }

    #[test]
    fn test_hashmap() {
        let cases = vec![
            ("{1:1}", Value::from(BTreeMap::from([(Value::from(1), Value::from(1))]))),
            ("{1:1}", Value::from(BTreeMap::from(
                [(Value::from(1), Value::from(1))]
            ))),
            ("{true:false}", Value::from(BTreeMap::from(
                [(Value::from(true), Value::from(false))]
            ))),
            ("{\"name\": \"monkey\", \"age\": 10+10}", Value::from(BTreeMap::from(
                [(Value::from("name"), Value::from("monkey")),
                    (Value::from("age"), Value::from(20))],
            ))),
            ("{\"my\" + \"age\" :10+10}", Value::from(BTreeMap::from(
                [(Value::from("myage"), Value::from(20))]
            ))),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_hashmap_index(){
        let cases = vec![
            ("{\"name\": \"monkey\", \"age\": 10}[\"name\"]", Value::from("monkey")),
            ("{\"name\": \"monkey\", \"age\": 10}[\"age\"]", Value::from(10)),
            ("{\"name\": \"monkey\", \"age\": 10}[\"birthday\"]", Value::from(NullValue)),
        ];
        run_cases(&cases)
    }

}