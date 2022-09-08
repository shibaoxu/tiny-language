use crate::ast::{BlockStatement, CallExpression, Expression, ExpressionStatement, FunctionLiteral, Identifier, IfExpression, InfixExpression, LetStatement, Node, PrefixExpression, Program, ReturnStatement, Statement};
use crate::evaluator::environment::Environment;
use crate::evaluator::object::{BuiltinObject, EvalResult, EvalValue, FunctionObject};
use crate::TokenType;

pub mod object;
pub mod environment;
mod builtins;

pub fn eval(program: &Program, env: &mut Environment) -> EvalResult {
    eval_statements(&program.statements, env)
}

fn eval_statements(statements: &Vec<Statement>, env: &mut Environment) -> EvalResult {
    let mut result = EvalResult::new_null_object();
    for stmt in statements {
        result = eval_statement(stmt, env);
        if result.is_return() {
            return result;
        }
    }
    result
}

fn eval_statement(stmt: &Statement, env: &mut Environment) -> EvalResult {
    match stmt {
        Statement::LetStmt(stmt) => eval_let_stmt(stmt, env),
        Statement::ReturnStmt(stmt) => eval_return_stmt(stmt, env),
        Statement::BlockStmt(stmt) => eval_block_statement(stmt, env),
        Statement::ExpStmt(stmt) => eval_expression_stmt(stmt, env),
    }
}

fn eval_let_stmt(stmt: &LetStatement, env: &mut Environment) -> EvalResult {
    let val = eval_expression(&stmt.value, env);
    match val {
        EvalResult::ErrorObj(_) => val,
        _ => {
            env.set(&stmt.name.to_string(), val.clone());
            val
        }
    }
}

fn eval_return_stmt(stmt: &ReturnStatement, env: &Environment) -> EvalResult {
    let mut val = eval_expression(&stmt.value, env);
    val.set_return();
    val
}

fn eval_block_statement(stmt: &BlockStatement, env: &Environment) -> EvalResult {
    let mut inner_env = env.new_closure();
    eval_statements(&stmt.statements, &mut inner_env)
}

fn eval_expression_stmt(stmt: &ExpressionStatement, env: &mut Environment) -> EvalResult {
    eval_expression(&stmt.expression, env)
}

fn eval_expression(expr: &Expression, env: &Environment) -> EvalResult {
    match expr {
        Expression::Identifier(expr) => eval_identifier(expr, env),
        Expression::BoolLiteral(expr) => EvalResult::new_bool_object(expr.value),
        Expression::IntLiteral(expr) => EvalResult::new_int_object(expr.value),
        Expression::StrLiteral(expr) => EvalResult::new_string_object(&expr.value),
        Expression::PrefixExpr(expr) => eval_prefix_expression(expr, env),
        Expression::InfixExpr(expr) => eval_infix_expression(expr, env),
        Expression::IfExpr(expr) => eval_if_expression(expr, env),
        Expression::FuncLiteral(expr) => eval_func_literal(expr, env),
        Expression::CallExpr(expr) => eval_call_function(expr, env),
    }
}

fn eval_identifier(identifier: &Identifier, env: &Environment) -> EvalResult {
    if let Some(val) = env.get(&identifier.token_literal()) {
        val.clone()
    } else {
        EvalResult::new_error_object(&format!("identifier not found: {}", identifier.token_literal()))
    }
}

fn eval_prefix_expression(expr: &PrefixExpression, env: &Environment) -> EvalResult {
    let right_val = eval_expression(expr.right.as_ref(), env);
    if let EvalResult::ErrorObj(_) = right_val {
        return right_val;
    }

    match expr.token.token_type {
        TokenType::Bang => {
            EvalResult::new_bool_object(!right_val.is_truthy())
        }
        TokenType::Minus => {
            match right_val {
                EvalResult::IntObj(v) => EvalResult::new_int_object(-v.value),
                _ => EvalResult::new_error_object(&format!("unknown operator: {}{}", expr.operator, right_val.get_type()))
            }
        }
        _ => EvalResult::new_error_object(&format!("unknown operator: {}", expr.token.token_type))
    }
}

fn eval_infix_expression(expr: &InfixExpression, env: &Environment) -> EvalResult {
    let left = eval_expression(expr.left.as_ref(), env);
    if left.is_error() {
        return left;
    }

    let right = eval_expression(expr.right.as_ref(), env);
    if right.is_error() {
        return right;
    }

    let operator = expr.token.token_type;

    if std::mem::discriminant(&left) != std::mem::discriminant(&right) {
        return EvalResult::new_error_object(&format!("type mismatch: {} {} {}", left.get_type(), operator, right.get_type()));
    }
    if left.is_error() || right.is_error() {
        return left;
    }

    match left {
        EvalResult::IntObj(_) => {
            let lval = left.convert_to_int().unwrap();
            let rval = right.convert_to_int().unwrap();
            match operator {
                TokenType::Plus => EvalResult::new_int_object(lval + rval),
                TokenType::Minus => EvalResult::new_int_object(lval - rval),
                TokenType::Asterisk => EvalResult::new_int_object(lval * rval),
                TokenType::Slash => EvalResult::new_int_object(lval / rval),
                TokenType::LT => EvalResult::new_bool_object(lval < rval),
                TokenType::GT => EvalResult::new_bool_object(lval > rval),
                TokenType::EQ => EvalResult::new_bool_object(lval == rval),
                TokenType::NotEq => EvalResult::new_bool_object(lval != rval),
                TokenType::LE => EvalResult::new_bool_object(lval <= rval),
                TokenType::GE => EvalResult::new_bool_object(lval >= rval),
                _ => EvalResult::new_error_object(&format!("unknown operator: {} {} {}", left.get_type(), operator, right.get_type())),
            }
        }
        EvalResult::BoolObj(_) => {
            let lval = left.convert_to_bool().unwrap();
            let rval = right.convert_to_bool().unwrap();
            match operator {
                TokenType::EQ => EvalResult::new_bool_object(lval == rval),
                TokenType::NotEq => EvalResult::new_bool_object(lval != rval),
                _ => EvalResult::new_error_object(&format!("unknown operator: {} {} {}", left.get_type(), operator, right.get_type())),
            }
        }
        EvalResult::StrObj(_) => {
            let lval = left.convert_to_string().unwrap();
            let rval = right.convert_to_string().unwrap();
            match operator {
                TokenType::Plus => {
                    EvalResult::new_string_object(&format!("{}{}", lval, rval))
                }
                _ => EvalResult::new_error_object(&format!("unknown operator: {} {} {}", left.get_type(), operator, right.get_type()))
            }
        }
        EvalResult::ErrorObj(_) => panic!("impossible executed position"),
        _ => EvalResult::new_error_object(&format!("unknown operator: {} {} {}", left.get_type(), operator, right.get_type())),
    }
}

fn eval_if_expression(expr: &IfExpression, env: &Environment) -> EvalResult {
    let condition = eval_expression(expr.condition.as_ref(), env);
    if condition.is_error() {
        return condition;
    }

    if condition.is_truthy() {
        eval_block_statement(&expr.consequence, env)
    } else {
        expr.alternative.as_ref().map_or(
            EvalResult::new_null_object(),
            |stmts| eval_block_statement(stmts.as_ref(), env))
    }
}

fn eval_func_literal(expr: &FunctionLiteral, _env: &Environment) -> EvalResult {
    EvalResult::new_function_object(expr.parameters.clone(), expr.body.clone())
}

fn eval_call_function(expr: &CallExpression, env: &Environment) -> EvalResult {

    // 获得函数的定义
    let func_define = eval_expression(expr.function.as_ref(), env);
    match func_define {
        EvalResult::FunObj(func) => eval_apply_func(&func, &expr.arguments, env),
        EvalResult::BuiltinObj(func) => eval_builtin_func(&func, &expr.arguments, env),
        EvalResult::ErrorObj(_) => func_define,
        _ => EvalResult::new_error_object(&format!("{} is not a function", func_define.inspect())),
    }
}


fn eval_builtin_func(func: &BuiltinObject, args: &Vec<Box<Expression>>, env: &Environment) -> EvalResult {
    let actual_args = eval_func_actual_arguments(args, env);
    func.function.execute(&actual_args)
}

fn eval_apply_func(func: &FunctionObject, args: &Vec<Box<Expression>>, env: &Environment) -> EvalResult {
    if func.parameters.len() != args.len() {
        return EvalResult::new_error_object(&format!("this function take {} parameters but {} parameter was supplied", func.parameters.len(), args.len()));
    }

    // compute the actual arguments
    let actual_args = eval_func_actual_arguments(args, env);
    let error_arg = actual_args.iter().find(|e| e.is_error());
    if error_arg.is_some(){
        return error_arg.unwrap().clone();
    }

    // extend environment
    let mut inner_env = env.new_closure();
    for (i, arg) in func.parameters.iter().enumerate() {
        let val_of_arg = actual_args.get(i).unwrap().clone();
        inner_env.set(&arg.token_literal(), val_of_arg);
    }

    eval_block_statement(&func.body, &inner_env)
}

// 用于计算参数的实际值
fn eval_func_actual_arguments(exprs: &Vec<Box<Expression>>, env: &Environment) -> Vec<EvalResult> {
    let mut result = vec![];
    for expr in exprs.iter().map(|e| e.as_ref()) {
        let val = eval_expression(expr, env);
        result.push(val);
    }
    result
}

#[cfg(test)]
mod tests {
    use crate::evaluator::environment::Environment;
    use crate::evaluator::eval;
    use crate::evaluator::object::{EvalResult, EvalValue};
    use crate::parser::Parser;

    #[test]
    fn test_eval_integer_infix_expression() {
        let cases = vec![
            ("5", EvalResult::new_int_object(5)),
            ("10", EvalResult::new_int_object(10)),
            ("-5", EvalResult::new_int_object(-5)),
            ("-10", EvalResult::new_int_object(-10)),
            ("5 + 5 + 5 + 5 - 10", EvalResult::new_int_object(10)),
            ("2 * 2 * 2 * 2 * 2", EvalResult::new_int_object(32)),
            ("-50 + 100 + -50", EvalResult::new_int_object(0)),
            ("5 * 2 + 10", EvalResult::new_int_object(20)),
            ("5 + 2 * 10", EvalResult::new_int_object(25)),
            ("20 + 2 * -10", EvalResult::new_int_object(0)),
            ("50 / 2 * 2 + 10", EvalResult::new_int_object(60)),
            ("2 * (5 + 10)", EvalResult::new_int_object(30)),
            ("3 * 3 * 3 + 10", EvalResult::new_int_object(37)),
            ("3 * (3 * 3) + 10", EvalResult::new_int_object(37)),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", EvalResult::new_int_object(50)),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_eval_string_expression() {
        let cases = vec![
            ("\"foo\"", EvalResult::new_string_object("foo")),
            ("\"foo bar\"", EvalResult::new_string_object("foo bar")),
            ("\"\"", EvalResult::new_string_object("")),
            ("\"foo\" + \"bar\"", EvalResult::new_string_object("foobar")),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_eval_boolean_expression() {
        let cases = vec![
            ("true", EvalResult::new_bool_object(true)),
            ("false", EvalResult::new_bool_object(false)),
            ("1 < 2", EvalResult::new_bool_object(true)),
            ("1 > 2", EvalResult::new_bool_object(false)),
            ("1 < 1", EvalResult::new_bool_object(false)),
            ("1 > 1", EvalResult::new_bool_object(false)),
            ("1 == 1", EvalResult::new_bool_object(true)),
            ("1 != 1", EvalResult::new_bool_object(false)),
            ("1 == 2", EvalResult::new_bool_object(false)),
            ("1 != 2", EvalResult::new_bool_object(true)),
            ("true == true", EvalResult::new_bool_object(true)),
            ("false == false", EvalResult::new_bool_object(true)),
            ("true == false", EvalResult::new_bool_object(false)),
            ("true != false", EvalResult::new_bool_object(true)),
            ("false != true", EvalResult::new_bool_object(true)),
            ("(1 < 2) == true", EvalResult::new_bool_object(true)),
            ("(1 < 2) == false", EvalResult::new_bool_object(false)),
            ("(1 > 2) == true", EvalResult::new_bool_object(false)),
            ("(1 > 2) == false", EvalResult::new_bool_object(true)),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_eval_bang_expression() {
        let cases = vec![
            ("!true", EvalResult::new_bool_object(false)),
            ("!false", EvalResult::new_bool_object(true)),
            ("!5", EvalResult::new_bool_object(false)),
            ("!0", EvalResult::new_bool_object(false)),
            ("!!true", EvalResult::new_bool_object(true)),
            ("!!false", EvalResult::new_bool_object(false)),
            ("!!5", EvalResult::new_bool_object(true)),
            ("!!0", EvalResult::new_bool_object(true)),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_if_else_expression() {
        let cases = vec![
            ("if (true) { 10 }", EvalResult::new_int_object(10)),
            ("if (false) { 10 }", EvalResult::new_null_object()),
            ("if (1) { 10 }", EvalResult::new_int_object(10)),
            ("if (1 < 2) { 10 }", EvalResult::new_int_object(10)),
            ("if (1 > 2) { 10 }", EvalResult::new_null_object()),
            ("if (1 > 2) { 10 } else { 20 }", EvalResult::new_int_object(20)),
            ("if (1 < 2) { 10 } else { 20 }", EvalResult::new_int_object(10)),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_if_expression() {
        let cases = vec![
            ("if (true) { 10 }", EvalResult::new_int_object(10)),
            ("if (1) { 10 }", EvalResult::new_int_object(10)),
            ("if (1 < 2) { 10 }", EvalResult::new_int_object(10)),
            ("if (1 > 2) { 10 } else { 20 }", EvalResult::new_int_object(20)),
            ("if (1 < 2) { 10 } else { 20 }", EvalResult::new_int_object(10)),
            ("if (false) { 10 }", EvalResult::new_null_object()),
            ("if (1 > 2) { 10 }", EvalResult::new_null_object()),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_return_statements() {
        let cases = vec![
            ("return 10;", EvalResult::new_int_object_with_return(10)),
            ("return 10;9;", EvalResult::new_int_object_with_return(10)),
            ("return 2*5;9;", EvalResult::new_int_object_with_return(10)),
            ("9;return 2*5;9;", EvalResult::new_int_object_with_return(10)),
            ("if (10>1) {\
                 if (10>1) {\
                    return 10;\
                 }\
                 return 1;\
             }", EvalResult::new_int_object_with_return(10)),
            ("let f = fn(x) {\
                 return x;\
                 x + 10;\
              };\
              f(10);", EvalResult::new_int_object_with_return(10)),
            ("let f = fn(x) {\
                 let result = x + 10;\
                 return result;\
                 return 10;\
              };\
              f(10);", EvalResult::new_int_object_with_return(20)),
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_error_handling() {
        let cases = vec![
            ("5 + true;", EvalResult::new_error_object("type mismatch: INTEGER + BOOLEAN")),
            ("5 + true; 5;", EvalResult::new_error_object("type mismatch: INTEGER + BOOLEAN")),
            ("-true", EvalResult::new_error_object("unknown operator: -BOOLEAN")),
            ("true + false;", EvalResult::new_error_object("unknown operator: BOOLEAN + BOOLEAN")),
            ("true + false + true + false;", EvalResult::new_error_object("unknown operator: BOOLEAN + BOOLEAN")),
            ("5; true + false; 5", EvalResult::new_error_object("unknown operator: BOOLEAN + BOOLEAN")),
            ("if (10 > 1) { true + false; }", EvalResult::new_error_object("unknown operator: BOOLEAN + BOOLEAN")),
            ("if (10 > 1) {
                if (10 > 1) {
                  return true + false;
                }
                return 1;
             }", EvalResult::new_error_object("unknown operator: BOOLEAN + BOOLEAN")),
            ("foobar", EvalResult::new_error_object("identifier not found: foobar")),
        ];
        run_cases(&cases);
    }


    #[test]
    fn test_let_statements() {
        let cases = vec![
            ("let a = 5; a;", EvalResult::new_int_object(5)),
            ("let a = 5 * 5; a;", EvalResult::new_int_object(25)),
            ("let a = 5; let b = a; b;", EvalResult::new_int_object(5)),
            ("let a = 5; let b = a; let c = a + b + 5; c;", EvalResult::new_int_object(15)),
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
            let program = parser.parse();
            assert_eq!(parser.errors.len(), 0);
            let mut env = Environment::new();
            let result = eval(&program, &mut env);
            assert_eq!(result.inspect(), case.1);
        }
    }

    #[test]
    fn test_function_application() {
        let cases = vec![
            ("let identity = fn(x) { x; }; identity(5);", EvalResult::new_int_object(5)),
            ("let identity = fn(x) { return x; }; identity(5);", EvalResult::new_int_object_with_return(5)),
            ("let double = fn(x) { x * 2; }; double(5);", EvalResult::new_int_object(10)),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", EvalResult::new_int_object(10)),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", EvalResult::new_int_object(20)),
            ("fn(x) { x; }(5)", EvalResult::new_int_object(5)),
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
             EvalResult::new_int_object(70))
        ];
        run_cases(&cases);
    }

    #[test]
    fn test_builtin_function(){
        let cases = vec![
            ("len(\"\")", EvalResult::new_int_object(0)),
            ("let s = \"\"; len(s);", EvalResult::new_int_object(0)),
            ("len(\"four\")", EvalResult::new_int_object(4)),
            ("len(\"hello world\")", EvalResult::new_int_object(11)),
            ("len(1)", EvalResult::new_error_object("argument to `len` not supported, got INTEGER")),
            ("len(\"one\", \"two\")", EvalResult::new_error_object("wrong number of arguments. got=2, want=1")),

        ];
        run_cases(&cases);
    }

    fn run_cases(cases: &Vec<(&str, EvalResult)>) {
        for (no, case) in cases.iter().enumerate() {
            let mut parser = Parser::from_string(case.0);
            let program = parser.parse();
            assert_eq!(parser.errors.len(), 0);
            let mut env = Environment::new();
            let result = eval(&program, &mut env);
            assert_eq!(result, case.1, "{}: actual is {:?}, expected is {:?}", no, result, case.1)
        }
    }
}