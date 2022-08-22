use std::any::{Any, TypeId};
use crate::ast::{BooleanLiteral, ExpressionStatement, InfixExpression, IntegerLiteral, LetStatement, Node, PrefixExpression, Program, ReturnStatement, Statement};
use crate::evaluator::object::{BooleanObject, IntegerObject, NullObject, Object, ObjectType};
use crate::evaluator::object::ObjectType::Boolean;
use crate::TokenType;

mod object;

pub fn eval(node: &dyn Node) -> Box<dyn Object> {
    let value_any = node as &dyn Any;
    let type_id = value_any.type_id();
    return if type_id == TypeId::of::<Program>() {
        let program = value_any.downcast_ref::<Program>().unwrap();
        eval_program(&program)
    } else if type_id == TypeId::of::<ExpressionStatement>() {
        let exp_statement = value_any.downcast_ref::<ExpressionStatement>().unwrap();
        eval(exp_statement.expression.as_ref())
    } else if type_id == TypeId::of::<IntegerLiteral>() {
        let integer_literal = value_any.downcast_ref::<IntegerLiteral>().unwrap();
        Box::new(IntegerObject::from(integer_literal.value))
    } else if type_id == TypeId::of::<BooleanLiteral>() {
        let bool_literal = value_any.downcast_ref::<BooleanLiteral>().unwrap();
        Box::new(BooleanObject::from(bool_literal.value))
    } else if type_id == TypeId::of::<PrefixExpression>() {
        let prefix_exp = value_any.downcast_ref::<PrefixExpression>().unwrap();
        let right = eval(prefix_exp.right.as_ref());
        match prefix_exp.token.token_type {
            TokenType::Bang => eval_bang_operator_expression(right),
            TokenType::Minus => eval_minus_operator_expression(right),
            _ => {
                //todo: 语法错误
                Box::new(NullObject)
            }
        }
    } else if type_id == TypeId::of::<InfixExpression>() {
        let infix_exp = value_any.downcast_ref::<InfixExpression>().unwrap();
        let left = eval(infix_exp.left.as_ref());
        let right = eval(infix_exp.right.as_ref());
        eval_infix_expression(infix_exp.token.token_type, left.as_ref(), right.as_ref())
    } else {
        Box::new(NullObject)
    };
}

fn eval_program(program: &Program) -> Box<dyn Object> {
    let mut result: Box<dyn Object> = Box::new(NullObject);
    for stmt in program.statements.iter().map(|e| e.as_ref()) {
        let value_any = stmt as &dyn Any;
        let type_id = value_any.type_id();
        result = if type_id == TypeId::of::<LetStatement>() {
            let let_statement = value_any.downcast_ref::<LetStatement>().unwrap();
            eval(let_statement)
        } else if type_id == TypeId::of::<ReturnStatement>() {
            let return_statement = value_any.downcast_ref::<ReturnStatement>().unwrap();
            eval(return_statement)
        } else {
            let expression_statement = value_any.downcast_ref::<ExpressionStatement>().unwrap();
            eval(expression_statement)
        };
    }
    result
}

fn eval_bang_operator_expression(right: Box<dyn Object>) -> Box<dyn Object> {
    let right_any = right.as_ref() as &dyn Any;
    let type_id = right_any.type_id();
    if type_id == TypeId::of::<BooleanObject>() {
        let actual = right_any.downcast_ref::<BooleanObject>().unwrap();
        Box::new(BooleanObject::from(!actual.value))
    } else if type_id == TypeId::of::<IntegerObject>() {
        let actual = right_any.downcast_ref::<IntegerObject>().unwrap();
        match actual.value {
            0 => Box::new(BooleanObject::from(true)),
            _ => Box::new(BooleanObject::from(false)),
        }
    } else if type_id == TypeId::of::<NullObject>() {
        Box::new(BooleanObject::from(true))
    } else {
        //todo: 语法错误
        Box::new(NullObject)
    }
}

fn eval_minus_operator_expression(right: Box<dyn Object>) -> Box<dyn Object> {
    let right_any = right.as_ref() as &dyn Any;
    let type_id = right_any.type_id();
    if type_id == TypeId::of::<IntegerObject>() {
        let actual = right_any.downcast_ref::<IntegerObject>().unwrap();
        Box::new(IntegerObject::from(-actual.value))
    } else {
        //todo: 语法错误
        Box::new(NullObject)
    }
}

fn eval_infix_expression(operator: TokenType, left: &dyn Object, right: &dyn Object) -> Box<dyn Object> {
    let left_any = left as &dyn Any;
    let right_any = right as &dyn Any;
    if left_any.type_id() != right_any.type_id() {
        //todo: syntax error
        return Box::new(NullObject);
    }

    if left_any.type_id() == TypeId::of::<IntegerObject>() {
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
            _ => Box::new(NullObject) as Box<dyn Object>,
        }
    } else if left_any.type_id() == TypeId::of::<BooleanObject>() {
        let left_bool = left_any.downcast_ref::<BooleanObject>().unwrap();
        let right_bool = right_any.downcast_ref::<BooleanObject>().unwrap();
        match operator {
            TokenType::EQ => Box::new(BooleanObject::from(left_bool.value == right_bool.value)) as Box<dyn Object>,
            TokenType::NotEq => Box::new(BooleanObject::from(left_bool.value != right_bool.value)) as Box<dyn Object>,
            _ => Box::new(NullObject) as Box<dyn Object>,
        }
    } else {
        //todo: syntax error
        Box::new(NullObject) as Box<dyn Object>
    }
}

#[cfg(test)]
mod tests {
    use std::any::{Any, TypeId};
    use std::fmt::Debug;
    use crate::evaluator::eval;
    use crate::evaluator::object::{BooleanObject, IntegerObject, Object, ObjectType};
    use crate::evaluator::object::ObjectType::Boolean;
    use crate::parser::Parser;

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

    fn run_cases<T: Object + Debug>(cases: &Vec<(&str, T)>) {
        for (no, case) in cases.iter().enumerate() {
            let mut parser = Parser::from_string(case.0);
            let program = parser.parse();
            assert_eq!(parser.errors.len(), 0);
            let result = eval(&program);
            assert!(equal(result.as_ref(), &case.1), "{}: actual is {:?}, expected is {:?}", no, result.inspect(), case.1)
        }
    }

    fn equal(left: &dyn Object, right: &dyn Object) -> bool {
        let type_id = left.type_id();
        if left.type_id() != right.type_id() {
            return false;
        }

        let left_any = left as &dyn Any;
        let right_any = right as &dyn Any;
        return if type_id == TypeId::of::<BooleanObject>() {
            left_any.downcast_ref::<BooleanObject>().unwrap().value == right_any.downcast_ref::<BooleanObject>().unwrap().value
        } else if type_id == TypeId::of::<IntegerObject>() {
            left_any.downcast_ref::<IntegerObject>().unwrap().value == right_any.downcast_ref::<IntegerObject>().unwrap().value
        } else {
            true
        };
    }
}