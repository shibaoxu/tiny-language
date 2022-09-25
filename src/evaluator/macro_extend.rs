use crate::ast::{CallExpression, modify_program, Node, Program, Statement};
use crate::ast::Expression;
use crate::evaluator::eval_block_statement;
use crate::evaluator::environment::Environment;
use crate::evaluator::object::{MacroDefinition, Value, WrappedValue};

pub fn define_macro(program: &mut Program, env: &mut Environment) {
    let mut un_macro = vec![];
    for statement in &program.statements {
        if is_macro_definition(statement) {
            add_macro(statement, env);
        } else {
            un_macro.push(statement.clone());
        }
    }
    program.statements = un_macro;
}

fn is_macro_definition(stmt: &Statement) -> bool {
    if let Statement::LetStmt(l) = stmt {
        return match l.value {
            Expression::MacroExpr(_) => true,
            _ => false,
        };
    }
    false
}

fn add_macro(stmt: &Statement, env: &mut Environment) {
    if let Statement::LetStmt(stmt) = stmt {
        if let Expression::MacroExpr(func_literal) = &stmt.value {
            let macro_value = MacroDefinition {
                parameters: func_literal.parameters.clone(),
                body: func_literal.body.clone(),
            };
            let value = Value::from(macro_value);
            env.set(&stmt.name.to_string(), value);
        }
    }
}

pub fn expand_macro(program: &Program, env: &Environment) -> Program {
    modify_program(program, env, modifier)
}

fn modifier(expr: &Expression, env: &Environment) -> Expression {
    match expr {
        Expression::CallExpr(call_expr) => {
            let func_name = match call_expr.function.as_ref() {
                Expression::IdentExpr(identifier) => identifier.token_literal(),
                _ => panic!("{} is not a valid macro name", call_expr.function.as_ref()),
            };

            let func_definition = match env.get(&func_name) {
                None => panic!("can not find macro definition `{}` from env", func_name),
                Some(v) => v,
            };

            match func_definition.value {
                WrappedValue::MacroValue(macro_def) => {
                    let args = quote_args(call_expr);
                    let mut macro_env = env.new_closure();
                    for (i, arg) in macro_def.parameters.iter().enumerate() {
                        macro_env.set(&arg.token_literal(), args.get(i).unwrap().clone());
                    }
                    let evaluated = eval_block_statement(&macro_def.body, &macro_env);
                    if evaluated.is_err() {
                        panic!("expand macro: {}", evaluated.err().unwrap());
                    }

                    match evaluated.unwrap().value {
                        WrappedValue::Quote(expr) => expr,
                        _ => panic!("wrong return value")
                    }
                }
                _ => expr.clone(),
            }
        }
        _ => expr.clone()
    }
}


fn quote_args(expr: &CallExpression) -> Vec<Value> {
    expr.arguments.iter().map(|e| Value::from(e.as_ref().clone())).collect()
}