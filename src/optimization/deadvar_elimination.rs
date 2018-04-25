
use std::collections::HashSet;
use syntax::ast::*;
use super::EdgeData;

/// Returns a list of all variables defined by the given statement, if any.
fn defined_by(vars: &mut HashSet<String>, statement: &Statement) {
    match statement.stmt {
        Stmt::Assign { ref lhs, .. } => {
            vars.insert(String::from(&lhs.text));
        }
        Stmt::AssignArray { ref lhs, .. } => {
            vars.insert(String::from(&lhs.text));
        }
        Stmt::If { .. } |
        Stmt::While { .. } |
        Stmt::Block { .. } => panic!("Control flow graph shouldn't contain compound statements"),
        _ => (),
    }
}

fn used_by_statement(vars: &mut HashSet<String>, statement: &Statement) {
    match statement.stmt {
        Stmt::Assign { ref rhs, .. } => {
            used_by_expression(vars, rhs);
        }
        Stmt::AssignArray { ref index, ref rhs, .. } => {
            used_by_expression(vars, index);
            used_by_expression(vars, rhs);
        }
        Stmt::SideEffect { ref expression, .. } => {
            used_by_expression(vars, expression);
        }
        Stmt::Print { ref expression, .. } => {
            used_by_expression(vars, expression);
        }
        _ => panic!("Control flow graph shouldn't contain compound statements"),
    }
}

fn used_by_expression(vars: &mut HashSet<String>, expression: &Expression) {
    match expression.expr {
        Expr::Identifier(ref id) => {
            vars.insert(String::from(&id.text));
        }
        Expr::Unary(ref unary) => {
            match unary {
                UnaryExpression::NewArray(ref expr) => {
                    used_by_expression(vars, expr);
                }
                UnaryExpression::Not(ref expr) => {
                    used_by_expression(vars, expr);
                }
                UnaryExpression::Parentheses(ref expr) => {
                    used_by_expression(vars, expr);
                }
                UnaryExpression::Length(ref expr) => {
                    used_by_expression(vars, expr);
                }
                UnaryExpression::ArrayLookup { ref lhs, ref index, .. } => {
                    used_by_expression(vars, lhs);
                    used_by_expression(vars, index);
                }
                UnaryExpression::Application { ref expression, ref id, ref list, .. } => {
                    used_by_expression(vars, expression);
                    for arg in list.iter() {
                        used_by_expression(vars, arg);
                    }
                }
            }
        }
        Expr::Binary(ref binary) => {
            used_by_expression(vars, &binary.lhs);
            used_by_expression(vars, &binary.rhs);
        }
        _ => (),
    }
}