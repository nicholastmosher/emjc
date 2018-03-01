use super::ast::*;

pub mod printer;

pub trait Visitor {
    fn visit_program(&self, program: &Program);
    fn visit_main(&self, main: &Main);
    fn visit_identifier(&self, id: &Identifier);
    fn visit_class(&self, class: &Class);
    fn visit_extends(&self, extends: &Extends);
    fn visit_variable(&self, variable: &Variable);
    fn visit_function(&self, function: &Function);
    fn visit_type(&self, ty: &Type);
    fn visit_argument(&self, argument: &Argument);
    fn visit_statement(&self, statement: &Statement);
    fn visit_expression(&self, expression: &Expression);
    fn visit_unary_expression(&self, unary_expression: &UnaryExpression);
    fn visit_binary_expression(&self, binary_expression: &BinaryExpression);
    fn visit_expression_list(&self, expression_list: &ExpressionList);
}

pub trait VisitorMut<R=()> {
    fn visit_program(&self, program: &mut Program) -> R;
    fn visit_main(&self, main: &mut Main) -> R;
    fn visit_identifier(&self, id: &mut Identifier) -> R;
    fn visit_class(&self, class: &mut Class) -> R;
    fn visit_extends(&self, extends: &mut Extends) -> R;
    fn visit_variable(&self, variable: &mut Variable) -> R;
    fn visit_function(&self, function: &mut Function) -> R;
    fn visit_type(&self, ty: &mut Type) -> R;
    fn visit_argument(&self, argument: &mut Argument) -> R;
    fn visit_statement(&self, statement: &mut Statement) -> R;
    fn visit_expression(&self, expression: &mut Expression) -> R;
    fn visit_unary_expression(&self, unary_expression: &mut UnaryExpression) -> R;
    fn visit_binary_expression(&self, binary_expression: &mut BinaryExpression) -> R;
    fn visit_expression_list(&self, expression_list: &mut ExpressionList) -> R;
}
