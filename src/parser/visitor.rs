use super::ast::*;

pub trait Visitor {
    type Output;
    fn visit_program(&self, program: &mut Program) -> Self::Output;
    fn visit_main(&self, main: &mut Main) -> Self::Output;
    fn visit_identifier(&self, id: &mut Identifier) -> Self::Output;
    fn visit_class(&self, id: &mut Class) -> Self::Output;
    fn visit_extends(&self, id: &mut Extends) -> Self::Output;
    fn visit_variable(&self, id: &mut Variable) -> Self::Output;
    fn visit_function(&self, id: &mut Function) -> Self::Output;
    fn visit_type(&self, id: &mut Type) -> Self::Output;
    fn visit_argument(&self, id: &mut Argument) -> Self::Output;
    fn visit_statement(&self, id: &mut Statement) -> Self::Output;
    fn visit_expression(&self, id: &mut Expression) -> Self::Output;
    fn visit_unary_expression(&self, id: &mut UnaryExpression) -> Self::Output;
    fn visit_binary_expression(&self, id: &mut BinaryExpression) -> Self::Output;
    fn visit_expression_list(&self, id: &mut ExpressionList) -> Self::Output;
}

