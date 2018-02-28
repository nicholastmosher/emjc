use super::ast::*;

pub trait Visitor {
    fn visit_program<R>(&self, program: &mut Program) -> R;
    fn visit_main<R>(&self, main: &mut Main) -> R;
    fn visit_identifier<R>(&self, id: &mut Identifier) -> R;
    fn visit_class<R>(&self, id: &mut Class) -> R;
    fn visit_extends<R>(&self, id: &mut Extends) -> R;
    fn visit_variable<R>(&self, id: &mut Variable) -> R;
    fn visit_function<R>(&self, id: &mut Function) -> R;
    fn visit_type<R>(&self, id: &mut Type) -> R;
    fn visit_argument<R>(&self, id: &mut Argument) -> R;
    fn visit_statement<R>(&self, id: &mut Statement) -> R;
    fn visit_expression<R>(&self, id: &mut Expression) -> R;
    fn visit_unary_expression<R>(&self, id: &mut UnaryExpression) -> R;
    fn visit_binary_expression<R>(&self, id: &mut BinaryExpression) -> R;
    fn visit_expression_list<R>(&self, id: &mut ExpressionList) -> R;
}

