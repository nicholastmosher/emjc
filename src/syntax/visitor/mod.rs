pub mod printer;

pub trait Visitor<T, R=()> {
    fn visit(&mut self, t: T) -> R;
}

//pub trait Visitor {
//    fn visit_program(&mut self, program: &Program);
//    fn visit_identifier(&mut self, identifier: &Identifier);
//    fn visit_type(&mut self, kind: &Type);
//    fn visit_class(&mut self, class: &Class);
//    fn visit_variable(&mut self, variable: &Variable);
//    fn visit_function(&mut self, function: &Function);
//    fn visit_statement(&mut self, statement: &Statement);
//    fn visit_expression(&mut self, expression: &Expression);
//    fn visit_unary_expression(&mut self, unary: &UnaryExpression);
//    fn visit_binary_expression(&mut self, binary: &BinaryExpression);
//}
//
//pub trait Visitable {
//    fn accept(&self, visitor: &mut Visitor);
//}
//
//impl Visitable for Program {
//    fn accept(&self, visitor: &mut Visitor) {
//        visitor.visit_program(self);
//        self.main.accept(visitor);
//        for class in self.classes.iter() {
//            class.accept(visitor);
//        }
//    }
//}
//
//impl Visitable for Identifier {
//    fn accept(&self, visitor: &mut Visitor) {
//        visitor.visit_identifier(self);
//    }
//}
//
//impl Visitable for Type {
//    fn accept(&self, visitor: &mut Visitor) {
//        visitor.visit_type(self)
//    }
//}
//
//impl Visitable for Class {
//    fn accept(&self, visitor: &mut Visitor) {
//        visitor.visit_class(self);
//        for variable in self.variables.iter() {
//            variable.accept(visitor);
//        }
//        for function in self.functions.iter() {
//            function.accept(visitor);
//        }
//    }
//}
//
//impl Visitable for Variable {
//    fn accept(&self, visitor: &mut Visitor) {
//        self.name.accept(visitor);
//        self.kind.accept(visitor);
//    }
//}
//
//impl Visitable for Function {
//    fn accept(&self, visitor: &mut Visitor) {
//        visitor.visit_function(self);
//        for variable in self.variables.iter() {
//            variable.accept(visitor);
//        }
//        for statement in self.statements.iter() {
//            statement.accept(visitor);
//        }
//    }
//}
//
//impl Visitable for Statement {
//    fn accept(&self, visitor: &mut Visitor) {
//        visitor.visit_statement(self);
//
//        use syntax::ast::Stmt::*;
//        match self.stmt {
//            Assign { ref lhs, ref rhs } => {
//                lhs.accept(visitor);
//                rhs.accept(visitor);
//            }
//            AssignArray { ref lhs, ref in_bracket, ref rhs } => {
//                lhs.accept(visitor);
//                in_bracket.accept(visitor);
//                rhs.accept(visitor);
//            }
//            Block { ref statements } => {
//                for statement in statements.iter() {
//                    statement.accept(visitor);
//                }
//            }
//            If { ref condition, ref statement, ref otherwise } => {
//                condition.accept(visitor);
//                statement.accept(visitor);
//                otherwise.as_ref().map(|otherwise| otherwise.accept(visitor));
//            }
//            Print { ref expression } => {
//                expression.accept(visitor);
//            }
//            SideEffect { ref expression } => {
//                expression.accept(visitor);
//            }
//            While { ref expression, ref statement } => {
//                expression.accept(visitor);
//                statement.accept(visitor);
//            }
//        }
//    }
//}
//
//impl Visitable for Expression {
//    fn accept(&self, visitor: &mut Visitor) {
//        visitor.visit_expression(self);
//
//        match self.expr {
//            Expr::Identifier(ref id) => id.accept(visitor),
//            Expr::Unary(ref unary) => unary.accept(visitor),
//            Expr::Binary(ref binary) => binary.accept(visitor),
//            Expr::NewClass(ref class) => class.accept(visitor),
//            _ => (),
//        }
//    }
//}
//
//impl Visitable for UnaryExpression {
//    fn accept(&self, visitor: &mut Visitor) {
//        visitor.visit_unary_expression(self);
//
//        use syntax::ast::UnaryExpression::*;
//        match *self {
//            NewArray(ref expr) => expr.accept(visitor),
//            Not(ref expr) => expr.accept(visitor),
//            Parentheses(ref expr) => expr.accept(visitor),
//            Length(ref expr) => expr.accept(visitor),
//            Application { ref expression, ref id, ref list } => {
//                expression.accept(visitor);
//                id.accept(visitor);
//                for expr in list.iter() {
//                    expr.accept(visitor);
//                }
//            }
//        }
//    }
//}
//
//impl Visitable for BinaryExpression {
//    fn accept(&self, visitor: &mut Visitor) {
//        self.lhs.accept(visitor);
//        self.rhs.accept(visitor);
//        visitor.visit_binary_expression(self);
//    }
//}
