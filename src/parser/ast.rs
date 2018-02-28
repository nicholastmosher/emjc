use super::*;
use super::visitor::Visitor;

// Rule of thumb: Enums for types which have several forms (e.g. Statements)
// Structs for types which have only one form (e.g. MainClass)

pub trait Node {
    fn accept<R, V: Visitor>(&mut self, visitor: &V) -> R;
}

pub enum NodeType {
    Identifier,
    Program,
    Main,
    Class,
    Extends,
    Variable,
    Function,
    Type,
    Argument,
    Statement,
    Expression,
    UnaryExpression,
    BinaryExpression,
    ExpressionList,
}

pub struct Program<'a> {
    pub main: Main<'a>,
    pub classes: Vec<Class<'a>>,
}

impl<'a> Node for Program<'a> {
    fn accept<R, V: Visitor>(&mut self, visitor: &V) -> R {
        visitor.visit_program(self)
    }
}

pub struct Main<'a> {
    pub id: Identifier<'a>,
    pub args: Identifier<'a>,
    pub body: Statement<'a>,
}

impl<'a> Node for Main<'a> {
    fn accept<R, V: Visitor>(&mut self, visitor: &V) -> R {
        visitor.visit_main(self)
    }
}

pub struct Identifier<'a>(Token<'a>);

impl<'a> Node for Identifier<'a> {
    fn accept<R, V: Visitor>(&mut self, visitor: &V) -> R {
        visitor.visit_identifier(self)
    }
}

pub struct Class<'a> {
    pub id: Identifier<'a>,
    pub extends: Option<Extends<'a>>,
    pub variables: Vec<Variable<'a>>,
    pub functions: Vec<Function<'a>>,
}

impl<'a> Node for Class<'a> {
    fn accept<R, V: Visitor>(&mut self, visitor: &V) -> R {
        visitor.visit_class(self)
    }
}

pub struct Extends<'a> {
    pub extended: Identifier<'a>,
}

impl<'a> Node for Extends<'a> {
    fn accept<R, V: Visitor>(&mut self, visitor: &V) -> R {
        visitor.visit_extends(self)
    }
}

pub struct Variable<'a> {
    pub kind: Type<'a>,
    pub name: Identifier<'a>,
}

impl<'a> Node for Variable<'a> {
    fn accept<R, V: Visitor>(&mut self, visitor: &V) -> R {
        visitor.visit_variable(self)
    }
}

pub struct Function<'a> {
    pub kind: Type<'a>,
    pub name: Identifier<'a>,
    pub args: Vec<Argument<'a>>,
    pub variables: Vec<Variable<'a>>,
    pub statements: Vec<Statement<'a>>,
    pub expression: Expression<'a>,
}

impl<'a> Node for Function<'a> {
    fn accept<R, V: Visitor>(&mut self, visitor: &V) -> R {
        visitor.visit_function(self)
    }
}

pub enum Type<'a> {
    Id(Identifier<'a>),
    Boolean,
    String,
    Int,
    IntArray,
}

impl<'a> Node for Type<'a> {
    fn accept<R, V: Visitor>(&mut self, visitor: &V) -> R {
        visitor.visit_type(self)
    }
}

pub struct Argument<'a> {
    pub kind: Type<'a>,
    pub name: Identifier<'a>,
}

impl<'a> Node for Argument<'a> {
    fn accept<R, V: Visitor>(&mut self, visitor: &V) -> R {
        visitor.visit_argument(self)
    }
}

pub enum Statement<'a> {
    Braced {
        statements: Vec<Statement<'a>>,
    },
    While {
        expression: Expression<'a>,
        statement: Box<Statement<'a>>,
    },
    Print {
        expression: Expression<'a>,
    },
    Assign {
        lhs: Identifier<'a>,
        rhs: Expression<'a>,
    },
    AssignArray {
        lhs: Identifier<'a>,
        in_bracket: Expression<'a>,
        rhs: Expression<'a>,
    },
    SideEffect {
        expression: Expression<'a>,
    },
}

impl<'a> Node for Statement<'a> {
    fn accept<R, V: Visitor>(&mut self, visitor: &V) -> R {
        visitor.visit_statement(self)
    }
}

pub enum Expression<'a> {
    IntLiteral,
    StringLiteral,
    TrueLiteral,
    FalseLiteral,
    Identifier,
    This,
    NewClass,
    Unary(Box<UnaryExpression<'a>>),
    Binary(Box<BinaryExpression<'a>>),
}

impl<'a> Node for Expression<'a> {
    fn accept<R, V: Visitor>(&mut self, visitor: &V) -> R {
        visitor.visit_expression(self)
    }
}

pub enum UnaryExpression<'a> {
    NewArray(Expression<'a>),
    Not(Expression<'a>),
    Parentheses(Expression<'a>),
    Brackets(Expression<'a>),
    Length(Expression<'a>),
    Application {
        expression: Expression<'a>,
        id: Identifier<'a>,
        list: ExpressionList<'a>,
    },
}

impl<'a> Node for UnaryExpression<'a> {
    fn accept<R, V: Visitor>(&mut self, visitor: &V) -> R {
        visitor.visit_unary_expression(self)
    }
}

pub struct BinaryExpression<'a> {
    pub kind: BinaryKind,
    pub lhs: Expression<'a>,
    pub rhs: Expression<'a>,
}

impl<'a> Node for BinaryExpression<'a> {
    fn accept<R, V: Visitor>(&mut self, visitor: &V) -> R {
        visitor.visit_binary_expression(self)
    }
}

pub enum BinaryKind {
    And,
    Or,
    Equals,
    LessThan,
    Plus,
    Minus,
    Times,
    Divide,
}

pub struct ExpressionList<'a>(Vec<Expression<'a>>);

impl<'a> Node for ExpressionList<'a> {
    fn accept<R, V: Visitor>(&mut self, visitor: &V) -> R {
        visitor.visit_expression_list(self)
    }
}
