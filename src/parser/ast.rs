use super::*;

// Rule of thumb: Enums for types which have several forms (e.g. Statements)
// Structs for types which have only one form (e.g. MainClass)

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

pub struct Main<'a> {
    pub id: Identifier<'a>,
    pub args: Identifier<'a>,
    pub body: Statement<'a>,
}

pub struct Identifier<'a>(pub Token<'a>);

pub struct Class<'a> {
    pub id: Identifier<'a>,
    pub extends: Option<Extends<'a>>,
    pub variables: Vec<Variable<'a>>,
    pub functions: Vec<Function<'a>>,
}

pub struct Extends<'a> {
    pub extended: Identifier<'a>,
}

pub struct Variable<'a> {
    pub kind: Type<'a>,
    pub name: Identifier<'a>,
}

pub struct Function<'a> {
    pub kind: Type<'a>,
    pub name: Identifier<'a>,
    pub args: Vec<Argument<'a>>,
    pub variables: Vec<Variable<'a>>,
    pub statements: Vec<Statement<'a>>,
    pub expression: Expression<'a>,
}

pub enum Type<'a> {
    Id(Identifier<'a>),
    Boolean,
    String,
    Int,
    IntArray,
}

pub struct Argument<'a> {
    pub kind: Type<'a>,
    pub name: Identifier<'a>,
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

pub enum Expression<'a> {
    TrueLiteral,
    FalseLiteral,
    This,
    NewClass,
    Identifier(Identifier<'a>),
    IntLiteral(Token<'a>),
    StringLiteral(Token<'a>),
    Unary(Box<UnaryExpression<'a>>),
    Binary(Box<BinaryExpression<'a>>),
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

pub struct BinaryExpression<'a> {
    pub kind: BinaryKind,
    pub lhs: Expression<'a>,
    pub rhs: Expression<'a>,
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

pub struct ExpressionList<'a>(pub Vec<Expression<'a>>);
