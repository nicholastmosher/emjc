use super::*;

#[derive(Debug)]
pub struct Program {
    pub main: Main,
    pub classes: Vec<Class>,
}

#[derive(Debug)]
pub struct Main {
    pub id: Identifier,
    pub args: Identifier,
    pub body: Statement,
}

#[derive(Debug)]
pub struct Identifier(pub Token);

impl From<Token> for Identifier {
    fn from(token: Token) -> Self {
        Identifier(token)
    }
}

#[derive(Debug)]
pub struct Class {
    pub id: Identifier,
    pub extends: Option<Extends>,
    pub variables: Vec<Variable>,
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Extends {
    pub extended: Identifier,
}

#[derive(Debug)]
pub struct Variable {
    pub kind: Type,
    pub name: Identifier,
}

#[derive(Debug)]
pub struct Function {
    pub kind: Type,
    pub name: Identifier,
    pub args: Vec<Argument>,
    pub variables: Vec<Variable>,
    pub statements: Vec<Statement>,
    pub expression: Expression,
}

#[derive(Debug)]
pub enum Type {
    Id(Identifier),
    Boolean,
    String,
    Int,
    IntArray,
}

#[derive(Debug)]
pub struct Argument {
    pub kind: Type,
    pub name: Identifier,
}

#[derive(Debug)]
pub enum Statement {
    Braced {
        statements: Vec<Statement>,
    },
    While {
        expression: Expression,
        statement: Box<Statement>,
    },
    Print {
        expression: Expression,
    },
    Assign {
        lhs: Identifier,
        rhs: Expression,
    },
    AssignArray {
        lhs: Identifier,
        in_bracket: Expression,
        rhs: Expression,
    },
    SideEffect {
        expression: Expression,
    },
    If {
        condition: Expression,
        statement: Box<Statement>,
        otherwise: Option<Box<Statement>>,
    },
}

#[derive(Debug)]
pub enum Expression {
    TrueLiteral,
    FalseLiteral,
    This,
    NewClass(Identifier),
    Identifier(Identifier),
    IntLiteral(Token),
    StringLiteral(Token),
    Unary(UnaryExpression),
    Binary(BinaryExpression),
}

impl From<UnaryExpression> for Expression {
    fn from(unary: UnaryExpression) -> Self {
        Expression::Unary(unary)
    }
}

impl From<BinaryExpression> for Expression {
    fn from(binary: BinaryExpression) -> Self {
        Expression::Binary(binary)
    }
}

#[derive(Debug)]
pub enum UnaryExpression {
    NewArray(Box<Expression>),
    Not(Box<Expression>),
    Parentheses(Box<Expression>),
    Length(Box<Expression>),
    Application {
        expression: Box<Expression>,
        id: Identifier,
        list: ExpressionList,
    },
}

#[derive(Debug)]
pub struct BinaryExpression {
    pub kind: BinaryKind,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum BinaryKind {
    And,
    Or,
    Equals,
    LessThan,
    Plus,
    Minus,
    Times,
    Divide,
    ArrayLookup,
}

#[derive(Debug)]
pub struct ExpressionList(pub Vec<Expression>);
