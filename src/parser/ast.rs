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
    Unary(Box<UnaryExpression>),
    Binary(Box<BinaryExpression>),
}

#[derive(Debug)]
pub enum UnaryExpression {
    NewArray(Expression),
    Not(Expression),
    Parentheses(Expression),
    Brackets(Expression),
    Length(Expression),
    Application {
        expression: Expression,
        id: Identifier,
        list: ExpressionList,
    },
}

#[derive(Debug)]
pub struct BinaryExpression {
    pub kind: BinaryKind,
    pub lhs: Expression,
    pub rhs: Expression,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct ExpressionList(pub Vec<Expression>);
