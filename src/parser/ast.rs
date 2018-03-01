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

pub struct Program {
    pub main: Main,
    pub classes: Vec<Class>,
}

pub struct Main {
    pub id: Identifier,
    pub args: Identifier,
    pub body: Statement,
}

pub struct Identifier(pub Token);

impl From<Token> for Identifier {
    fn from(token: Token) -> Self {
        Identifier(token)
    }
}

pub struct Class {
    pub id: Identifier,
    pub extends: Option<Extends>,
    pub variables: Vec<Variable>,
    pub functions: Vec<Function>,
}

pub struct Extends {
    pub extended: Identifier,
}

pub struct Variable {
    pub kind: Type,
    pub name: Identifier,
}

pub struct Function {
    pub kind: Type,
    pub name: Identifier,
    pub args: Vec<Argument>,
    pub variables: Vec<Variable>,
    pub statements: Vec<Statement>,
    pub expression: Expression,
}

pub enum Type {
    Id(Identifier),
    Boolean,
    String,
    Int,
    IntArray,
}

pub struct Argument {
    pub kind: Type,
    pub name: Identifier,
}

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

pub struct BinaryExpression {
    pub kind: BinaryKind,
    pub lhs: Expression,
    pub rhs: Expression,
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

pub struct ExpressionList(pub Vec<Expression>);
