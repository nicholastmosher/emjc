use super::*;

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Program {
    pub main: Main,
    pub classes: Vec<Class>,
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Main {
    pub id: Identifier,
    pub args: Identifier,
    pub body: Statement,
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Identifier(pub Token);

impl From<Token> for Identifier {
    fn from(token: Token) -> Self {
        Identifier(token)
    }
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Class {
    pub id: Identifier,
    pub extends: Option<Extends>,
    pub variables: Vec<Variable>,
    pub functions: Vec<Function>,
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Extends {
    pub extended: Identifier,
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Variable {
    pub kind: Type,
    pub name: Identifier,
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Function {
    pub kind: Type,
    pub name: Identifier,
    pub args: Vec<Argument>,
    pub variables: Vec<Variable>,
    pub statements: Vec<Statement>,
    pub expression: Expression,
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum Type {
    Id(Identifier),
    Boolean,
    String,
    Int,
    IntArray,
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Argument {
    pub kind: Type,
    pub name: Identifier,
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum Statement {
    Block {
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

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
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

impl Expression {
    pub fn associate_left(self) -> Expression {
        if let Expression::Binary(binary) = self {
            binary.associate_left().into()
        } else { self }
    }
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
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

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct BinaryExpression {
    pub kind: BinaryKind,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Hash, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
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

impl BinaryExpression {
    /// Takes a BinaryExpression and left-associates all of the operators of the same kind.
    pub fn associate_left(self) -> BinaryExpression {
        let kind = self.kind;
        let mut parent = self;

        loop {
            // If the right hand side is a binary operator of the same kind, rotate left.
            let BinaryExpression { lhs: parent_lhs, rhs: parent_rhs, .. } = parent;
            if let Expression::Binary(binary) = *parent_rhs {
                if binary.kind == kind {
                    let BinaryExpression { lhs: right_lhs, rhs: right_rhs, .. } = binary;
                    let child = BinaryExpression {
                        kind,
                        lhs: parent_lhs,
                        rhs: right_lhs
                    };
                    parent = BinaryExpression { kind, lhs: Box::new(child.into()), rhs: right_rhs }
                } else {
                    break BinaryExpression { kind, lhs: parent_lhs, rhs: Box::new(binary.into()) };
                }
            } else {
                break BinaryExpression { kind, lhs: parent_lhs, rhs: parent_rhs };
            }
        }
    }
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct ExpressionList(pub Vec<Expression>);
