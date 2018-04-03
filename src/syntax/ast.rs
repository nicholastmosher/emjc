use super::*;
use std::rc::Rc;
use std::cell::RefCell;
use std::ops::Deref;
use std::hash::{Hash, Hasher};
use std::fmt::{
    Display,
    Formatter,
    Result as fmtResult,
};

use lexer::OwnedToken;
use semantics::{
    Environment,
    Symbol,
};

pub enum AstNode {
    Identifier(Rc<Identifier>),
    Program(Rc<Program>),
    Class(Rc<Class>),
    Variable(Rc<Variable>),
    Function(Rc<Function>),
    Type(Rc<Type>),
    Argument(Rc<Argument>),
    Statement(Rc<Statement>),
    Expression(Rc<Expression>),
}

#[derive(Debug, Hash, Clone)]
pub struct Program {
    pub main: Rc<Class>,
    pub classes: Vec<Rc<Class>>,
}

impl Program {
    pub fn new<M: Into<Class>, C: Into<Class>>(main: M, classes: Vec<C>) -> Program {
        Program {
            main: Rc::new(main.into()),
            classes: classes.into_iter().map(|c| Rc::new(c.into())).collect(),
        }
    }
}

impl From<Rc<Program>> for AstNode {
    fn from(program: Rc<Program>) -> Self { AstNode::Program(program.clone()) }
}

#[derive(Debug, Clone, Eq, Ord, PartialOrd)]
pub struct Identifier {
    token: Token,
    symbol: RefCell<Option<Rc<Symbol>>>,
}

impl Identifier {
    pub fn set_symbol(&self, symbol: &Rc<Symbol>) {
        self.symbol.replace(Some(symbol.clone()));
    }

    pub fn get_symbol(&self) -> Option<Rc<Symbol>> {
        self.symbol.borrow().as_ref().map(|rc| rc.clone())
    }
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Identifier) -> bool {
        self.token.text == other.token.text
    }
}

impl Hash for Identifier {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.token.text.hash(state);
    }
}

impl Deref for Identifier {
    type Target = Token;
    fn deref(&self) -> &Self::Target { &self.token }
}

impl From<Token> for Identifier {
    fn from(token: Token) -> Self {
        Identifier {
            token,
            symbol: RefCell::new(None),
        }
    }
}

impl<T: AsRef<Identifier>> From<T> for OwnedToken {
    fn from(id: T) -> Self {
        (&id.as_ref().token).into()
    }
}

#[derive(Debug, Clone)]
pub struct Class {
    pub id: Rc<Identifier>,
    pub extends: Option<Rc<Identifier>>,
    pub variables: Vec<Rc<Variable>>,
    pub functions: Vec<Rc<Function>>,
    scope: RefCell<Option<Rc<Environment>>>,
}

impl Class {
    pub fn new<I, E, V, F>(id: I, extends: Option<E>, variables: Vec<V>, functions: Vec<F>) -> Class
        where I: Into<Identifier>,
              E: Into<Identifier>,
              V: Into<Variable>,
              F: Into<Function>,
    {
        Class {
            id: Rc::new(id.into()),
            extends: extends.map(|e| Rc::new(e.into())),
            variables: variables.into_iter().map(|v| Rc::new(v.into())).collect(),
            functions: functions.into_iter().map(|f| Rc::new(f.into())).collect(),
            scope: RefCell::new(None),
        }
    }

    pub fn set_env(&self, scope: &Rc<Environment>) {
        self.scope.replace(Some(scope.clone()));
    }

    pub fn get_env(&self) -> Option<Rc<Environment>> {
        self.scope.borrow().as_ref().map(|rc| rc.clone())
    }
}

impl Hash for Class {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.extends.hash(state);
        self.variables.hash(state);
        self.functions.hash(state);
    }
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Variable {
    pub kind: Rc<Type>,
    pub name: Rc<Identifier>,
}

impl Variable {
    pub fn new<T, I>(kind: T, id: I) -> Variable
        where T: Into<Type>,
              I: Into<Identifier>,
    {
        Variable {
            kind: Rc::new(kind.into()),
            name: Rc::new(id.into()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub kind: Rc<Type>,
    pub name: Rc<Identifier>,
    pub args: Vec<Rc<Argument>>,
    pub variables: Vec<Rc<Variable>>,
    pub statements: Vec<Rc<Statement>>,
    pub expression: Option<Rc<Expression>>,
    scope: RefCell<Option<Rc<Environment>>>,
}

impl Function {
    pub fn new<T, I, A, V, S, E>(
        kind: T, name: I, args: Vec<A>, vars: Vec<V>, stmts: Vec<S>, expr: Option<E>,
    ) -> Function
        where T: Into<Type>,
              I: Into<Identifier>,
              A: Into<Argument>,
              V: Into<Variable>,
              S: Into<Statement>,
              E: Into<Expression>,
    {
        Function {
            kind: Rc::new(kind.into()),
            name: Rc::new(name.into()),
            args: args.into_iter().map(|a| Rc::new(a.into())).collect(),
            variables: vars.into_iter().map(|v| Rc::new(v.into())).collect(),
            statements: stmts.into_iter().map(|s| Rc::new(s.into())).collect(),
            expression: expr.map(|e| Rc::new(e.into())),
            scope: RefCell::new(None),
        }
    }

    pub fn get_symbol(&self) -> Option<Rc<Symbol>> {
        self.name.get_symbol()
    }

    pub fn set_env(&self, scope: &Rc<Environment>) {
        self.scope.replace(Some(scope.clone()));
    }

    pub fn get_env(&self) -> Option<Rc<Environment>> {
        self.scope.borrow().as_ref().map(|rc| rc.clone())
    }
}

impl Hash for Function {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
        self.name.hash(state);
        self.args.hash(state);
        self.variables.hash(state);
        self.statements.hash(state);
        self.expression.hash(state);
    }
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum Type {
    Id(Rc<Identifier>),
    Void,
    Boolean,
    String,
    StringArray,
    Int,
    IntArray,
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Argument {
    pub kind: Rc<Type>,
    pub name: Rc<Identifier>,
}

impl Argument {
    pub fn new<T, I>(kind: T, name: I) -> Argument
        where T: Into<Type>,
              I: Into<Identifier>,
    {
        Argument {
            kind: Rc::new(kind.into()),
            name: Rc::new(name.into()),
        }
    }
}

#[derive(Debug, Hash, Clone)]
pub enum Stmt {
    Block {
        statements: Vec<Rc<Statement>>,
    },
    While {
        expression: Rc<Expression>,
        statement: Rc<Statement>,
    },
    Print {
        expression: Rc<Expression>,
    },
    Assign {
        lhs: Rc<Identifier>,
        rhs: Rc<Expression>,
    },
    AssignArray {
        lhs: Rc<Identifier>,
        in_bracket: Rc<Expression>,
        rhs: Rc<Expression>,
    },
    SideEffect {
        expression: Rc<Expression>,
    },
    If {
        condition: Rc<Expression>,
        statement: Rc<Statement>,
        otherwise: Option<Rc<Statement>>,
    },
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub stmt: Stmt,
    scope: RefCell<Option<Rc<Environment>>>,
}

impl Hash for Statement {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.stmt.hash(state);
    }
}

impl From<Stmt> for Statement {
    fn from(stmt: Stmt) -> Self {
        Statement { stmt, scope: RefCell::new(None), }
    }
}

impl Deref for Statement {
    type Target = Stmt;
    fn deref(&self) -> &<Self as Deref>::Target { &self.stmt }
}

impl Statement {
    pub fn set_env(&self, scope: &Rc<Environment>) {
        self.scope.replace(Some(scope.clone()));
    }

    pub fn get_env(&self) -> Option<Rc<Environment>> {
        self.scope.borrow().as_ref().map(|rc| rc.clone())
    }
}

impl Statement {
    pub fn new_block<S: Into<Statement>>(statements: Vec<S>) -> Statement {
        Stmt::Block {
            statements: statements.into_iter().map(|s| Rc::new(s.into())).collect(),
        }.into()
    }

    pub fn new_while<E, S>(expression: E, statement: S) -> Statement
        where E: Into<Expression>,
              S: Into<Statement>,
    {
        Stmt::While {
            expression: Rc::new(expression.into()),
            statement: Rc::new(statement.into()),
        }.into()
    }

    pub fn new_print<E: Into<Expression>>(expr: E) -> Statement {
        Stmt::Print { expression: Rc::new(expr.into()) }.into()
    }

    pub fn new_assign<I, E>(id: I, expr: E) -> Statement
        where I: Into<Identifier>,
              E: Into<Expression>,
    {
        Stmt::Assign {
            lhs: Rc::new(id.into()),
            rhs: Rc::new(expr.into()),
        }.into()
    }

    pub fn new_assign_array<I, E1, E2>(lhs: I, bracket: E1, rhs: E2) -> Statement
        where I: Into<Identifier>,
              E1: Into<Expression>,
              E2: Into<Expression>,
    {
        Stmt::AssignArray {
            lhs: Rc::new(lhs.into()),
            in_bracket: Rc::new(bracket.into()),
            rhs: Rc::new(rhs.into()),
        }.into()
    }

    pub fn new_sidef<E: Into<Expression>>(expr: E) -> Statement {
        Stmt::SideEffect { expression: Rc::new(expr.into()) }.into()
    }

    pub fn new_if<E, S1, S2>(cond: E, body: S1, otherwise: Option<S2>) -> Statement
        where E: Into<Expression>,
              S1: Into<Statement>,
              S2: Into<Statement>,
    {
        Stmt::If {
            condition: Rc::new(cond.into()),
            statement: Rc::new(body.into()),
            otherwise: otherwise.map(|s| Rc::new(s.into())),
        }.into()
    }
}

#[derive(Debug)]
pub struct Expression {
    expr: Expr,
    scope: RefCell<Option<Rc<Environment>>>,
}

impl Expression {
    pub fn set_env(&self, scope: &Rc<Environment>) {
        self.scope.replace(Some(scope.clone()));
    }

    pub fn get_env(&self) -> Option<Rc<Environment>> {
        self.scope.borrow().as_ref().map(|rc| rc.clone())
    }
}

impl Hash for Expression {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.expr.hash(state)
    }
}

impl From<Expr> for Expression {
    fn from(expr: Expr) -> Self {
        Expression { expr, scope: RefCell::new(None) }
    }
}

impl Deref for Expression {
    type Target = Expr;
    fn deref(&self) -> &<Self as Deref>::Target {
        &self.expr
    }
}

#[derive(Debug, Hash, Clone)]
pub enum Expr {
    TrueLiteral,
    FalseLiteral,
    This,
    NewClass(Rc<Identifier>),
    Identifier(Rc<Identifier>),
    IntLiteral(Rc<Token>),
    StringLiteral(Rc<Token>),
    Unary(UnaryExpression),
    Binary(BinaryExpression),
}

impl From<UnaryExpression> for Expression {
    fn from(unary: UnaryExpression) -> Self {
        Expr::Unary(unary).into()
    }
}

impl From<BinaryExpression> for Expression {
    fn from(binary: BinaryExpression) -> Self {
        Expr::Binary(binary).into()
    }
}

impl Expression {
    pub fn associate_left(self) -> Expression {
        if let Expr::Binary(binary) = self.expr {
            binary.associate_left().into()
        } else { self }
    }
}

#[derive(Debug, Hash, Clone)]
pub enum UnaryExpression {
    NewArray(Rc<Expression>),
    Not(Rc<Expression>),
    Parentheses(Rc<Expression>),
    Length(Rc<Expression>),
    Application {
        expression: Rc<Expression>,
        id: Rc<Identifier>,
        list: Vec<Rc<Expression>>,
    },
}

#[derive(Debug, Hash, Clone)]
pub struct BinaryExpression {
    pub kind: BinaryKind,
    pub lhs: Rc<Expression>,
    pub rhs: Rc<Expression>,
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

impl Display for BinaryKind {
    fn fmt(&self, f: &mut Formatter) -> fmtResult {
        match *self {
            BinaryKind::And => write!(f, "&&"),
            BinaryKind::Or => write!(f, "||"),
            BinaryKind::Equals => write!(f, "=="),
            BinaryKind::LessThan => write!(f, "<"),
            BinaryKind::Plus => write!(f, "+"),
            BinaryKind::Minus => write!(f, "-"),
            BinaryKind::Times => write!(f, "*"),
            BinaryKind::Divide => write!(f, "/"),
            BinaryKind::ArrayLookup => write!(f, "[]"),
        }
    }
}

impl BinaryExpression {
    /// Takes a BinaryExpression and left-associates all of the operators of the same kind.
    pub fn associate_left(self) -> BinaryExpression {
        let kind = self.kind;
        let mut parent = self;

        loop {
            // If the right hand side is a binary operator of the same kind, rotate left.
            let BinaryExpression { lhs: parent_lhs, rhs: parent_rhs, .. } = parent;
            if let Expr::Binary(ref binary) = **parent_rhs {
                if binary.kind == kind {
                    let &BinaryExpression { lhs: ref right_lhs, rhs: ref right_rhs, .. } = binary;
                    let child = BinaryExpression {
                        kind,
                        lhs: parent_lhs,
                        rhs: right_lhs.clone(),
                    };
                    parent = BinaryExpression { kind, lhs: Rc::new(child.into()), rhs: right_rhs.clone() }
                } else {
                    break BinaryExpression { kind, lhs: parent_lhs, rhs: Rc::new(binary.clone().into()) }
                }
            } else {
                break BinaryExpression { kind, lhs: parent_lhs, rhs: parent_rhs };
            }
        }
    }
}

impl Expression {
    pub fn new_class<I: Into<Identifier>>(id: I) -> Expression {
        Expr::NewClass(Rc::new(id.into())).into()
    }

    pub fn new_identifier<I: Into<Identifier>>(id: I) -> Expression {
        Expr::Identifier(Rc::new(id.into())).into()
    }

    pub fn new_intlit<T: Into<Token>>(token: T) -> Expression {
        Expr::IntLiteral(Rc::new(token.into())).into()
    }

    pub fn new_stringlit<T: Into<Token>>(token: T) -> Expression {
        Expr::StringLiteral(Rc::new(token.into())).into()
    }

    pub fn new_array<E: Into<Expression>>(expr: E) -> Expression {
        UnaryExpression::NewArray(Rc::new(expr.into())).into()
    }

    pub fn new_not<E: Into<Expression>>(expr: E) -> Expression {
        UnaryExpression::Not(Rc::new(expr.into())).into()
    }

    pub fn new_parentheses<E: Into<Expression>>(expr: E) -> Expression {
        UnaryExpression::Parentheses(Rc::new(expr.into())).into()
    }

    pub fn new_length<E: Into<Expression>>(expr: E) -> Expression {
        UnaryExpression::Length(Rc::new(expr.into())).into()
    }

    pub fn new_application<I, E1, E2>(lhs: E1, id: I, list: Vec<E2>) -> Expression
        where I: Into<Identifier>,
              E1: Into<Expression>,
              E2: Into<Expression>,
    {
        UnaryExpression::Application {
            expression: Rc::new(lhs.into()),
            id: Rc::new(id.into()),
            list: list.into_iter().map(|e| Rc::new(e.into())).collect(),
        }.into()
    }

    pub fn new_binary<E1, E2>(kind: BinaryKind, lhs: E1, rhs: E2) -> Expression
        where E1: Into<Expression>,
              E2: Into<Expression>,
    {
        BinaryExpression {
            kind,
            lhs: Rc::new(lhs.into()),
            rhs: Rc::new(rhs.into()),
        }.into()
    }
}
