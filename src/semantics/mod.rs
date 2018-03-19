#![allow(warnings)]

use Result;
use std::rc::{Rc, Weak};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{
    Formatter,
    Display,
    Result as fmtResult,
};
use lexer::OwnedToken;
use syntax::ast::*;

pub mod name_analyzer;

#[derive(Debug, Fail)]
pub enum SemanticError {
    ExtendingUndeclared(OwnedToken),
    UnavailableName(OwnedToken, String, String),
}

impl Display for SemanticError {
    fn fmt(&self, f: &mut Formatter) -> fmtResult {
        match *self {
            SemanticError::ExtendingUndeclared(ref t) => {
                write!(f, "{}:{} error: extending undeclared class '{}'", t.line, t.column, t.text)
            },
            SemanticError::UnavailableName(ref t, ref new, ref old) => {
                write!(f, "{}:{} error: cannot declare {} '{}', it is already declared as a {}", t.line, t.column, new, t.text, old)
            },
        }
    }
}

impl SemanticError {
    fn extending_undelcared<T: Into<OwnedToken>>(token: T) -> SemanticError {
        SemanticError::ExtendingUndeclared(token.into())
    }
    fn unavailable_name<T: Into<OwnedToken>>(token: T, declaring: &NameKind, declared: &NameKind) -> SemanticError {
        SemanticError::UnavailableName(token.into(), format!("{:?}", declaring), format!("{:?}", declared))
    }
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum NameKind {
    /// A name which describes a class.
    Class,
    /// A name which describes a variable.
    Variable,
    /// A name which describes a function.
    Function {
        /// Holds a reference to the Function AST object that describes this function.
        ast: Rc<Function>,
    },
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
struct Metadata {
    /// A unique identifier for this item. This allows for multiple items to be given
    /// the same name but still be unique. This is used in variable shadowing, where
    /// two different variables may have the same name but refer to different memory.
    uid: usize,
    kind: NameKind,
}

impl Metadata {
    fn new_class(uid: usize) -> Self { Metadata { uid, kind: NameKind::Variable } }
    fn new_variable(uid: usize) -> Self { Metadata { uid, kind: NameKind::Variable } }
    fn new_method(uid: usize, ast: Rc<Function>) -> Self { Metadata { uid, kind: NameKind::Function{ ast } } }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Bindings {
    store: HashMap<String, Metadata>,
}

impl Bindings {
    fn new() -> Self {
        Bindings { store: HashMap::new() }
    }

    /// Inserts an identifier declaration into this set of bindings.
    /// If this identifier has already been declared in this scope, that's a
    /// semantic error.
    fn declare<I: AsRef<Identifier>>(&mut self, id: I, meta: Metadata) -> Result<()> {
        let new_kind = meta.kind.clone();
        if let Some(ref old) = self.store.insert(String::from(&id.as_ref().text), meta) {
            Err(SemanticError::unavailable_name(&id, &new_kind, &old.kind))?;
        }
        Ok(())
    }

    fn get<I: AsRef<Identifier>>(&self, id: &I) -> Option<&Metadata> {
        let name = String::from(&id.as_ref().text);
        self.store.get(&name)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum AstNode {
    Program(Rc<Program>),
    Main(Rc<Main>),
    Identifier(Rc<Identifier>),
    Class(Rc<Class>),
    Extends(Rc<Extends>),
    Variable(Rc<Variable>),
    Function(Rc<Function>),
    Type(Rc<Type>),
    Argument(Rc<Argument>),
    Statement(Rc<Statement>),
    Expression(Rc<Expression>),
}

impl From<Rc<Program>>    for AstNode { fn from(program: Rc<Program>)       -> Self { AstNode::Program(program.clone()) } }
impl From<Rc<Main>>       for AstNode { fn from(main: Rc<Main>)             -> Self { AstNode::Main(main.clone()) } }
impl From<Rc<Identifier>> for AstNode { fn from(id: Rc<Identifier>)         -> Self { AstNode::Identifier(id.clone()) } }
impl From<Rc<Class>>      for AstNode { fn from(class: Rc<Class>)           -> Self { AstNode::Class(class.clone()) } }
impl From<Rc<Extends>>    for AstNode { fn from(extends: Rc<Extends>)       -> Self { AstNode::Extends(extends.clone()) } }
impl From<Rc<Variable>>   for AstNode { fn from(variable: Rc<Variable>)     -> Self { AstNode::Variable(variable.clone()) } }
impl From<Rc<Function>>   for AstNode { fn from(function: Rc<Function>)     -> Self { AstNode::Function(function.clone()) } }
impl From<Rc<Type>>       for AstNode { fn from(kind: Rc<Type>)             -> Self { AstNode::Type(kind.clone()) } }
impl From<Rc<Argument>>   for AstNode { fn from(arg: Rc<Argument>)          -> Self { AstNode::Argument(arg.clone()) } }
impl From<Rc<Statement>>  for AstNode { fn from(statement: Rc<Statement>)   -> Self { AstNode::Statement(statement.clone()) } }
impl From<Rc<Expression>> for AstNode { fn from(expression: Rc<Expression>) -> Self { AstNode::Expression(expression.clone()) } }

/// An AstWrapper is a data structure for building a tree "parallel" to the
/// abstract syntax tree in order to associate new metadata into the program.
/// This is used, for example, for keeping track of the Environment of declared
/// names and types at various points in the program.
pub struct AstWrapper<T> {
    parent: Option<Weak<RefCell<AstWrapper<T>>>>,
    children: Vec<Rc<RefCell<AstWrapper<T>>>>,
    ast_node: AstNode,
    data: T,
}

/// An Environment is a tree parallel to the AST at which each node has a set
/// of Bindings associated with it. The Bindings store information regarding
/// declarations of identifiers as well as their usages.
type Environment = AstWrapper<Bindings>;

/// The Env type is a shorthand for Environment which is useful for navigating
/// the tree structure, which makes heavy use of Rc<RefCell<_>>.
type Env = Rc<RefCell<Environment>>;

impl Environment {
    /// Creates a new, bare environment "wrapping" an AST node.
    fn new<T: Into<AstNode>>(ast_node: T) -> Env {
        Self::with_parent(ast_node, None)
    }

    /// Creates a new environment "wrapping" an AST node, optionally extending
    /// a given parent environment.
    fn with_parent<T: Into<AstNode>>(ast_node: T, parent: Option<Env>) -> Env {
        Rc::new(RefCell::new(
            AstWrapper {
                parent: parent.map(|ref e| Rc::downgrade(e)),
                children: vec![],
                ast_node: ast_node.into(),
                data: Bindings::new(),
            }
        ))
    }

    fn get<I: AsRef<Identifier>>(mut env: Env, id: I) -> Option<Metadata> {
        loop {
            if let Some(meta) = env.borrow().data.get(&id) {
                return Some(meta.clone());
            }

            let super_env = env.borrow().parent.as_ref().and_then(|s| s.upgrade());
            if super_env.is_some() {
                env = super_env.unwrap();
                continue;
            }
            return None;
        }
    }
}
