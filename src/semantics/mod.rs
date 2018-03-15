#![allow(warnings)]

use Result;
use std::rc::{Rc, Weak};
use std::cell::RefCell;
use std::collections::HashMap;
use syntax::ast::*;

pub mod name_analyzer;

#[derive(Debug, Fail)]
pub enum SemanticError {
    #[fail(display = "name conflict: duplicate name in same scope")]
    ConflictingDeclaration,
    #[fail(display = "name conflict: extending undeclared class")]
    ExtendingUndeclared,
}

#[derive(Debug, Hash, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
struct Metadata { }

impl Metadata {
    fn new() -> Self {
        Metadata { }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Bindings {
    store: HashMap<Identifier, Metadata>,
}

impl Bindings {
    fn new() -> Self {
        Bindings { store: HashMap::new() }
    }

    /// Inserts an identifier declaration into this set of bindings.
    /// If this identifier has already been declared in this scope, that's a
    /// semantic error.
    fn declare(&mut self, id: &Identifier) -> Result<()> {
        if let Some(_) = self.store.insert(id.clone(), Metadata::new()) {
            Err(SemanticError::ConflictingDeclaration)?;
        }
        Ok(())
    }

    fn contains(&self, id: &Identifier) -> bool {
        self.store.contains_key(id)
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
