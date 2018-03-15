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

impl<'a> From<&'a Program>    for AstNode { fn from(program: &'a Program)       -> Self { AstNode::Program(Rc::new(program.clone())) } }
impl<'a> From<&'a Main>       for AstNode { fn from(main: &'a Main)             -> Self { AstNode::Main(Rc::new(main.clone())) } }
impl<'a> From<&'a Identifier> for AstNode { fn from(id: &'a Identifier)         -> Self { AstNode::Identifier(Rc::new(id.clone())) } }
impl<'a> From<&'a Class>      for AstNode { fn from(class: &'a Class)           -> Self { AstNode::Class(Rc::new(class.clone())) } }
impl<'a> From<&'a Extends>    for AstNode { fn from(extends: &'a Extends)       -> Self { AstNode::Extends(Rc::new(extends.clone())) } }
impl<'a> From<&'a Variable>   for AstNode { fn from(variable: &'a Variable)     -> Self { AstNode::Variable(Rc::new(variable.clone())) } }
impl<'a> From<&'a Function>   for AstNode { fn from(function: &'a Function)     -> Self { AstNode::Function(Rc::new(function.clone())) } }
impl<'a> From<&'a Type>       for AstNode { fn from(kind: &'a Type)             -> Self { AstNode::Type(Rc::new(kind.clone())) } }
impl<'a> From<&'a Argument>   for AstNode { fn from(arg: &'a Argument)          -> Self { AstNode::Argument(Rc::new(arg.clone())) } }
impl<'a> From<&'a Statement>  for AstNode { fn from(statement: &'a Statement)   -> Self { AstNode::Statement(Rc::new(statement.clone())) } }
impl<'a> From<&'a Expression> for AstNode { fn from(expression: &'a Expression) -> Self { AstNode::Expression(Rc::new(expression.clone())) } }

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
