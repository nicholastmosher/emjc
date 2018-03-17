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
    ConflictingDeclaration(OwnedToken),
    ExtendingUndeclared(OwnedToken),
    UnavailableName(OwnedToken, NameKind, NameKind),
}

impl Display for SemanticError {
    fn fmt(&self, f: &mut Formatter) -> fmtResult {
        match *self {
            SemanticError::ConflictingDeclaration(ref t) => {
                write!(f, "{}:{} error: conflicting declaration of '{}'", t.line, t.column, t.text)
            },
            SemanticError::ExtendingUndeclared(ref t) => {
                write!(f, "{}:{} error: extending undeclared class '{}'", t.line, t.column, t.text)
            },
            SemanticError::UnavailableName(ref t, ref new, ref old) => {
                write!(f, "{}:{} error: cannot declare {:?} '{}', it is already declared as a {:?}", t.line, t.column, new, t.text, old)
            },
        }
    }
}

impl SemanticError {
    fn conflicting_declaration<T: Into<OwnedToken>>(token: T) -> SemanticError {
        SemanticError::ConflictingDeclaration(token.into())
    }
    fn extending_undelcared<T: Into<OwnedToken>>(token: T) -> SemanticError {
        SemanticError::ExtendingUndeclared(token.into())
    }
    fn unavailable_name<T: Into<OwnedToken>>(token: T, declaring: NameKind, declared: NameKind) -> SemanticError {
        SemanticError::UnavailableName(token.into(), declaring, declared)
    }
}

#[derive(Debug, Hash, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum NameKind {
    Class,
    Variable,
    Method,
}

#[derive(Debug, Hash, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
struct Metadata {
    kind: NameKind,
}

impl Metadata {
    fn new_class() -> Self { Metadata { kind: NameKind::Variable } }
    fn new_variable() -> Self { Metadata { kind: NameKind::Variable } }
//    fn new_method() -> Self { Metadata { kind: NameKind::Method } }
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
        if let Some(_) = self.store.insert(String::from(&id.as_ref().text), meta) {
            Err(SemanticError::conflicting_declaration(&id))?;
        }
        Ok(())
    }

    /// If these bindings contains the given Identifier and that Identifier is the same
    /// kind as requested, return true. If the given Identifier does not exist in these
    /// bindings, return false. If the given Identifier exists in these bindings but is
    /// of a different kind, return an error.
    fn contains_kind<I: AsRef<Identifier>>(&self, id: I, kind: NameKind) -> Result<bool> {
        match self.get(&id) {
            None => Ok(false),
            Some(meta) => {
                if meta.kind == kind { Ok(true) }
                else { Err(SemanticError::unavailable_name(&id, kind, meta.kind))? }
            },
        }
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
