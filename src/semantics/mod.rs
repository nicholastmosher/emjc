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
    VariableOverride(OwnedToken),
}

impl Display for SemanticError {
    fn fmt(&self, f: &mut Formatter) -> fmtResult {
        match *self {
            SemanticError::ExtendingUndeclared(ref t) => {
                write!(f, "{}:{} name error: extending undeclared class '{}'", t.line, t.column, t.text)
            },
            SemanticError::VariableOverride(ref t) => {
                write!(f, "{}:{} name error: variable '{}' overrides variable in superclass", t.line, t.column, t.text)
            }
        }
    }
}

impl SemanticError {
    fn extending_undelcared<T: Into<OwnedToken>>(token: T) -> SemanticError {
        SemanticError::ExtendingUndeclared(token.into())
    }
    fn variable_override<T: Into<OwnedToken>>(token: T) -> SemanticError {
        SemanticError::VariableOverride(token.into())
    }
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
struct Symbol {
    id: Rc<Identifier>,
    /// A unique identifier for this item. This allows for multiple items to be given
    /// the same name but still be unique. This is used in variable shadowing, where
    /// two different variables may have the same name but refer to different memory.
    uid: usize,
}

#[derive(Debug)]
struct GlobalScope {
    classes: HashMap<Rc<Identifier>, Rc<ClassScope>>,
}

impl GlobalScope {
    fn new() -> GlobalScope {
        GlobalScope { classes: HashMap::new() }
    }
}

#[derive(Debug)]
struct ClassScope {
    symbol: Symbol,
    extending: Option<Rc<ClassScope>>,
    variables: RefCell<HashMap<Rc<Identifier>, Symbol>>,
    functions: RefCell<HashMap<Rc<Identifier>, Rc<FunctionScope>>>,
}

impl ClassScope {
    fn new(symbol: Symbol) -> Rc<ClassScope> {
        Rc::new(ClassScope {
            symbol,
            extending: None,
            variables: RefCell::new(HashMap::new()),
            functions: RefCell::new(HashMap::new()),
        })
    }

    fn extending(symbol: Symbol, extending: &Rc<ClassScope>) -> Rc<ClassScope> {
        Rc::new(ClassScope {
            symbol,
            extending: Some(extending.clone()),
            variables: RefCell::new(HashMap::new()),
            functions: RefCell::new(HashMap::new()),
        })
    }
}

#[derive(Debug)]
struct FunctionScope {
    symbol: Symbol,
    class: Weak<ClassScope>,
    overriding: Option<Rc<FunctionScope>>,
    variables: HashMap<Rc<Identifier>, Symbol>,
}

impl FunctionScope {
    fn new(symbol: Symbol, class: Rc<ClassScope>) -> FunctionScope {
        FunctionScope {
            symbol,
            class: Rc::downgrade(&class),
            overriding: None,
            variables: HashMap::new(),
        }
    }
}
