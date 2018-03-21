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

pub mod pretty_printer;

#[derive(Debug, Fail)]
pub enum SemanticError {
    ExtendingUndeclared(OwnedToken),
    VariableOverride(OwnedToken),
    UsingUndeclared(OwnedToken),
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
            SemanticError::UsingUndeclared(ref t) => {
                write!(f, "{}:{} name error: use of undeclared identifier '{}'", t.line, t.column, t.text)
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
    fn using_undeclared<T: Into<OwnedToken>>(token: T) -> SemanticError {
        SemanticError::UsingUndeclared(token.into())
    }
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Symbol {
    id: String,
    /// A unique identifier for this item. This allows for multiple items to be given
    /// the same name but still be unique. This is used in variable shadowing, where
    /// two different variables may have the same name but refer to different memory.
    uid: Option<usize>,
}

impl Symbol {
    fn unresolved(id: &Rc<Identifier>) -> Rc<Symbol> {
        Rc::new(Symbol { id: String::from(&id.text), uid: None })
    }

    fn stringify(&self) -> String {
        let mut string = self.id.clone();
        match self.uid {
            None => string.push_str("_#error#_"),
            Some(uid) => string.push_str(&format!("_{}_", uid)),
        }
        string
    }
}

pub trait Scope {
    fn find(&self, id: &Rc<Identifier>) -> Option<Rc<Symbol>>;
}

#[derive(Debug)]
struct GlobalScope {
    classes: RefCell<HashMap<Rc<Identifier>, Rc<ClassScope>>>,
}

impl GlobalScope {
    fn new() -> Rc<GlobalScope> {
        Rc::new(GlobalScope { classes: RefCell::new(HashMap::new()) })
    }
}

impl Scope for GlobalScope {
    fn find(&self, id: &Rc<Identifier>) -> Option<Rc<Symbol>> {
        if let Some(class) = self.classes.borrow().get(id) {
            return Some(class.symbol.clone());
        }
        return None;
    }
}

#[derive(Debug)]
struct ClassScope {
    symbol: Rc<Symbol>,
    extending: Option<Rc<ClassScope>>,
    variables: RefCell<HashMap<Rc<Identifier>, Rc<Symbol>>>,
    functions: RefCell<HashMap<Rc<Identifier>, Rc<FunctionScope>>>,
}

impl ClassScope {
    fn new(symbol: &Rc<Symbol>) -> Rc<ClassScope> {
        Rc::new(ClassScope {
            symbol: symbol.clone(),
            extending: None,
            variables: RefCell::new(HashMap::new()),
            functions: RefCell::new(HashMap::new()),
        })
    }

    fn extending(symbol: &Rc<Symbol>, extending: &Rc<ClassScope>) -> Rc<ClassScope> {
        Rc::new(ClassScope {
            symbol: symbol.clone(),
            extending: Some(extending.clone()),
            variables: RefCell::new(HashMap::new()),
            functions: RefCell::new(HashMap::new()),
        })
    }
}

impl Scope for ClassScope {
    fn find(&self, id: &Rc<Identifier>) -> Option<Rc<Symbol>> {
        // Check whether a member variable of the class matches.
        if let Some(symbol) = self.variables.borrow().get(id) {
            return Some(symbol.clone());
        }

        // Check whether a function of the class matches.
        if let Some(func) = self.functions.borrow().get(id) {
            return Some(func.symbol.clone());
        }

        // If the class had a superclass, search it.
        if let Some(ref super_class) = self.extending {
            return super_class.find(id);
        }

        // If we hit a class with no superclass and still haven't found anything, return None.
        return None;
    }
}

#[derive(Debug)]
struct FunctionScope {
    symbol: Rc<Symbol>,
    class: Weak<ClassScope>,
    overriding: Option<Rc<FunctionScope>>,
    variables: HashMap<Rc<Identifier>, Rc<Symbol>>,
}

impl FunctionScope {
    fn new(symbol: &Rc<Symbol>, class: Rc<ClassScope>) -> FunctionScope {
        FunctionScope {
            symbol: symbol.clone(),
            class: Rc::downgrade(&class),
            overriding: None,
            variables: HashMap::new(),
        }
    }
}

impl Scope for FunctionScope {
    /// If there is a Symbol that represents the given identifier anywhere in this environment,
    /// return it. Otherwise, return None to indicate no such Symbol exists.
    fn find(&self, id: &Rc<Identifier>) -> Option<Rc<Symbol>> {
        // If a variable in this Function matches, return that variable's symbol.
        if let Some(symbol) = self.variables.get(id) {
            return Some(symbol.clone());
        }

        // If an identifier somewhere in the class environment matches, return that symbol.
        self.class.upgrade().unwrap().find(id)
    }
}
