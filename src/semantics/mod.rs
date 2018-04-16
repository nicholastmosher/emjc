#![allow(unused_must_use)]

use std::rc::Rc;
use std::fmt::{
    Display,
    Formatter,
    Error as fmtError,
};
use std::hash::{
    Hash,
    Hasher,
};
use std::cell::RefCell;
use std::collections::HashMap;
use uuid::Uuid;

pub mod name_analysis;
pub mod type_analysis;
pub mod pretty_printer;

use syntax::ast::*;
use semantics::{
    name_analysis::Name,
    type_analysis::SymbolType,
};

#[derive(Debug, Clone, Eq, Ord, PartialOrd)]
pub struct Symbol {
    pub name: Name,
    pub kind: RefCell<Option<SymbolType>>,
}

impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Symbol) -> bool {
        self.name == other.name
    }
}

impl Symbol {
    fn new(name: &str, uid: usize) -> Rc<Symbol> {
        Rc::new(Symbol {
            name: Name::new(name, uid),
            kind: RefCell::new(None),
        })
    }

    fn unresolved(id: &Rc<Identifier>) -> Rc<Symbol> {
        Rc::new(Symbol {
            name: Name::unresolved(id),
            kind: RefCell::new(None),
        })
    }

    fn set_type<T: Into<SymbolType>>(&self, kind: T) {
        self.kind.replace(Some(kind.into()));
    }

    pub fn get_type(&self) -> Option<SymbolType> {
        (*self.kind.borrow()).clone()
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmtError> {
        self.name.fmt(f)?;
        if let Some(ref kind) = *self.kind.borrow() { write!(f, ": {}", kind); }
        Ok(())
    }
}

#[derive(Debug, Eq)]
pub struct Environment {
    uuid: Uuid,
    extending: RefCell<Option<Rc<Environment>>>,
    bindings: RefCell<HashMap<Rc<Identifier>, Rc<Symbol>>>,
}

impl Environment {
    pub fn new() -> Rc<Environment> {
        Rc::new(Environment {
            uuid: Uuid::new_v4(),
            extending: RefCell::new(None),
            bindings: RefCell::new(HashMap::new()),
        })
    }

    pub fn extending(env: &Rc<Environment>) -> Rc<Environment> {
        Rc::new(Environment {
            uuid: Uuid::new_v4(),
            extending: RefCell::new(Some(env.clone())),
            bindings: RefCell::new(HashMap::new()),
        })
    }

    pub fn get(&self, id: &Rc<Identifier>) -> Option<Rc<Symbol>> {
        match self.bindings.borrow().get(id) {
            // If the identifier is in these bindings, return it.
            Some(symbol) => Some(symbol.clone()),
            // If the identifier is in an environment we extend, return it.
            None => self.get_super().as_ref().and_then(|env| env.get(id)),
        }
    }

    pub fn define(&self, id: &Rc<Identifier>, symbol: &Rc<Symbol>) {
        self.bindings.borrow_mut().insert(id.clone(), symbol.clone());
    }

    pub fn cycle(&self) -> bool {
        let mut super_env: Option<Rc<Environment>> = self.get_super().as_ref().map(|rc| rc.clone());
        loop {
            if super_env.is_none() { return false; }
            let env = super_env.unwrap();
            if *self == *env { return true; }
            let upper = env.get_super().as_ref().map(|rc| rc.clone());
            super_env = upper;
        }
    }

    pub fn set_super(&self, super_env: &Rc<Environment>) {
        self.extending.replace(Some(super_env.clone()));
    }

    pub fn get_super(&self) -> Option<Rc<Environment>> {
        self.extending.borrow().as_ref().map(|rc| rc.clone())
    }
}

impl PartialEq for Environment {
    fn eq(&self, other: &Environment) -> bool {
        self.uuid == other.uuid
    }
}
