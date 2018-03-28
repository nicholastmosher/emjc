#![allow(unused_must_use)]

use std::rc::{Rc, Weak};
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

pub mod name_analysis;
pub mod type_analysis;
pub mod pretty_printer;

use syntax::ast::*;
use semantics::name_analysis::Name;
use semantics::type_analysis::SymbolType;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Symbol {
    pub name: Name,
    pub kind: RefCell<Option<SymbolType>>,
}

impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
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

    fn _set_type<T: Into<SymbolType>>(&self, kind: T) {
        self.kind.replace(Some(kind.into()));
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmtError> {
        self.name.fmt(f)?;
        if let Some(ref kind) = *self.kind.borrow() { kind.fmt(f); }
        Ok(())
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
    global_scope: Weak<GlobalScope>,
    extending: RefCell<Option<Rc<ClassScope>>>,
    variables: RefCell<HashMap<Rc<Identifier>, Rc<Symbol>>>,
    functions: RefCell<HashMap<Rc<Identifier>, Rc<FunctionScope>>>,
}

impl ClassScope {
    fn new(symbol: &Rc<Symbol>, global_scope: &Rc<GlobalScope>) -> Rc<ClassScope> {
        Rc::new(ClassScope {
            symbol: symbol.clone(),
            global_scope: Rc::downgrade(global_scope),
            extending: RefCell::new(None),
            variables: RefCell::new(HashMap::new()),
            functions: RefCell::new(HashMap::new()),
        })
    }

    /// Checks whether this class belongs to an inheritance cycle.
    fn cycle(&self) -> bool {
        let mut super_class = self.extending.borrow().as_ref().map(|rc| rc.clone());
        loop {
            if super_class.is_none() { return false; }
            if super_class.as_ref().unwrap().symbol == self.symbol { return true; }
            let upper = super_class.as_ref().unwrap().extending.borrow().as_ref().map(|rc| rc.clone());
            super_class = upper;
        }
    }

    fn find_func(&self, id: &Rc<Identifier>) -> Option<Rc<FunctionScope>> {
        self.functions.borrow().get(id).map(|rc| rc.clone())
    }
}

impl Scope for ClassScope {
    fn find(&self, id: &Rc<Identifier>) -> Option<Rc<Symbol>> {
        // Check whether a member variable of the class matches.
        if let Some(symbol) = self.variables.borrow().get(id) {
            return Some(symbol.clone());
        }

        // Check whether a function of the class matches.
        if let Some(func_scope) = self.functions.borrow().get(id) {
            return func_scope.function.name.get_symbol();
        }

        // If the class had a superclass, search it.
        if let Some(ref super_class) = *self.extending.borrow() {
            return super_class.find(id);
        }

        // If all else fails, search the global scope.
        return self.global_scope.upgrade().unwrap().find(id);
    }
}

#[derive(Debug)]
struct FunctionScope {
    function: Rc<Function>,
    class: Weak<ClassScope>,
    overriding: Option<Rc<FunctionScope>>,
    variables: HashMap<Rc<Identifier>, Rc<Symbol>>,
}

impl FunctionScope {
    fn new(function: &Rc<Function>, class: Rc<ClassScope>) -> FunctionScope {
        FunctionScope {
            function: function.clone(),
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
