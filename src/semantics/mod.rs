#![allow(warnings)]

use Result;
use std::rc::{Rc, Weak};
use std::cell::RefCell;
use std::collections::HashMap;
use syntax::visitor::Visitor;
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

struct Environment {
    super_env: Option<Weak<RefCell<Environment>>>,
    sub_envs: Vec<Rc<RefCell<Environment>>>,
    bindings: Bindings,
}

type Env = Rc<RefCell<Environment>>;

impl Environment {
    fn new() -> Env {
        Rc::new(RefCell::new(
            Environment {
                super_env: None,
                sub_envs: Vec::new(),
                bindings: Bindings::new(),
            }
        ))
    }

    fn with_super(extending: &Env) -> Env {
        Rc::new(RefCell::new(
            Environment {
                super_env: Some(Rc::downgrade(extending)),
                sub_envs: Vec::new(),
                bindings: Bindings::new(),
            }
        ))
    }

    /// Given an Identifier, tell whether an item by that name has been declared
    /// in this Environment or any super Environment.
    fn has_declared(mut env: Env, id: &Identifier) -> bool {
        loop {
            if env.borrow().bindings.contains(id) { return true }
            let super_env = env.borrow().super_env.as_ref().and_then(|s| s.upgrade());
            if super_env.is_some() {
                env = super_env.unwrap();
                continue;
            }
            return false;
        }
    }
}

pub enum ASTItem<'a> {
    Program(&'a Program),
    MainClass(&'a Main),
    Identifier(&'a Identifier),
    Class(&'a Class),
    Extends(&'a Extends),
    Variable(&'a Variable),
    Function(&'a Function),
    Type(&'a Type),
    Argument(&'a Argument),
    Statement(&'a Statement),
    Expression(&'a Expression),
}

pub struct ASTNode {

}
