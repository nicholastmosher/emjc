#![allow(warnings)]

use Result;
use std::rc::{Rc, Weak};
use std::collections::HashMap;
use syntax::visitor::Visitor;
use syntax::ast::*;

#[derive(Debug, Fail)]
pub enum SemanticError {
    #[fail(display = "name conflict: duplicate name in same scope")]
    ConflictingDeclaration,
}

/// Represents a unique named item in a program. A name is described by the
/// identifier from the input stream which names the item, supplemented by a
/// numeric suffix which is used to distinguish separate language items with
/// the same identifier in a program. An example of this would be that while
/// a method variable and a local variable may share the same identifier, they
/// will have different `UniqueName`s since they are semantically separate
/// items.
///
/// Each `UniqueName` is stringified as `identifier_num_` where `identifier`
/// is the string captured by the ID token and `num` is the number that
/// distinguishes which semantic item is being referenced.  Consider the
/// following emj snippet:
///
/// ```java
/// class Foo {
///     int bar;
///     public int foo() {
///         int bar;
///     }
/// }
/// ```
///
/// This snippet has two distinct `bar` variables, represented as
/// `bar_0_` and `bar_1_`, respectively.
#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
struct UniqueName {
    id: Identifier,
    mangle: usize,
}

#[derive(Debug, Hash, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
enum NameKind {
    Class,
    Variable,
    Method,
}

#[derive(Debug, Hash, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
struct Metadata {
    kind: NameKind,
}

impl Metadata {
    fn new(kind: NameKind) -> Self {
        Metadata { kind }
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
    fn declare(&mut self, id: Identifier, kind: NameKind) -> Result<()> {
        if let Some(_) = self.store.insert(id, Metadata::new(kind)) {
            Err(SemanticError::ConflictingDeclaration)?;
        }
        Ok(())
    }
}

struct Environment {
    extends: Option<Weak<Environment>>,
    extended: Vec<Rc<Environment>>,
    bindings: Bindings,
}

impl Environment {
    fn new() -> Self {
        Environment {
            extends: None,
            extended: Vec::new(),
            bindings: Bindings::new(),
        }
    }

    fn with_super(extending: &Rc<Environment>) -> Self {
        Environment {
            extends: Some(Rc::downgrade(extending)),
            extended: Vec::new(),
            bindings: Bindings::new(),
        }
    }
}

pub struct NameAnalyzer;

impl Visitor<Program, Rc<Environment>> for NameAnalyzer {
    fn visit(&mut self, program: &Program) -> Rc<Environment> {
        let mut top_environment = Rc::new(Environment::new());

        // Do name analysis on the Main class
        let main_env = self.visit(&(&program.main, &top_environment));

        // Do name analysis on all other classes
        for class in program.classes.iter() {
            let class_env = self.visit(&(class, &top_environment));
        }

        top_environment
    }
}

impl<'a> Visitor<(&'a Main, &'a Rc<Environment>), Rc<Environment>> for NameAnalyzer {
    fn visit(&mut self, &(main, super_env): &(&'a Main, &'a Rc<Environment>)) -> Rc<Environment> {
        let main_env = Rc::new(Environment::with_super(&super_env));



        main_env
    }
}

impl<'a> Visitor<(&'a Class, &'a Rc<Environment>), Rc<Environment>> for NameAnalyzer {
    fn visit(&mut self, &(class, super_env): &(&'a Class, &'a Rc<Environment>)) -> Rc<Environment> {
        unimplemented!()
    }
}
