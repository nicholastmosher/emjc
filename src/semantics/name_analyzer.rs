use Result;
use failure::Error;
use std::rc::Rc;
use std::cell::RefCell;

use super::{
    SemanticError,
    NameKind,
    Metadata,
    Bindings,
    AstNode,
    AstWrapper,
    Environment,
    Env,
};

use syntax::visitor::Visitor;
use syntax::ast::*;

pub struct NameAnalyzer {
    name_count: usize,
    errors: Vec<Error>,
}

impl NameAnalyzer {
    pub fn new() -> Self {
        NameAnalyzer { name_count: 0, errors: vec![] }
    }

    pub fn analyze(&mut self, program: &Program) {
        self.visit(Rc::new(program.clone()));
        for err in self.errors.iter() {
            println!("Name analysis error: {}", err);
        }
    }

    fn make_uid(&mut self) -> usize {
        let uid = self.name_count;
        self.name_count += 1;
        uid
    }
}

impl Visitor<Rc<Program>, Env> for NameAnalyzer {
    /// Builds an AstWrapper tree representing the environment of names
    /// throughout the program. This tree creates a new set of Bindings
    /// for each new scope, allowing for variable shadowing.
    fn visit(&mut self, program: Rc<Program>) -> Env {

        // The top-level environment has empty bindings and a reference to the Program.
        let top: Env = Environment::new(program.clone());

        // Do name analysis on the Main class
        let main_env = self.visit((program.main.clone(), top.clone()));
        top.borrow_mut().children.push(main_env);

        // Do name analysis on all other classes
        for class in program.classes.iter() {
            let class_env = self.visit((class.clone(), top.clone()));
            top.borrow_mut().children.push(class_env);
        }

        top
    }
}

impl Visitor<(Rc<Main>, Env), Env> for NameAnalyzer {
    fn visit(&mut self, (main, top_env): (Rc<Main>, Env)) -> Env {

        // Add the Main class name to the top environment.
        top_env.borrow_mut().data.declare(&main.id, Metadata::new_class(self.make_uid()));

        // Create the Main class environment extending the top environment.
        let main_env = Environment::with_parent(main.clone(), Some(top_env.clone()));

        // Add the Main class args variable (String[] args) to the Main class environment.
        main_env.borrow_mut().data.declare(&main.args, Metadata::new_variable(self.make_uid()));

        // Do name analysis on the body of the Main class.
        self.visit((main.body.clone(), main_env.clone()));

        main_env
    }
}

impl<T: AsRef<Extends>> Visitor<(Option<T>, Env), Env> for NameAnalyzer {
    /// If this "extends" node _does_ extend another class, return the environment of the
    /// extended class. Otherwise, return the top environment.
    fn visit(&mut self, (extends, top_env): (Option<T>, Env)) -> Env {
        match extends {
            // If this class does not extend another, its environment extends the top environment.
            None => top_env,
            // If this class extends another, its environment extends the other's environment.
            Some(extends) => {
                match Environment::get(top_env.clone(), &extends.as_ref().extended) {
                    None => {
                        self.errors.push(SemanticError::extending_undelcared(&extends.as_ref().extended).into());
                        top_env
                    }
                    Some(meta) => {
                        if meta.kind != NameKind::Class {
                            self.errors.push(format_err!("unexpected err: Extending non-class identifier"));
                        }
                        // FIXME this should return the extended class environment
                        top_env
                    },
                }
            }
        }
    }
}

impl Visitor<(Rc<Class>, Env), Env> for NameAnalyzer {
    fn visit(&mut self, (class, top_env): (Rc<Class>, Env)) -> Env {

        // Add this class name to the top environment.
        top_env.borrow_mut().data.declare(&class.id, Metadata::new_class(self.make_uid()));

        let super_env = self.visit((class.extends.as_ref(), top_env));

        // Create the class environment extending the top environment.
        let class_env = Environment::with_parent(class.clone(), Some(super_env.clone()));

        // Add this class's variables to the class scope.
        for variable in class.variables.iter() {
            class_env.borrow_mut().data.declare(&variable.name, Metadata::new_variable(self.make_uid()));
        }

        // Perform name analysis on each function in this class.
        for function in class.functions.iter() {
            self.visit((function.clone(), class_env.clone()));
        }

        class_env
    }
}

impl Visitor<(Rc<Statement>, Env), Option<Env>> for NameAnalyzer {
    /// Some statements create new Environments, whereas others simply add to the
    /// environment they're in. For example, a Block statement creates a new
    /// environment in which names may be shadowed, but an Assign statement simply
    /// adds a new variable binding to the existing environment. To distinguish between
    /// these types of cases, we return an `Option<Env>`.
    fn visit(&mut self, (statement, super_env): (Rc<Statement>, Env)) -> Option<Env> {
        match *statement {
            // In a Block statement, create a new environment extending the current one and
            // visit each statement in the block using the new environment.
            Statement::Block { ref statements, .. } => {
                let stmt_env = Environment::with_parent(statement.clone(), Some(super_env.clone()));
                for s in statements.iter() {
                    let child_env = self.visit((s.clone(), stmt_env.clone()));
                    // If a child statement creates a new environment (e.g. another block
                    // statement), add that environment as a child of this one.
                    if let Some(env) = child_env {
                        stmt_env.borrow_mut().children.push(env);
                    }
                }
                Some(stmt_env)
            }
            // In an Assign statement, we don't create a new environment, we simply add a
            // new variable binding to the environment above.
            Statement::Assign { ref lhs, .. } => {
                super_env.borrow_mut().data.declare(lhs, Metadata::new_variable(self.make_uid()));
                None
            }
            Statement::SideEffect { .. } => {
                None
            }
            ref s => unimplemented!("Name Analyzer for Statement {:?}", s),
        }
    }
}

impl Visitor<(Rc<Function>, Env), Env> for NameAnalyzer {
    fn visit(&mut self, (f, super_env): (Rc<Function>, Env)) -> Env {

        // Check that a function by this name has not already been declared in this environment.
        match Environment::get(super_env.clone(), &f.name) {
            // If something by this name is declared, log an error based on what kind of
            // item was named that.
            Some(meta) => {
                let kind = NameKind::Function { ast: f.clone() };
                self.errors.push(SemanticError::unavailable_name(&f.name, &kind, &meta.kind).into());
            }
            // If nothing in the environment has the same name, add this function to the environment.
            None => {
                let meta = Metadata::new_method(self.make_uid(), f.clone());
                super_env.borrow_mut().data.declare(&f.name, meta);
            },
        }

        // Create a new environment for this function extending the outer environment.
        let func_env: Env = Environment::with_parent(f.clone(), Some(super_env.clone()));

        // Add the function arguments to the function scope.
        for arg in f.args.iter() {
            // Verify that no arguments name are in conflict.
            if let Some(meta) = func_env.borrow().data.get(&arg.name) {
                self.errors.push(SemanticError::unavailable_name(&arg.name, &NameKind::Variable, &meta.kind).into());
                continue;
            }
            func_env.borrow_mut().data.declare(&arg.name, Metadata::new_variable(self.make_uid()));
        }

        // Add the function's local variables to the function scope.
        for var in f.variables.iter() {
            // Verify that no variable names are in conflict.
            if let Some(meta) = func_env.borrow().data.get(&var.name) {
                self.errors.push(SemanticError::unavailable_name(&var.name, &NameKind::Variable, &meta.kind).into());
                continue;
            }
            func_env.borrow_mut().data.declare(&var.name, Metadata::new_variable(self.make_uid()));
        }

        func_env
    }
}
