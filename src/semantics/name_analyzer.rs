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
};

use syntax::visitor::Visitor;
use syntax::ast::*;

/// An Environment is a tree parallel to the AST at which each node has a set
/// of Bindings associated with it. The Bindings store information regarding
/// declarations of identifiers as well as their usages.
type Environment = AstWrapper<Bindings>;

/// The Env type is a shorthand for Environment which is useful for navigating
/// the tree structure, which makes heavy use of Rc<RefCell<_>>.
type Env = Rc<RefCell<Environment>>;

impl Environment {
    /// Creates a new, bare environment "wrapping" an AST node.
    fn new<T: Into<AstNode>>(ast_node: T) -> Env {
        Self::with_parent(ast_node, None)
    }

    /// Creates a new environment "wrapping" an AST node, optionally extending
    /// a given parent environment.
    fn with_parent<T: Into<AstNode>>(ast_node: T, parent: Option<Env>) -> Env {
        Rc::new(RefCell::new(
            AstWrapper {
                parent: parent.map(|ref e| Rc::downgrade(e)),
                children: vec![],
                ast_node: ast_node.into(),
                data: Bindings::new(),
            }
        ))
    }

    fn get<I: AsRef<Identifier>>(mut env: Env, id: I) -> Option<Metadata> {
        loop {
            if let Some(meta) = env.borrow().data.get(&id) {
                return Some(*meta);
            }

            let super_env = env.borrow().parent.as_ref().and_then(|s| s.upgrade());
            if super_env.is_some() {
                env = super_env.unwrap();
                continue;
            }
            return None;
        }
    }
}

pub struct NameAnalyzer {
    errors: Vec<Error>,
}

impl NameAnalyzer {
    pub fn new() -> Self {
        NameAnalyzer { errors: vec![] }
    }

    pub fn analyze(&mut self, program: &Program) {
        self.visit(Rc::new(program.clone()));
        for err in self.errors.iter() {
            println!("Name analysis error: {}", err);
        }
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
        top_env.borrow_mut().data.declare(&main.id, Metadata::new_class());

        // Create the Main class environment extending the top environment.
        let main_env = Environment::with_parent(main.clone(), Some(top_env.clone()));

        // Add the Main class args variable (String[] args) to the Main class environment.
        main_env.borrow_mut().data.declare(&main.args, Metadata::new_variable());

        // Do name analysis on the body of the Main class.
        self.visit((main.body.clone(), main_env.clone()));

        main_env
    }
}

impl Visitor<(Rc<Class>, Env), Env> for NameAnalyzer {
    fn visit(&mut self, (class, top_env): (Rc<Class>, Env)) -> Env {

        // If this class extends another class, verify that the extended class exists.
        if let Some(ref class) = class.extends {
            match Environment::get(top_env.clone(), &class.extended) {
                None => {
                    self.errors.push(SemanticError::extending_undelcared(&class.extended).into());
                }
                Some(meta) => {
                    if meta.kind != NameKind::Class {
                        self.errors.push(format_err!("unexpected err: Extending non-class identifier"));
                    }
                },
            }
        }

        // Add this class name to the top environment.
        top_env.borrow_mut().data.declare(&class.id, Metadata::new_class());

        // Create the class environment extending the top environment.
        let class_env = Environment::with_parent(class.clone(), Some(top_env.clone()));

        // Add this class's variables to the class scope.
        for variable in class.variables.iter() {
            class_env.borrow_mut().data.declare(&variable.name, Metadata::new_variable());
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
                super_env.borrow_mut().data.declare(lhs, Metadata::new_variable());
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
    fn visit(&mut self, (function, super_env): (Rc<Function>, Env)) -> Env {

        // Check that a function by this name has not already been declared in this environment.
        match Environment::get(super_env.clone(), &function.name) {
            // If something by this name is declared, log an error based on what kind of
            // item was named that.
            Some(meta) => {
                self.errors.push(SemanticError::unavailable_name(&function.name, NameKind::Method, meta.kind).into());
            }
            // If nothing in the environment has the same name, continue.
            None => (),
        }

        // Create a new environment for this function extending the outer environment.
        let func_env: Env = Environment::with_parent(function.clone(), Some(super_env.clone()));

        // Add the function arguments to the function scope.
        for arg in function.args.iter() {
            // Verify that no arguments name are in conflict.
            if let Some(meta) = func_env.borrow().data.get(&arg.name) {
                self.errors.push(SemanticError::unavailable_name(&arg.name, NameKind::Variable, meta.kind).into());
                continue;
            }
            func_env.borrow_mut().data.declare(&arg.name, Metadata::new_variable());
        }

        // Add the function's local variables to the function scope.
        for var in function.variables.iter() {
            // Verify that no variable names are in conflict.
            if let Some(meta) = func_env.borrow().data.get(&var.name) {
                self.errors.push(SemanticError::unavailable_name(&var.name, NameKind::Variable, meta.kind).into());
                continue;
            }
            func_env.borrow_mut().data.declare(&var.name, Metadata::new_variable());
        }

        func_env
    }
}
