use super::{
    SemanticError,
    Environment,
    Env,
};

use syntax::visitor::Visitor;
use syntax::ast::*;

pub struct NameAnalyzer {
    errors: Vec<SemanticError>,
}

impl NameAnalyzer {
    pub fn new() -> Self {
        NameAnalyzer { errors: vec![] }
    }

    pub fn analyze(&mut self, program: &Program) {
        self.visit(program);
    }
}

impl Visitor<Program, Env> for NameAnalyzer {
    fn visit(&mut self, program: &Program) -> Env {
        let mut top_environment = Environment::new();

        // Do name analysis on the Main class
        let main_env = self.visit(&(&program.main, &top_environment));
        top_environment.borrow_mut().sub_envs.push(main_env);

        // Do name analysis on all other classes
        for class in program.classes.iter() {
            let class_env = self.visit(&(class, &top_environment));
            top_environment.borrow_mut().sub_envs.push(class_env);
        }

        top_environment
    }
}

impl<'a> Visitor<(&'a Main, &'a Env), Env> for NameAnalyzer {
    fn visit(&mut self, &(main, top_env): &(&'a Main, &'a Env)) -> Env {

        // Add the Main class name to the top environment.
        top_env.borrow_mut().bindings.declare(&main.id);

        // Create the Main class environment extending the top environment.
        let main_env = Environment::with_super(top_env);

        // Add the Main class args variable (String[] args) to the Main class environment.
        main_env.borrow_mut().bindings.declare(&main.args);

        // Do name analysis on the body of the Main class.
        self.visit(&(&main.body, &main_env));

        main_env
    }
}

impl<'a> Visitor<(&'a Class, &'a Env), Env> for NameAnalyzer {
    fn visit(&mut self, &(class, top_env): &(&'a Class, &'a Env)) -> Env {

        // If this class extends another class, verify that the extended class exists.
        if let Some(ref class) = class.extends {
            if !Environment::has_declared(top_env.clone(), &class.extended) {
                self.errors.push(SemanticError::ExtendingUndeclared)
            }
        }

        // Add this class name to the top environment.
        top_env.borrow_mut().bindings.declare(&class.id);

        // Create the class environment extending the top environment.
        let class_env = Environment::with_super(top_env);

        // Add this class's variables to the class scope.
        for variable in class.variables.iter() {
            class_env.borrow_mut().bindings.declare(&variable.name);
        }

        // Perform name analysis on each function in this class.
        for function in class.functions.iter() {
            self.visit(&(function, &class_env));
        }

        class_env
    }
}

impl<'a> Visitor<(&'a Statement, &'a Env), Option<Env>> for NameAnalyzer {
    /// Some statements create new Environments, whereas others simply add to the
    /// environment they're in. For example, a Block statement creates a new
    /// environment in which names may be shadowed, but an Assign statement simply
    /// adds a new variable binding to the existing environment. To distinguish between
    /// these types of cases, we return an `Option<Env>`.
    fn visit(&mut self, &(statement, super_env): &(&'a Statement, &'a Env)) -> Option<Env> {

        match *statement {
            /// In a Block statement, create a new environment extending the current one and
            /// visit each statement in the block using the new environment.
            Statement::Block { ref statements, .. } => {
                let stmt_env = Environment::with_super(&super_env);
                for s in statements.iter() {
                    let child_env = self.visit(&(s, &stmt_env));
                    if let Some(env) = child_env {
                        stmt_env.borrow_mut().sub_envs.push(env);
                    }
                }
                Some(stmt_env)
            },
            /// In an Assign statement, we don't create a new environment, we simply add a
            /// new variable binding to the environment above.
            Statement::Assign { ref lhs, .. } => {
                super_env.borrow_mut().bindings.declare(lhs);
                None
            },
            Statement::SideEffect { ref expression, .. } => {
                None
            },
            ref s => unimplemented!("Name Analyzer for Statement {:?}", s),
        }
    }
}

impl <'a> Visitor<(&'a Function, &'a Env), Env> for NameAnalyzer {
    fn visit(&mut self, &(function, super_env): &(&'a Function, &'a Env)) -> Env {

        // Check that a function by this name has not already been declared in this environment.
        if Environment::has_declared(super_env.clone(), &function.name) {
            self.errors.push(SemanticError::ConflictingDeclaration);
        }

        // Create a new environment for this function extending the outer environment.
        let func_env = Environment::with_super(super_env);

        // Add the function arguments to the function scope.
        for arg in function.args.iter() {
            func_env.borrow_mut().bindings.declare(&arg.name);
        }

        // Add the function's local variables to the function scope.
        for var in function.variables.iter() {
            func_env.borrow_mut().bindings.declare(&var.name);
        }

        func_env
    }
}
