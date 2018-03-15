use std::rc::{Rc, Weak};
use std::cell::RefCell;

use super::{
    SemanticError,
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
    fn new<T: Into<AstNode>>(ast_node: T) -> Env {
        Self::with_parent(ast_node, None)
    }

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

    /// Given an Identifier, tell whether an item by that name has been declared
    /// in this Environment or any super Environment.
    fn has_declared(mut env: Env, id: &Identifier) -> bool {
        loop {
            if env.borrow().data.contains(id) { return true }
            let super_env = env.borrow().parent.as_ref().and_then(|s| s.upgrade());
            if super_env.is_some() {
                env = super_env.unwrap();
                continue;
            }
            return false;
        }
    }
}

pub struct NameAnalyzer {
    errors: Vec<SemanticError>,
}

impl NameAnalyzer {
    pub fn new() -> Self {
        NameAnalyzer { errors: vec![] }
    }

    pub fn analyze(&mut self, program: &Program) {
        self.visit(Rc::new(program.clone()));
    }
}

impl Visitor<Rc<Program>, Env> for NameAnalyzer {
    fn visit(&mut self, program: Rc<Program>) -> Env {

        let mut top: Env = Environment::new(program.clone());

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
        top_env.borrow_mut().data.declare(&main.id);

        // Create the Main class environment extending the top environment.
        let main_env = Environment::with_parent(main.clone(), Some(top_env.clone()));

        // Add the Main class args variable (String[] args) to the Main class environment.
        main_env.borrow_mut().data.declare(&main.args);

        // Do name analysis on the body of the Main class.
        self.visit((main.body.clone(), main_env.clone()));

        main_env
    }
}

impl Visitor<(Rc<Class>, Env), Env> for NameAnalyzer {
    fn visit(&mut self, (class, top_env): (Rc<Class>, Env)) -> Env {

        // If this class extends another class, verify that the extended class exists.
        if let Some(ref class) = class.extends {
            if !Environment::has_declared(top_env.clone(), &class.extended) {
                self.errors.push(SemanticError::ExtendingUndeclared)
            }
        }

        // Add this class name to the top environment.
        top_env.borrow_mut().data.declare(&class.id);

        // Create the class environment extending the top environment.
        let class_env = Environment::with_parent(class.clone(), Some(top_env.clone()));

        // Add this class's variables to the class scope.
        for variable in class.variables.iter() {
            class_env.borrow_mut().data.declare(&variable.name);
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
            /// In a Block statement, create a new environment extending the current one and
            /// visit each statement in the block using the new environment.
            Statement::Block { ref statements, .. } => {
                let stmt_env = Environment::with_parent(statement.clone(), Some(super_env.clone()));
                for s in statements.iter() {
                    let child_env = self.visit((s.clone(), stmt_env.clone()));
                    if let Some(env) = child_env {
                        stmt_env.borrow_mut().children.push(env);
                    }
                }
                Some(stmt_env)
            },
            /// In an Assign statement, we don't create a new environment, we simply add a
            /// new variable binding to the environment above.
            Statement::Assign { ref lhs, .. } => {
                super_env.borrow_mut().data.declare(lhs);
                None
            },
            Statement::SideEffect { ref expression, .. } => {
                None
            },
            ref s => unimplemented!("Name Analyzer for Statement {:?}", s),
        }
    }
}

impl Visitor<(Rc<Function>, Env), Env> for NameAnalyzer {
    fn visit(&mut self, (function, super_env): (Rc<Function>, Env)) -> Env {

        // Check that a function by this name has not already been declared in this environment.
        if Environment::has_declared(super_env.clone(), &function.name) {
            self.errors.push(SemanticError::ConflictingDeclaration);
        }

        // Create a new environment for this function extending the outer environment.
        let func_env = Environment::with_parent(function.clone(), Some(super_env.clone()));

        // Add the function arguments to the function scope.
        for arg in function.args.iter() {
            func_env.borrow_mut().data.declare(&arg.name);
        }

        // Add the function's local variables to the function scope.
        for var in function.variables.iter() {
            func_env.borrow_mut().data.declare(&var.name);
        }

        func_env
    }
}
