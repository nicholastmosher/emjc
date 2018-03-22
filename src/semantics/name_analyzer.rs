use Result;
use failure::Error;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use super::{
    SemanticError,
    Symbol,
    Scope,
    GlobalScope,
    ClassScope,
    FunctionScope,
};

use syntax::visitor::Visitor;
use syntax::ast::*;

pub struct NameAnalyzer {
    pub errors: Vec<Error>,
    symbol_count: usize,
    global_scope: Rc<GlobalScope>,
}

impl NameAnalyzer {
    pub fn new() -> Self {
        NameAnalyzer {
            errors: vec![],
            symbol_count: 0,
            global_scope: GlobalScope::new(),
        }
    }

    pub fn analyze(&mut self, program: &Rc<Program>) {
        info!("Performing name analysis");
        self.visit(program.clone());
    }

    /// Creates a new unique symbol for the given Identifier, assigning the Symbol
    /// to the Identifier and also returning a reference to that Symbol.
    fn make_symbol(&mut self, id: &Rc<Identifier>) -> Rc<Symbol> {
        let uid = Some(self.symbol_count);
        self.symbol_count += 1;
        let symbol = Rc::new(Symbol { id: String::from(&id.text), uid });
        id.set_symbol(&symbol);
        symbol
    }
}

impl Visitor<Rc<Program>> for NameAnalyzer {
    fn visit(&mut self, program: Rc<Program>) {
        self.visit(program.main.clone());
        for class in program.classes.iter() {
            self.visit(class.clone());
        }
    }
}

impl Visitor<Rc<Main>> for NameAnalyzer {
    fn visit(&mut self, main: Rc<Main>) {
        debug!("Name checking main class '{}'", &main.id.text);
        let main_symbol = self.make_symbol(&main.id);

        // Build a scope for the main class.
        let main_scope = ClassScope::new(&main_symbol);

        let main_func_symbol = Symbol::unresolved(&main.func);

        let args_symbol = self.make_symbol(&main.args);

        let main_func = FunctionScope::new(&main_func_symbol, main_scope.clone());

        main_scope.functions.borrow_mut().insert(main.func.clone(), Rc::new(main_func));

        self.global_scope.classes.borrow_mut().insert(main.id.clone(), main_scope);
    }
}

impl Visitor<Rc<Class>> for NameAnalyzer {
    fn visit(&mut self, class: Rc<Class>) {
        debug!("Name checking class '{}'", &class.id.text);
        let class_symbol = self.make_symbol(&class.id);

        // If this class extends another, link this class's scope to the extended one.
        let extending = class.extends.as_ref().and_then(|extends| {
            let extended = self.global_scope.classes.borrow().get(&extends.extended).map(|rc| rc.clone());
            if extended.is_none() {
                self.errors.push(SemanticError::extending_undelcared(extends.extended.clone()).into());
            }
            extended
        });

        let mut class_scope = match extending {
            Some(ref super_class) => ClassScope::extending(&class_symbol, super_class),
            None => ClassScope::new(&class_symbol),
        };

        // Process the variables in this class
        for var in class.variables.iter() {
            debug!("Processing variable {}", &var.name.text);
            let var_symbol = self.make_symbol(&var.name);

            // If this class extends another, verify that this variable doesn't override another.
            let mut super_class = &extending;
            loop {
                if super_class.is_none() {
                    class_scope.variables.borrow_mut().insert(var.name.clone(), var_symbol);
                    break;
                }

                // If a superclass has a definition for a variable with this name, give an error.
                let up = super_class.as_ref().unwrap();
                if up.variables.borrow().contains_key(&var.name) {
                    self.errors.push(SemanticError::variable_override(&var.name).into());
                    break;
                }

                // Check whether the superclass has a superclass.
                super_class = &up.extending;
            }
        }

        // Process the functions in this class
        for func in class.functions.iter() {
            let func_symbol = self.make_symbol(&func.name);

            // Create the Function Scope for this function.
            let mut func_scope = self.visit((func.clone(), class_scope.clone()));
            let mut super_class = &extending;
            loop {
                if super_class.is_none() {
                    class_scope.functions.borrow_mut().insert(func.name.clone(), Rc::new(func_scope));
                    break;
                }

                // If a superclass has a definition for a function with this name, this function
                // is overriding the superclass function.
                let up = super_class.as_ref().unwrap();
                if let Some(super_func) = up.functions.borrow().get(&func.name) {
                    func_scope.overriding = Some(super_func.clone());
                    class_scope.functions.borrow_mut().insert(func.name.clone(), Rc::new(func_scope));
                    break;
                }
            }
        }

        self.global_scope.classes.borrow_mut().insert(class.id.clone(), class_scope);
    }
}

impl Visitor<(Rc<Function>, Rc<ClassScope>), FunctionScope> for NameAnalyzer {
    fn visit(&mut self, (function, class_scope): (Rc<Function>, Rc<ClassScope>)) -> FunctionScope {
        debug!("Name checking function '{}'", function.name.text);
        let func_symbol = self.make_symbol(&function.name);

        let mut func_scope = FunctionScope::new(&func_symbol, class_scope);

        // Create new symbols for each argument and add them to the function scope.
        for arg in function.args.iter() {
            let arg_symbol = self.make_symbol(&arg.name);
            func_scope.variables.insert(arg.name.clone(), arg_symbol);
        }

        // Create new symbols for each variable and add them to the function scope.
        for var in function.variables.iter() {
            let var_symbol = self.make_symbol(&var.name);
            func_scope.variables.insert(var.name.clone(), var_symbol);
        }

        // Check that for each statement that references a variable or other item, that
        // the item being referenced exists.
        for stmt in function.statements.iter() {
            self.visit((stmt.clone(), &func_scope as &Scope));
        }

        func_scope
    }
}

impl<'a> Visitor<(Rc<Statement>, &'a Scope)> for NameAnalyzer {
    fn visit(&mut self, (statement, scope): (Rc<Statement>, &'a Scope)) {
        match *statement {
            Statement::Assign { ref lhs, ref rhs, .. } => {
                // Check that the left-hand identifier maps to a symbol in the environment.
                self.visit((lhs.clone(), scope));
                self.visit((rhs.clone(), scope));
            }
            Statement::AssignArray { ref lhs, ref in_bracket, ref rhs, .. } => {
                self.visit((lhs.clone(), scope));
                self.visit((in_bracket.clone(), scope));
                self.visit((rhs.clone(), scope));
            }
            Statement::SideEffect { ref expression, .. } => {
                self.visit((expression.clone(), scope));
            }
            Statement::Block { ref statements, .. } => {
                for stmt in statements.iter() {
                    self.visit((stmt.clone(), scope));
                }
            }
            Statement::Print { ref expression, .. } => {
                self.visit((expression.clone(), scope));
            }
            Statement::While { ref expression, ref statement, .. } => {
                self.visit((expression.clone(), scope));
                self.visit((statement.clone(), scope));
            }
            Statement::If { ref condition, ref statement, ref otherwise, .. } => {
                self.visit((condition.clone(), scope));
                self.visit((statement.clone(), scope));
                if let Some(otherwise) = otherwise.as_ref() {
                    self.visit((otherwise.clone(), scope));
                }
            }
        }
    }
}

impl<'a> Visitor<(Rc<Expression>, &'a Scope)> for NameAnalyzer {
    fn visit(&mut self, (expression, scope): (Rc<Expression>, &'a Scope)) {
        match *expression {
            Expression::NewClass(ref id) => {
                self.visit((id.clone(), scope));
            },
            Expression::Identifier(ref id) => {
                self.visit((id.clone(), scope));
            },
            Expression::Unary(ref unary) => self.visit((unary, scope)),
            Expression::Binary(ref binary) => self.visit((binary, scope)),
            _ => (),
        }
    }
}

impl<'a, 'b> Visitor<(&'a UnaryExpression, &'b Scope)> for NameAnalyzer {
    fn visit(&mut self, (unary, scope): (&'a UnaryExpression, &'b Scope)) {
        match *unary {
            UnaryExpression::NewArray(ref expr) => self.visit((expr.clone(), scope)),
            UnaryExpression::Not(ref expr) => self.visit((expr.clone(), scope)),
            UnaryExpression::Parentheses(ref expr) => self.visit((expr.clone(), scope)),
            UnaryExpression::Length(ref expr) => self.visit((expr.clone(), scope)),
            UnaryExpression::Application { ref expression, ref id, ref list, .. } => {
                self.visit((expression.clone(), scope));

                // In this phase, leave function identifiers unresolved.
                id.set_symbol(&Symbol::unresolved(id));

                for expr in list.iter() {
                    self.visit((expr.clone(), scope));
                }
            }
        }
    }
}

impl<'a, 'b> Visitor<(&'a BinaryExpression, &'b Scope)> for NameAnalyzer {
    fn visit(&mut self, (binary, scope): (&'a BinaryExpression, &'b Scope)) {
        self.visit((binary.lhs.clone(), scope));
        self.visit((binary.rhs.clone(), scope));
    }
}

impl<'a> Visitor<(Rc<Identifier>, &'a Scope), bool> for NameAnalyzer {
    /// If the given identifier was found in the given scope, return true. Otherwise,
    /// return false to indicate that we should search again after "hoisting" all of
    /// the rest of the items in the scope.
    fn visit(&mut self, (id, scope): (Rc<Identifier>, &'a Scope)) -> bool {
        if let Some(symbol) = scope.find(&id) {
            id.set_symbol(&symbol);
            true
        } else {
            self.errors.push(SemanticError::using_undeclared(id).into());
            false
        }
    }
}
