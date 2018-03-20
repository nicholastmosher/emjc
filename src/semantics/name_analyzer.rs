use Result;
use failure::Error;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use super::{
    SemanticError,
    Symbol,
    GlobalScope,
    ClassScope,
    FunctionScope,
};

use syntax::visitor::Visitor;
use syntax::ast::*;

pub struct NameAnalyzer {
    symbol_count: usize,
    errors: Vec<Error>,
    global_scope: GlobalScope,
}

impl NameAnalyzer {
    pub fn new() -> Self {
        NameAnalyzer {
            symbol_count: 0,
            errors: vec![],
            global_scope: GlobalScope::new(),
        }
    }

    pub fn analyze(&mut self, program: &Program) {
        info!("Performing name analysis");
        self.visit(Rc::new(program.clone()));
        println!("{:?}", self.global_scope);
        for err in self.errors.iter() {
            println!("{}", err);
        }
    }

    fn make_symbol(&mut self, id: &Rc<Identifier>) -> Symbol {
        let uid = self.symbol_count;
        self.symbol_count += 1;
        Symbol { id: id.clone(), uid }
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
//        let main_scope = ClassScope::new(main_symbol);
//
//        let main_func = FunctionScope::new(BLAH, main_scope.clone());
//
//        main_scope.borrow_mut().functions.insert();
    }
}

impl Visitor<Rc<Class>> for NameAnalyzer {
    fn visit(&mut self, class: Rc<Class>) {
        debug!("Name checking class '{}'", &class.id.text);
        let class_symbol = self.make_symbol(&class.id);

        // If this class extends another, link this class's scope to the extended one.
        let extending = class.extends.as_ref().and_then(|extends| {
            let extended = self.global_scope.classes.get(&extends.extended).map(|rc| rc.clone());
            if extended.is_none() {
                self.errors.push(SemanticError::extending_undelcared(extends.extended.clone()).into());
            }
            extended
        });

        let mut class_scope = match extending {
            Some(ref super_class) => ClassScope::extending(class_symbol, super_class),
            None => ClassScope::new(class_symbol),
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

        self.global_scope.classes.insert(class.id.clone(), class_scope);
    }
}

impl Visitor<(Rc<Function>, Rc<ClassScope>), FunctionScope> for NameAnalyzer {
    fn visit(&mut self, (function, class_scope): (Rc<Function>, Rc<ClassScope>)) -> FunctionScope {
        debug!("Name checking function '{}'", function.name.text);
        let func_symbol = self.make_symbol(&function.name);

        let mut func_scope = FunctionScope::new(func_symbol, class_scope);

        // Create new symbols for each variable and add them to the function scope.
        for var in function.variables.iter() {
            let var_symbol = self.make_symbol(&var.name);
            func_scope.variables.insert(var.name.clone(), var_symbol);
        }

        // Check that for each statement that references a variable or other item, that
        // the item being referenced exists.
        for stmt in function.statements.iter() {
            match **stmt {
                Statement::Assign { ref lhs, ref rhs, .. } => {

                }
                _ => unimplemented!()
            }
        }

        func_scope
    }
}
