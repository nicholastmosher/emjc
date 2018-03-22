use Result;
use failure::Error;
use std::rc::Rc;
use std::cell::RefCell;
use std::marker::PhantomData;
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
}

impl NameAnalyzer {
    pub fn new() -> Self {
        NameAnalyzer {
            errors: vec![],
        }
    }

    pub fn analyze(&mut self, program: &Rc<Program>) {
        info!("Performing name analysis");

        let mut generator = SymbolVisitor::new();
        generator.visit(program.clone());

        let mut linker: SymbolVisitor<Linker> = generator.into();
        linker.visit(program.clone());

        self.errors.extend(linker.errors);
    }
}

enum Generator {}

enum Linker {}

struct SymbolVisitor<T> {
    symbol_count: usize,
    global_scope: Rc<GlobalScope>,
    errors: Vec<Error>,
    in_main: bool,
    kind: PhantomData<T>,
}

impl SymbolVisitor<Generator> {
    fn new() -> SymbolVisitor<Generator> {
        SymbolVisitor {
            symbol_count: 0,
            global_scope: GlobalScope::new(),
            errors: vec![],
            in_main: false,
            kind: PhantomData,
        }
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

impl From<SymbolVisitor<Generator>> for SymbolVisitor<Linker> {
    fn from(generator: SymbolVisitor<Generator>) -> Self {
        let SymbolVisitor { symbol_count, global_scope, in_main, errors, .. } = generator;
        SymbolVisitor { symbol_count, global_scope, errors, in_main, kind: PhantomData }
    }
}

impl Visitor<Rc<Program>> for SymbolVisitor<Generator> {
    fn visit(&mut self, program: Rc<Program>) {
        self.in_main = true;
        self.visit(program.main.clone());
        self.in_main = false;
        for class in program.classes.iter() {
            self.visit(class.clone());
        }
    }
}

impl Visitor<Rc<Program>> for SymbolVisitor<Linker> {
    fn visit(&mut self, program: Rc<Program>) {
        self.in_main = true;
        self.visit(program.main.clone());
        self.in_main = false;
        for class in program.classes.iter() {
            self.visit(class.clone());
        }
    }
}

impl Visitor<Rc<Main>> for SymbolVisitor<Generator> {
    fn visit(&mut self, main: Rc<Main>) {
        debug!("Name checking main class '{}'", &main.id.text);
        let main_symbol = self.make_symbol(&main.id);

        // Build and save a scope for the main class.
        let main_scope = ClassScope::new(&main_symbol, &self.global_scope);
        let main_func_symbol = Symbol::unresolved(&main.func);
        let args_symbol = self.make_symbol(&main.args);
        let main_func = FunctionScope::new(&main_func_symbol, main_scope.clone());
        main_scope.functions.borrow_mut().insert(main.func.clone(), Rc::new(main_func));
        self.global_scope.classes.borrow_mut().insert(main.id.clone(), main_scope);
    }
}

impl Visitor<Rc<Main>> for SymbolVisitor<Linker> {
    fn visit(&mut self, main: Rc<Main>) {
        let main_scope = self.global_scope.classes.borrow().get(&main.id).unwrap().clone();

        // Link all the identifier usages in Main's statements.
        self.visit((main.body.clone(), &*main_scope as &Scope));
    }
}

impl Visitor<Rc<Class>> for SymbolVisitor<Generator> {
    fn visit(&mut self, class: Rc<Class>) {
        debug!("Name checking class '{}'", &class.id.text);
        let class_symbol = self.make_symbol(&class.id);
        let mut class_scope = ClassScope::new(&class_symbol, &self.global_scope);

        // Process the variables in this class
        for var in class.variables.iter() {
            debug!("Processing variable {}", &var.name.text);
            let var_symbol = self.make_symbol(&var.name);
            class_scope.variables.borrow_mut().insert(var.name.clone(), var_symbol);
        }

        // Process the functions in this class
        for func in class.functions.iter() {
            self.make_symbol(&func.name);
            let mut func_scope = self.visit((func.clone(), class_scope.clone()));
            class_scope.functions.borrow_mut().insert(func.name.clone(), Rc::new(func_scope));
        }

        self.global_scope.classes.borrow_mut().insert(class.id.clone(), class_scope);
    }
}

impl Visitor<Rc<Class>> for SymbolVisitor<Linker> {
    fn visit(&mut self, class: Rc<Class>) {
        let class_scope = self.global_scope.classes.borrow().get(&class.id).unwrap().clone();

        // If this class extends another, find the scope of the class it extends.
        if let Some(ref extends) = class.extends.as_ref() {
            match self.global_scope.classes.borrow().get(&extends.extended).map(|rc| rc.clone()) {
                // If we don't find a scope for the extended class, give an error.
                None => self.errors.push(SemanticError::extending_undelcared(extends.extended.clone()).into()),
                // If we find the extended class scope, link this scope to it.
                Some(ref extended) => { class_scope.extending.replace(Some(extended.clone())); },
            }
        }

        // Check for cyclic inheritance
        if class_scope.cycle() {
            self.errors.push(SemanticError::inheritance_cycle(&class.id, &class.extends.as_ref().unwrap().extended).into())
        }

        for var in class.variables.iter() {
            // Walk up the class scopes, checking whether a variable by this name already exists.
            let mut super_class = class_scope.extending.borrow().as_ref().map(|rc| rc.clone());
            loop {
                if super_class.is_none() { break }
                if super_class.as_ref().unwrap().variables.borrow().contains_key(&var.name) {
                    // If a variable with this name exists in a super scope, give an error.
                    self.errors.push(SemanticError::variable_override(&var.name).into());
                    break;
                }

                let upper = super_class.as_ref().unwrap().extending.borrow().as_ref().map(|rc| rc.clone());
                super_class = upper;
            }
        }

        for func in class.functions.iter() {
            let func_scope = class_scope.functions.borrow().get(&func.name).unwrap().clone();
            self.visit((func.clone(), func_scope.clone()));
        }
    }
}

impl Visitor<(Rc<Function>, Rc<ClassScope>), FunctionScope> for SymbolVisitor<Generator> {
    fn visit(&mut self, (function, class_scope): (Rc<Function>, Rc<ClassScope>)) -> FunctionScope {
        debug!("Name checking function '{}'", function.name.text);
        let func_symbol = self.make_symbol(&function.name);

        let mut func_scope = FunctionScope::new(&func_symbol, class_scope);

        // Create new symbols for each argument and add them to the function scope.
        for arg in function.args.iter() {

            // Check if any arguments in this local scope have this name.
            if func_scope.variables.contains_key(&arg.name) {
                self.errors.push(SemanticError::conflicting_declaration(&arg.name).into());
                continue;
            }

            let arg_symbol = self.make_symbol(&arg.name);
            func_scope.variables.insert(arg.name.clone(), arg_symbol);
        }

        // Create new symbols for each variable and add them to the function scope.
        for var in function.variables.iter() {

            // Check if any arguments in this local scope have this name.
            if func_scope.variables.contains_key(&var.name) {
                self.errors.push(SemanticError::conflicting_declaration(&var.name).into());
                continue;
            }

            let var_symbol = self.make_symbol(&var.name);
            func_scope.variables.insert(var.name.clone(), var_symbol);
        }

        func_scope
    }
}

impl Visitor<(Rc<Function>, Rc<FunctionScope>)> for SymbolVisitor<Linker> {
    fn visit(&mut self, (function, func_scope): (Rc<Function>, Rc<FunctionScope>)) {
        let scope: &Scope = &*func_scope;
        for arg in function.args.iter() {
            self.visit((arg.kind.clone(), scope));
        }
        for var in function.variables.iter() {
            self.visit((var.kind.clone(), scope));
        }
        for stmt in function.statements.iter() {
            self.visit((stmt.clone(), scope));
        }
        self.visit((function.expression.clone(), scope));
    }
}

impl<'a> Visitor<(Rc<Type>, &'a Scope)> for SymbolVisitor<Linker> {
    fn visit(&mut self, (kind, scope): (Rc<Type>, &'a Scope)) {
        if let Type::Id(ref id) = *kind {
            self.visit((id.clone(), scope));
        }
    }
}

impl<'a> Visitor<(Rc<Statement>, &'a Scope)> for SymbolVisitor<Linker> {
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

impl<'a> Visitor<(Rc<Expression>, &'a Scope)> for SymbolVisitor<Linker> {
    fn visit(&mut self, (expression, scope): (Rc<Expression>, &'a Scope)) {
        match *expression {
            Expression::NewClass(ref id) => { self.visit((id.clone(), scope)); }
            Expression::Identifier(ref id) => { self.visit((id.clone(), scope)); }
            Expression::Unary(ref unary) => self.visit((unary, scope)),
            Expression::Binary(ref binary) => self.visit((binary, scope)),
            Expression::This => {
                // If we're in the main function, 'this' is illegal, give an error.
                if self.in_main {
                    self.errors.push(SemanticError::static_this().into());
                }
            }
            _ => (),
        }
    }
}

impl<'a, 'b> Visitor<(&'a UnaryExpression, &'b Scope)> for SymbolVisitor<Linker> {
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

impl<'a, 'b> Visitor<(&'a BinaryExpression, &'b Scope)> for SymbolVisitor<Linker> {
    fn visit(&mut self, (binary, scope): (&'a BinaryExpression, &'b Scope)) {
        self.visit((binary.lhs.clone(), scope));
        self.visit((binary.rhs.clone(), scope));
    }
}

impl<'a> Visitor<(Rc<Identifier>, &'a Scope)> for SymbolVisitor<Linker> {
    fn visit(&mut self, (id, scope): (Rc<Identifier>, &'a Scope)) {
        if let Some(symbol) = scope.find(&id) {
            id.set_symbol(&symbol);
        } else {
            self.errors.push(SemanticError::using_undeclared(id).into());
        }
    }
}
