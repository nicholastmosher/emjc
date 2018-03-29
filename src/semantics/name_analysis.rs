use failure::Error;
use std::rc::Rc;
use std::fmt::{
    Display,
    Formatter,
    Result as fmtResult,
};
use std::marker::PhantomData;

use super::{
    Symbol,
    Scope,
    GlobalScope,
    ClassScope,
    FunctionScope,
};

use lexer::OwnedToken;
use syntax::visitor::Visitor;
use syntax::ast::*;

#[derive(Debug, Fail)]
pub enum NameError {
    ExtendingUndeclared(OwnedToken),
    VariableOverride(OwnedToken),
    UsingUndeclared(OwnedToken),
    ConflictingVariable(OwnedToken),
    ConflictingClass(OwnedToken),
    InheritanceCycle(OwnedToken, OwnedToken),
    OverloadedFunction(OwnedToken),
    OverrideMismatch(OwnedToken, usize, usize),
    StaticThis,
}

impl Display for NameError {
    fn fmt(&self, f: &mut Formatter) -> fmtResult {
        match *self {
            NameError::ExtendingUndeclared(ref t) => write!(f, "{}:{} name error: extending undeclared class '{}'", t.line, t.column, t.text),
            NameError::VariableOverride(ref t) => write!(f, "{}:{} name error: variable '{}' overrides variable in superclass", t.line, t.column, t.text),
            NameError::UsingUndeclared(ref t) => write!(f, "{}:{} name error: use of undeclared identifier '{}'", t.line, t.column, t.text),
            NameError::ConflictingVariable(ref t) => write!(f, "{}:{} name error: conflicting variable declaration '{}'", t.line, t.column, t.text),
            NameError::ConflictingClass(ref t) => write!(f, "{}:{} name error: conflicting class declaration '{}'", t.line, t.column, t.text),
            NameError::InheritanceCycle(ref t, ref e) => write!(f, "{}:{} name error: cyclic inheritance at '{} extends {}'", t.line, t.column, t.text, e.text),
            NameError::OverloadedFunction(ref t) => write!(f, "{}:{} name error: overloaded function '{}'", t.line, t.column, t.text),
            NameError::OverrideMismatch(ref t, ref actual, ref expected) => {
                write!(f, "{}:{} name error: function '{}' has {} argument{} but overrides a function with {} argument{}",
                    t.line, t.column, t.text,
                    actual, if *actual == 1 { "" } else { "s" },
                    expected, if *expected == 1 { "" } else { "s" },
                )
            },
            NameError::StaticThis => write!(f, "name error: use of 'this' keyword in main"),
        }
    }
}

impl NameError {
    fn extending_undelcared<T: Into<OwnedToken>>(token: T) -> NameError { NameError::ExtendingUndeclared(token.into()) }
    fn variable_override<T: Into<OwnedToken>>(token: T) -> NameError { NameError::VariableOverride(token.into()) }
    fn using_undeclared<T: Into<OwnedToken>>(token: T) -> NameError { NameError::UsingUndeclared(token.into()) }
    fn conflicting_variable<T: Into<OwnedToken>>(token: T) -> NameError { NameError::ConflictingVariable(token.into()) }
    fn conflicting_class<T: Into<OwnedToken>>(token: T) -> NameError { NameError::ConflictingClass(token.into()) }
    fn overloaded_function<T: Into<OwnedToken>>(token: T) -> NameError { NameError::OverloadedFunction(token.into()) }
    fn override_mismatch<T: Into<OwnedToken>>(token: T, actual: usize, expected: usize) -> NameError { NameError::OverrideMismatch(token.into(), actual, expected) }
    fn static_this() -> NameError { NameError::StaticThis }
    fn inheritance_cycle<T1, T2>(token: T1, extends: T2) -> NameError
        where T1: Into<OwnedToken>,
              T2: Into<OwnedToken>,
    { NameError::InheritanceCycle(token.into(), extends.into()) }
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Name {
    id: String,
    /// A unique identifier for this item. This allows for multiple items to be given
    /// the same name but still be unique. This is used in variable shadowing, where
    /// two different variables may have the same name but refer to different memory.
    uid: Option<usize>,
}

impl Name {
    pub fn new(id: &str, uid: usize) -> Name {
        Name {
            id: id.to_owned(),
            uid: Some(uid),
        }
    }

    pub fn unresolved(id: &Rc<Identifier>) -> Name {
        Name { id: String::from(&id.text), uid: None }
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut Formatter) -> fmtResult {
        write!(f, "{}", self.id);
        match self.uid {
            None => write!(f, "_#error#_"),
            Some(uid) => write!(f, "_{}_", uid),
        }
    }
}

pub struct NameAnalyzer {
    pub errors: Vec<Error>,
}

impl NameAnalyzer {
    pub fn new() -> Self {
        NameAnalyzer {
            errors: vec![],
        }
    }

    pub fn analyze(&mut self, program: &Rc<Program>) -> Rc<GlobalScope> {
        info!("Performing name analysis");

        let mut generator = SymbolVisitor::new();
        generator.visit(program.clone());

        let mut linker: SymbolVisitor<Linker> = generator.into();
        linker.visit(program.clone());

        self.errors.extend(linker.errors);
        linker.global_scope
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
        let uid = self.symbol_count;
        self.symbol_count += 1;
        let symbol = Symbol::new(&id.text, uid);
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
        debug!("Generating symbols for main class '{}'", &main.id.text);
        let main_symbol = self.make_symbol(&main.id);

        // Make a symbol for the main function
        self.make_symbol(&main.func);

        // Build and save a scope for the main class.
        let main_scope = ClassScope::new(&main_symbol, &self.global_scope);
        self.make_symbol(&main.args);
        self.global_scope.classes.borrow_mut().insert(main.id.clone(), main_scope);
    }
}

impl Visitor<Rc<Main>> for SymbolVisitor<Linker> {
    fn visit(&mut self, main: Rc<Main>) {
        let main_scope = self.global_scope.get(&main.id).expect("Main class should have a symbol");

        // Link all the identifier usages in Main's statements.
        self.visit((main.body.clone(), &*main_scope as &Scope));
    }
}

impl Visitor<Rc<Class>> for SymbolVisitor<Generator> {
    fn visit(&mut self, class: Rc<Class>) {
        debug!("Generating symbols for class '{}'", &class.id.text);

        // Check for duplicate classes
        if self.global_scope.classes.borrow().contains_key(&class.id) {
            warn!("Found conflicting class {}", &class.id.text);
            self.errors.push(NameError::conflicting_class(&class.id).into());
            return;
        }

        let class_symbol = self.make_symbol(&class.id);
        let class_scope = ClassScope::new(&class_symbol, &self.global_scope);

        // Process the variables in this class
        for var in class.variables.iter() {
            debug!("Generating symbol for variable '{}'", &var.name.text);

            // If a variable with this name was already declared, give an error.
            if class_scope.variables.borrow().contains_key(&var.name) {
                self.errors.push(NameError::conflicting_variable(&var.name).into());
            } else {
                let var_symbol = self.make_symbol(&var.name);
                class_scope.variables.borrow_mut().insert(var.name.clone(), var_symbol);
            }
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
        debug!("Linking symbols for class '{}'", &class.id.text);
        let class_scope = self.global_scope.get(&class.id).expect("Every class should have a symbol");

        // If this class extends another, find the scope of the class it extends.
        if let Some(ref extends) = class.extends.as_ref() {
            match self.global_scope.get(&extends.extended) {
                // If we don't find a scope for the extended class, give an error.
                None => self.errors.push(NameError::extending_undelcared(extends.extended.clone()).into()),
                // If we find the extended class scope, link this scope to it.
                Some(ref extended) => { class_scope.extending.replace(Some(extended.clone())); }
            }
        }

        // Check for cyclic inheritance
        if class_scope.cycle() {
            self.errors.push(NameError::inheritance_cycle(&class.id, &class.extends.as_ref().unwrap().extended).into())
        }

        for var in class.variables.iter() {
            // Walk up the class scopes, checking whether a variable by this name already exists.
            let mut super_class = class_scope.extending.borrow().as_ref().map(|rc| rc.clone());
            loop {
                if super_class.is_none() { break; }
                if super_class.as_ref().unwrap().variables.borrow().contains_key(&var.name) {
                    // If a variable with this name exists in a super scope, give an error.
                    self.errors.push(NameError::variable_override(&var.name).into());
                    break;
                }

                let upper = super_class.as_ref().unwrap().extending.borrow().as_ref().map(|rc| rc.clone());
                super_class = upper;
            }
        }

        for func in class.functions.iter() {
            if let Some(func_scope) = class_scope.functions.borrow().get(&func.name) {
                self.visit((func.clone(), func_scope.clone()));
            }
        }
    }
}

impl Visitor<(Rc<Function>, Rc<ClassScope>), FunctionScope> for SymbolVisitor<Generator> {
    fn visit(&mut self, (function, class_scope): (Rc<Function>, Rc<ClassScope>)) -> FunctionScope {
        debug!("Generating symbols in function '{}'", function.name.text);

        // Check for overloaded functions
        if class_scope.functions.borrow().contains_key(&function.name) {
            self.errors.push(NameError::overloaded_function(&function.name).into());
        }

        self.make_symbol(&function.name);
        let mut func_scope = FunctionScope::new(&function, class_scope);

        // Create new symbols for each argument and add them to the function scope.
        for arg in function.args.iter() {

            // Check if any arguments in this local scope have this name.
            if func_scope.variables.contains_key(&arg.name) {
                self.errors.push(NameError::conflicting_variable(&arg.name).into());
                continue;
            }

            let arg_symbol = self.make_symbol(&arg.name);
            func_scope.variables.insert(arg.name.clone(), arg_symbol);
        }

        // Create new symbols for each variable and add them to the function scope.
        for var in function.variables.iter() {

            // Check if any arguments in this local scope have this name.
            if func_scope.variables.contains_key(&var.name) {
                self.errors.push(NameError::conflicting_variable(&var.name).into());
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
        debug!("Linking symbols in function '{}'", &function.name.text);

        // If this function overrides another, check that they have the same arity of arguments
        if let Some(ref extending) = *func_scope.class.upgrade().unwrap().extending.borrow() {

            // If there's another function with the same name in scope, do an arity check.
            if let Some(other_func_scope) = extending.find_func(&function.name) {

                let other_len = other_func_scope.function.args.len();
                let my_len = function.args.len();
                if other_len != my_len {
                    self.errors.push(NameError::override_mismatch(&function.name, my_len, other_len).into());
                }
            }
        }

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
                    self.errors.push(NameError::static_this().into());
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
        debug!("Linking identifier '{}'", &id.text);
        if let Some(symbol) = scope.find(&id) {
            id.set_symbol(&symbol);
        } else {
            self.errors.push(NameError::using_undeclared(id).into());
        }
    }
}
