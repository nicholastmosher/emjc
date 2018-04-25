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
    Environment,
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
            NameError::ExtendingUndeclared(ref t) => write!(f, "{} name error: extending undeclared class '{}'", t.span.start, t.text),
            NameError::VariableOverride(ref t) => write!(f, "{} name error: variable '{}' overrides variable in superclass", t.span.start, t.text),
            NameError::UsingUndeclared(ref t) => write!(f, "{} name error: use of undeclared identifier '{}'", t.span.start, t.text),
            NameError::ConflictingVariable(ref t) => write!(f, "{} name error: conflicting variable declaration '{}'", t.span.start, t.text),
            NameError::ConflictingClass(ref t) => write!(f, "{} name error: conflicting class declaration '{}'", t.span.start, t.text),
            NameError::InheritanceCycle(ref t, ref e) => write!(f, "{} name error: cyclic inheritance at '{} extends {}'", t.span.start, t.text, e.text),
            NameError::OverloadedFunction(ref t) => write!(f, "{} name error: overloaded function '{}'", t.span.start, t.text),
            NameError::OverrideMismatch(ref t, ref actual, ref expected) => {
                write!(f, "{} name error: function '{}' has {} argument{} but overrides a function with {} argument{}",
                    t.span.start, t.text,
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
    pub id: String,
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

    pub fn analyze(&mut self, program: &Rc<Program>) {
        info!("Performing name analysis");

        let mut generator = SymbolVisitor::new(program);
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
    program: Rc<Program>,
    global_env: Rc<Environment>,
    errors: Vec<Error>,
    in_main: bool,
    kind: PhantomData<T>,
}

impl SymbolVisitor<Generator> {
    fn new(program: &Rc<Program>) -> SymbolVisitor<Generator> {
        SymbolVisitor {
            symbol_count: 0,
            program: program.clone(),
            global_env: Environment::new(),
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
        let SymbolVisitor { symbol_count, program, global_env, in_main, errors, .. } = generator;
        SymbolVisitor { symbol_count, program, global_env, errors, in_main, kind: PhantomData }
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

            // Link classes to their superclasses.
            if let Some(ref extends) = class.extends {

                // Search for a class whose identifier matches the one to extend.
                let superclass = program.classes.iter().find(|other_class| other_class.id == *extends);
                match superclass {
                    None => warn!("Didn't find superclass for '{}'", &class.id.text),
                    Some(superclass) => {
                        class.set_superclass(superclass);
                    }
                }
            }
        }
    }
}

impl Visitor<Rc<Class>> for SymbolVisitor<Generator> {
    fn visit(&mut self, class: Rc<Class>) {
        debug!("Generating symbols for class '{}'", &class.id.text);

        // Check if a class has already been defined with this name
        if self.global_env.get(&class.id).is_some() {
            warn!("Found conflicting class {}", &class.id.text);
            self.errors.push(NameError::conflicting_class(&class.id).into());
        }

        // Create a new symbol representing this class
        let class_symbol = self.make_symbol(&class.id);
        self.global_env.define(&class.id, &class_symbol);

        // Create an environment for this class extending the global environment
        let class_env = Environment::extending(&self.global_env);
        class.set_env(&class_env);

        // Process the variables in this class
        for var in class.variables.iter() {
            debug!("Generating symbol for variable '{}'", &var.name.text);

            // If a variable with this name was already declared, give an error.
            if class_env.get(&var.name).is_some() {
                self.errors.push(NameError::conflicting_variable(&var.name).into());
            } else {
                let var_symbol = self.make_symbol(&var.name);
                class_env.define(&var.name, &var_symbol);
            }
        }

        // Process the functions in this class
        for func in class.functions.iter() {

            // Create an environment for this function that extends the class environment.
            func.set_env(&Environment::extending(&class_env));
            self.visit(func.clone());
        }
    }
}

impl Visitor<Rc<Class>> for SymbolVisitor<Linker> {
    fn visit(&mut self, class: Rc<Class>) {
        debug!("Linking symbols for class '{}'", &class.id.text);
        let class_env = class.get_env().expect("Every class should have an environment");

        // If this class extends another, find the environment of the class it extends.
        if let Some(ref extends) = class.extends {

            // Get the extended class symbol from the global environment.
            let extending = self.global_env.get(extends).expect("Extended class should have a symbol in global env");
            extends.set_symbol(&extending);

            match self.program.get_class(&extending) {
                // If we don't find a scope for the extended class, give an error.
                None => self.errors.push(NameError::extending_undelcared(extends.clone()).into()),
                // If we find the extended class scope, link this scope to it.
                Some(ref extended) => {
                    let super_env = extended.get_env().expect("Super class should have an environment");
                    class_env.set_super(&super_env);
                }
            }
        }

        // Check for cyclic inheritance
        if class_env.cycle() {
            self.errors.push(NameError::inheritance_cycle(&class.id, &class.extends.as_ref().unwrap()).into())
        }

        let super_env = class_env.get_super().expect("Every class env should extend another env");
        for var in class.variables.iter() {
            // Link this variable to the class it's declared in (this one). Needed for codegen.
            var.set_class(&class);

            // If a variable with this name exists in a super scope, give an error.
            if let Some(_) = super_env.get(&var.name) {
                self.errors.push(NameError::variable_override(&var.name).into());
            }
        }

        for func in class.functions.iter() {
            self.visit((class.clone(), func.clone()));
        }
    }
}

impl Visitor<Rc<Function>> for SymbolVisitor<Generator> {
    fn visit(&mut self, function: Rc<Function>) {
        debug!("Generating symbols in function '{}'", function.name.text);

        // Get the environment of this function.
        let func_env = function.get_env().expect("Function should have an environment");
        let class_env = func_env.get_super().expect("Every function env extends a class env");

        // Check for overloaded functions
        if class_env.get(&function.name).is_some() {
            self.errors.push(NameError::overloaded_function(&function.name).into());
        }

        // Create a unique symbol for this function.
        let func_symbol = self.make_symbol(&function.name);
        class_env.define(&function.name, &func_symbol);

        // Create new symbols for each argument and add them to the function scope.
        for arg in function.args.iter() {

            // Check if any arguments in this local scope have this name.
            if func_env.bindings.borrow().contains_key(&arg.name) {
                self.errors.push(NameError::conflicting_variable(&arg.name).into());
                continue;
            }

            // Create a unique symbol for this arg.
            let arg_symbol = self.make_symbol(&arg.name);
            func_env.define(&arg.name, &arg_symbol);
        }

        // Create new symbols for each variable and add them to the function scope.
        for var in function.variables.iter() {

            // Check if any arguments in this local scope have this name.
            if let Some(ref symbol) = func_env.bindings.borrow().get(&var.name) {
                self.errors.push(NameError::conflicting_variable(&var.name).into());

                // Just pretend there wasn't a duplicate definition. Give this var the same
                // symbol as the clashing variable definition so subsequent stages run without error.
                var.name.set_symbol(&symbol.clone());
                continue;
            }

            // Create a unique symbol for this var.
            let var_symbol = self.make_symbol(&var.name);
            func_env.define(&var.name, &var_symbol);
        }

        // Attach the function environment to every statement.
        for stmt in function.statements.iter() {
            self.visit((stmt.clone(), func_env.clone()));
        }

        // Attach the function environment to the return statement.
        if let Some(ref return_expr) = function.expression {
            self.visit((return_expr.clone(), func_env.clone()));
        }
    }
}

impl Visitor<(Rc<Class>, Rc<Function>)> for SymbolVisitor<Linker> {
    fn visit(&mut self, (class, function): (Rc<Class>, Rc<Function>)) {
        debug!("Linking symbols in function '{}'", &function.name.text);

        let func_env = function.get_env().expect("Function should have an environment");
        let class_env = func_env.get_super().expect("Each function env should extend a class env");

        // Check if this function overrides another function.
        let class_super_env = class_env.get_super().expect("Each class extends an env");
        if let Some(ref symbol) = class_super_env.get(&function.name) {
            // Look up the symbol we found in the symbol_table.
            match class.get_function_by_symbol(symbol) {
                None => {
//                    self.errors.push(format_err!("unknown error: function has the same name as a non-function symbol in scope"));
                },
                Some(ref other_func) => {
                    let other_len = other_func.args.len();
                    let my_len = function.args.len();
                    if other_len != my_len {
                        self.errors.push(NameError::override_mismatch(&function.name, my_len, other_len).into());
                    }
                }
            }
        }

        for arg in function.args.iter() {
            if let Type::Id(ref id) = *arg.kind {
                self.visit((id.clone(), func_env.clone()));
            }
        }
        for var in function.variables.iter() {
            if let Type::Id(ref id) = *var.kind {
                self.visit((id.clone(), func_env.clone()));
            }
        }
        for stmt in function.statements.iter() {
            self.visit(stmt.clone());
        }
        if let Some(ref return_expr) = function.expression {
            self.visit(return_expr.clone());
        }
    }
}

impl Visitor<(Rc<Statement>, Rc<Environment>)> for SymbolVisitor<Generator> {
    fn visit(&mut self, (stmt, env): (Rc<Statement>, Rc<Environment>)) {
        stmt.set_env(&env);
        match **stmt {
            Stmt::Assign { ref rhs, .. } => {
                debug!("Set 'assign' statement env");
                self.visit((rhs.clone(), env.clone()));
            }
            Stmt::AssignArray { ref index, ref rhs, .. } => {
                debug!("Set 'assign-array' statement env");
                self.visit((index.clone(), env.clone()));
                self.visit((rhs.clone(), env.clone()));
            }
            Stmt::SideEffect { ref expression, .. } => {
                debug!("Set 'sidef' statement env");
                self.visit((expression.clone(), env.clone()));
            }
            Stmt::Block { ref statements, .. } => {
                debug!("Set 'block' statement env");
                for stmt in statements.iter() {
                    self.visit((stmt.clone(), env.clone()))
                }
            }
            Stmt::Print { ref expression, .. } => {
                debug!("Set 'print' statement env");
                self.visit((expression.clone(), env.clone()));
            }
            Stmt::While { ref expression, ref statement, .. } => {
                debug!("Set 'while' statement env");
                self.visit((expression.clone(), env.clone()));
                self.visit((statement.clone(), env.clone()));
            }
            Stmt::If { ref condition, ref statement, ref otherwise, .. } => {
                debug!("Set 'if' statement env");
                self.visit((condition.clone(), env.clone()));
                self.visit((statement.clone(), env.clone()));
                if let Some(otherwise) = otherwise {
                    self.visit((otherwise.clone(), env.clone()));
                }
            }
        }
    }
}

impl Visitor<Rc<Statement>> for SymbolVisitor<Linker> {
    fn visit(&mut self, statement: Rc<Statement>) {
        let env = statement.get_env().expect("Statement should have an environment");
        match **statement {
            Stmt::Assign { ref lhs, ref rhs, .. } => {
                // Check that the left-hand identifier maps to a symbol in the environment.
                self.visit((lhs.clone(), env));
                self.visit(rhs.clone());
            }
            Stmt::AssignArray { ref lhs, ref index, ref rhs, .. } => {
                self.visit((lhs.clone(), env));
                self.visit(index.clone());
                self.visit(rhs.clone());
            }
            Stmt::SideEffect { ref expression, .. } => {
                self.visit(expression.clone());
            }
            Stmt::Block { ref statements, .. } => {
                for stmt in statements.iter() {
                    self.visit(stmt.clone());
                }
            }
            Stmt::Print { ref expression, .. } => {
                self.visit(expression.clone());
            }
            Stmt::While { ref expression, ref statement, .. } => {
                self.visit(expression.clone());
                self.visit(statement.clone());
            }
            Stmt::If { ref condition, ref statement, ref otherwise, .. } => {
                self.visit(condition.clone());
                self.visit(statement.clone());
                if let Some(otherwise) = otherwise.as_ref() {
                    self.visit(otherwise.clone());
                }
            }
        }
    }
}

impl Visitor<(Rc<Expression>, Rc<Environment>)> for SymbolVisitor<Generator> {
    fn visit(&mut self, (expr, env): (Rc<Expression>, Rc<Environment>)) {
        expr.set_env(&env);
        match **expr {
            Expr::Unary(ref unary) => {
                match *unary {
                    UnaryExpression::NewArray(ref expression) => {
                        self.visit((expression.clone(), env.clone()));
                    }
                    UnaryExpression::Not(ref expression) => {
                        self.visit((expression.clone(), env.clone()));
                    }
                    UnaryExpression::Parentheses(ref expression) => {
                        self.visit((expression.clone(), env.clone()));
                    }
                    UnaryExpression::Length(ref expression) => {
                        self.visit((expression.clone(), env.clone()));
                    }
                    UnaryExpression::Application { ref expression, ref list, .. } => {
                        self.visit((expression.clone(), env.clone()));
                        for expr in list.iter() {
                            self.visit((expr.clone(), env.clone()));
                        }
                    }
                    UnaryExpression::ArrayLookup { ref lhs, ref index, .. } => {
                        self.visit((lhs.clone(), env.clone()));
                        self.visit((index.clone(), env.clone()));
                    }
                }
            },
            Expr::Binary(ref binary) => {
                self.visit((binary.lhs.clone(), env.clone()));
                self.visit((binary.rhs.clone(), env.clone()));
            },
            _ => ()
        }
    }
}

impl Visitor<Rc<Expression>> for SymbolVisitor<Linker> {
    fn visit(&mut self, expression: Rc<Expression>) {
        let env = expression.get_env().expect("Expression should have an environment");
        match **expression {
            Expr::Identifier(ref id) => { self.visit((id.clone(), env)) }
            Expr::NewClass(ref id) => { self.visit((id.clone(), env)) }
            Expr::Unary(ref unary) => self.visit(unary),
            Expr::Binary(ref binary) => self.visit(binary),
            Expr::This => {
                // If we're in the main function, 'this' is illegal, give an error.
                if self.in_main {
                    self.errors.push(NameError::static_this().into());
                }
            }
            _ => (),
        }
    }
}

impl<'a> Visitor<&'a UnaryExpression> for SymbolVisitor<Linker> {
    fn visit(&mut self, unary: &'a UnaryExpression) {
        match *unary {
            UnaryExpression::NewArray(ref expr) => self.visit(expr.clone()),
            UnaryExpression::Not(ref expr) => self.visit(expr.clone()),
            UnaryExpression::Parentheses(ref expr) => self.visit(expr.clone()),
            UnaryExpression::Length(ref expr) => self.visit(expr.clone()),
            UnaryExpression::Application { ref expression, ref id, ref list, .. } => {
                self.visit(expression.clone());

                // In this phase, leave function identifiers unresolved.
                id.set_symbol(&Symbol::unresolved(id));

                for expr in list.iter() {
                    self.visit(expr.clone());
                }
            }
            UnaryExpression::ArrayLookup { ref lhs, ref index, .. } => {
                self.visit(lhs.clone());
                self.visit(index.clone());
            }
        }
    }
}

impl<'a> Visitor<&'a BinaryExpression> for SymbolVisitor<Linker> {
    fn visit(&mut self, binary: &'a BinaryExpression) {
        self.visit(binary.lhs.clone());
        self.visit(binary.rhs.clone());
    }
}

impl Visitor<(Rc<Identifier>, Rc<Environment>)> for SymbolVisitor<Linker> {
    fn visit(&mut self, (id, env): (Rc<Identifier>, Rc<Environment>)) {
        debug!("Linking identifier '{}'", &id.text);
        match env.get(&id) {
            Some(ref symbol) => id.set_symbol(symbol),
            None => self.errors.push(NameError::using_undeclared(id).into()),
        }
    }
}
