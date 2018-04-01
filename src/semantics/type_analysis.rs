#![allow(warnings)]

use std::rc::Rc;
use std::fmt::{
    Display,
    Formatter,
    Result as fmtResult,
};
use std::marker::PhantomData;
use failure::Error;

use lexer::OwnedToken;
use syntax::ast::*;
use syntax::visitor::Visitor;
use semantics::{
    Symbol,
    SymbolTable,
};

#[derive(Debug, Fail)]
pub enum TypeError {
    OverrideTypeMismatch(OwnedToken),
}

impl Display for TypeError {
    fn fmt(&self, f: &mut Formatter) -> fmtResult {
        match *self {
            TypeError::OverrideTypeMismatch(ref t) => write!(f, "{}:{} type error: function '{}' overrides a function with a different type", t.line, t.column, t.text),
        }
    }
}

impl TypeError {
    fn override_type_mismatch<T: Into<OwnedToken>>(token: T) -> TypeError {
        TypeError::OverrideTypeMismatch(token.into())
    }
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum AtomType {
    Void,
    Int,
    IntArray,
    String,
    StringArray,
    Boolean,
    Class(Rc<Symbol>),
}

impl Display for AtomType {
    fn fmt(&self, f: &mut Formatter) -> fmtResult {
        match *self {
            AtomType::Void => write!(f, "void"),
            AtomType::Int => write!(f, "int"),
            AtomType::IntArray => write!(f, "int[]"),
            AtomType::String => write!(f, "String"),
            AtomType::StringArray => write!(f, "String[]"),
            AtomType::Boolean => write!(f, "boolean"),
            AtomType::Class(ref symbol) => symbol.name.fmt(f),
        }
    }
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct FunctionType {
    inputs: Vec<AtomType>,
    output: AtomType,
}

impl Display for FunctionType {
    fn fmt(&self, f: &mut Formatter) -> fmtResult {
        if self.inputs.len() == 0 { write!(f, "()"); }
        for (i, input) in self.inputs.iter().enumerate() {
            if i != 0 { write!(f, " x "); }
            input.fmt(f);
        }

        write!(f, " -> {}", self.output)
    }
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum SymbolType {
    Atom(AtomType),
    Function(FunctionType),
}

impl Display for SymbolType {
    fn fmt(&self, f: &mut Formatter) -> fmtResult {
        match *self {
            SymbolType::Atom(ref atom) => atom.fmt(f),
            SymbolType::Function(ref function) => function.fmt(f),
        }
    }
}

impl From<AtomType> for SymbolType {
    fn from(atom: AtomType) -> Self { SymbolType::Atom(atom) }
}

impl From<FunctionType> for SymbolType {
    fn from(func: FunctionType) -> Self { SymbolType::Function(func) }
}

impl<T: AsRef<Type>> From<T> for AtomType {
    fn from(kind: T) -> Self {
        match *kind.as_ref() {
            Type::Void => AtomType::Void,
            Type::Int => AtomType::Int,
            Type::IntArray => AtomType::IntArray,
            Type::String => AtomType::String,
            Type::StringArray => AtomType::StringArray,
            Type::Boolean => AtomType::Boolean,
            Type::Id(ref id) => AtomType::Class(id.get_symbol().expect("Class types should have symbols")),
        }
    }
}

impl<T: AsRef<Type>> From<T> for SymbolType {
    fn from(kind: T) -> Self {
        SymbolType::Atom(AtomType::from(kind))
    }
}

pub struct TypeChecker {
    pub errors: Vec<Error>,
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker {
            errors: vec![]
        }
    }

    pub fn analyze(&self, program: &Rc<Program>, symbol_table: SymbolTable) {
        let mut assigner = TypeVisitor::new(symbol_table);
        assigner.visit(program.clone());

        let mut checker: TypeVisitor<Checker> = assigner.into();
        checker.visit(program.clone());
    }
}

enum Assigner {}

enum Checker {}

struct TypeVisitor<T> {
    symbol_table: SymbolTable,
    errors: Vec<Error>,
    phase: PhantomData<T>,
}

impl TypeVisitor<Assigner> {
    fn new(symbol_table: SymbolTable) -> TypeVisitor<Assigner> {
        TypeVisitor { symbol_table, errors: vec![], phase: PhantomData }
    }
}

impl From<TypeVisitor<Assigner>> for TypeVisitor<Checker> {
    fn from(assigner: TypeVisitor<Assigner>) -> Self {
        let TypeVisitor { symbol_table, errors, .. } = assigner;
        TypeVisitor { symbol_table, errors, phase: PhantomData }
    }
}

impl Visitor<Rc<Program>> for TypeVisitor<Assigner> {
    fn visit(&mut self, program: Rc<Program>) {
        self.visit(program.main.clone());
        for class in program.classes.iter() {
            self.visit(class.clone());
        }
    }
}

impl Visitor<Rc<Program>> for TypeVisitor<Checker> {
    fn visit(&mut self, program: Rc<Program>) {
        self.visit(program.main.clone());
        for class in program.classes.iter() {
            self.visit(class.clone());
        }
    }
}

impl Visitor<Rc<Class>> for TypeVisitor<Assigner> {
    fn visit(&mut self, class: Rc<Class>) {

        // Assign member variable types
        for var in class.variables.iter() {
            let var_symbol = var.name.get_symbol().expect("Every member variable should have a symbol");
            var_symbol.set_type(&var.kind);
        }

        // Assign function types
        for func in class.functions.iter() {
            let func_symbol = func.name.get_symbol().expect("Every function should have a symbol");
            let output = AtomType::from(&func.kind);
            let inputs = func.args.iter().map(|arg| AtomType::from(&arg.kind)).collect();
            let func_type = FunctionType { inputs, output };
            func_symbol.set_type(func_type);
        }

        // Assign types for function arguments and variables
        for func in class.functions.iter() {
            self.visit(func.clone());
        }
    }
}

impl Visitor<Rc<Class>> for TypeVisitor<Checker> {
    fn visit(&mut self, class: Rc<Class>) {
        let class_env = class.get_env().expect("Every class should have a scope");
        for func in class.functions.iter() {
            self.visit(func.clone());
        }
    }
}

impl Visitor<Rc<Function>> for TypeVisitor<Assigner> {
    fn visit(&mut self, func: Rc<Function>) {

        // Assign argument types
        for arg in func.args.iter() {
            let arg_symbol = arg.name.get_symbol().expect("Every argument should have a symbol");
            arg_symbol.set_type(&arg.kind);
        }

        // Assign local variable types
        for var in func.variables.iter() {
            debug!("Assigning variable type for {}", &var.name.text);
            let var_symbol = var.name.get_symbol().expect("Every local variable should have a symbol");
            var_symbol.set_type(&var.kind);
        }
    }
}

impl Visitor<Rc<Function>> for TypeVisitor<Checker> {
    fn visit(&mut self, func: Rc<Function>) {

        let func_env = func.get_env().expect("Every function should have an environment");

        unimplemented!()
    }
}
