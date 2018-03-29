#![allow(warnings)]

use std::rc::Rc;
use std::fmt::{
    Display,
    Formatter,
    Error as fmtError,
};
use std::marker::PhantomData;
use failure::Error;

use syntax::ast::*;
use syntax::visitor::Visitor;
use semantics::{
    Symbol,
    GlobalScope,
};

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
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmtError> {
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
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmtError> {
        if self.inputs.len() == 0 { write!(f, "()"); }
        for (i, input) in self.inputs.iter().enumerate() {
            if i != 0 { write!(f, " x "); }
            input.fmt(f);
        }

        write!(f, ": {}", self.output)
    }
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum SymbolType {
    Atom(AtomType),
    Function(FunctionType),
}

impl Display for SymbolType {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmtError> {
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
            Type::Int => AtomType::Int,
            Type::IntArray => AtomType::IntArray,
            Type::String => AtomType::String,
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

    pub fn analyze(&self, program: &Rc<Program>, global_scope: Rc<GlobalScope>) {
        let mut assigner = TypeVisitor::new(global_scope);
        assigner.visit(program.clone());

        let mut checker: TypeVisitor<Checker> = assigner.into();
        checker.visit(program.clone());
    }
}

enum Assigner { }
enum Checker { }

struct TypeVisitor<T> {
    global_scope: Rc<GlobalScope>,
    errors: Vec<Error>,
    phase: PhantomData<T>,
}

impl TypeVisitor<Assigner> {
    fn new(global_scope: Rc<GlobalScope>) -> TypeVisitor<Assigner> {
        TypeVisitor { global_scope, errors: vec![], phase: PhantomData }
    }
}

impl From<TypeVisitor<Assigner>> for TypeVisitor<Checker> {
    fn from(assigner: TypeVisitor<Assigner>) -> Self {
        let TypeVisitor { global_scope, errors, .. } = assigner;
        TypeVisitor { global_scope, errors, phase: PhantomData }
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
//        self.visit(program.main.clone());
        for class in program.classes.iter() {
            self.visit(class.clone());
        }
    }
}

impl Visitor<Rc<Main>> for TypeVisitor<Assigner> {
    fn visit(&mut self, main: Rc<Main>) {
        let main_symbol = main.func.get_symbol().expect("Main function should have a symbol");
        let main_type = FunctionType {
            inputs: vec![ AtomType::StringArray ],
            output: AtomType::Void,
        };
        main_symbol.set_type(main_type);
    }
}

impl Visitor<Rc<Main>> for TypeVisitor<Checker> {
    fn visit(&mut self, main: Rc<Main>) {
        unimplemented!()
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
        unimplemented!()
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
            let var_symbol = var.name.get_symbol().expect("Every local variable should have a symbol");
            var_symbol.set_type(&var.kind);
        }
    }
}
