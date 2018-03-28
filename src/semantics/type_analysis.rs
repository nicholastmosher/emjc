use std::rc::Rc;
use std::fmt::{
    Display,
    Formatter,
    Error as fmtError,
};
use failure::Error;

use semantics::Symbol;

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum AtomType {
    Int,
    IntArray,
    String,
    StringArray,
    Class(Rc<Symbol>),
}

impl Display for AtomType {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmtError> {
        match *self {
            AtomType::Int => write!(f, "int"),
            AtomType::IntArray => write!(f, "int[]"),
            AtomType::String => write!(f, "String"),
            AtomType::StringArray => write!(f, "String[]"),
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

pub struct TypeChecker {
    _errors: Vec<Error>,
}
