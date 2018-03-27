use std::rc::Rc;
use failure::Error;

use syntax::ast::*;
use syntax::visitor::Visitor;

pub enum AtomType {
    Int,
    IntArray,
    String,
    StringArray,
    Class(Rc<Symbol>),
}

pub struct FunctionType {
    args: Vec<AtomType>,
    ret: AtomType,
}

pub enum SymbolType {
    Atom(AtomType),
    Function(FunctionType),
}

pub struct TypeChecker {
    _errors: Vec<Error>,
}
