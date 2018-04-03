use std::rc::Rc;
use std::fmt::{
    Display,
    Formatter,
    Result as fmtResult,
};
use failure::Error;

use lexer::OwnedToken;
use syntax::ast::*;
use semantics::{
    Symbol,
    SymbolTable,
};

#[derive(Debug, Fail)]
pub enum TypeError {
    OverrideTypeMismatch(OwnedToken),
    InvalidOperandType(String, String),
    ComparisonMismatch(String, String),
    NonArrayLength(String),
    NonIntIndexing(String),
    VariableAssignMismatch(String, String),
}

impl Display for TypeError {
    fn fmt(&self, f: &mut Formatter) -> fmtResult {
        match *self {
            TypeError::OverrideTypeMismatch(ref t) => write!(f, "{}:{} type error: function '{}' overrides a function with a different type", t.line, t.column, t.text),
            TypeError::InvalidOperandType(ref operator, ref operand) => write!(f, "type error: operator {} got operand with invalid type {}", operator, operand),
            TypeError::ComparisonMismatch(ref lhs, ref rhs) => write!(f, "type error: cannot compare type '{}' with '{}'", lhs, rhs),
            TypeError::NonArrayLength(ref non_array) => write!(f, "type error: cannot apply '.length' to non-array type '{}'", non_array),
            TypeError::NonIntIndexing(ref non_index) => write!(f, "type error: array index must be an int, got a '{}'", non_index),
            TypeError::VariableAssignMismatch(ref var_type, ref expr_type) => write!(f, "type error: expression should match variable type '{}', got '{}'", var_type, expr_type),
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
    ClassArray(Rc<Symbol>),
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
            AtomType::ClassArray(ref symbol) => write!(f, "{}[]", symbol.name),
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
    _symbol_table: SymbolTable,
    pub errors: Vec<Error>,
}

impl TypeChecker {
    pub fn new(symbol_table: SymbolTable) -> TypeChecker {
        TypeChecker {
            _symbol_table: symbol_table,
            errors: vec![]
        }
    }

    pub fn analyze(&mut self, program: &Rc<Program>) {
        self.assign_program(program.clone());
        self.check_program(program.clone());
    }

    fn assign_program(&mut self, program: Rc<Program>) {
        self.assign_class(program.main.clone());
        for class in program.classes.iter() {
            self.assign_class(class.clone());
        }
    }

    fn assign_class(&mut self, class: Rc<Class>) {
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
            self.assign_function(func.clone());
        }
    }

    fn assign_function(&mut self, func: Rc<Function>) {
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

    fn check_program(&mut self, program: Rc<Program>) {
        self.check_class(&program.main);
        for class in program.classes.iter() {
            self.check_class(class);
        }
    }

    fn check_class(&mut self, class: &Rc<Class>) {
        for func in class.functions.iter() {
            self.check_function(class, func);
        }
    }

    fn check_function(&mut self, class: &Rc<Class>, func: &Rc<Function>) {
        debug!("Type checking function '{}'", &func.name.text);

        // Get the function's symbol and type.
        let func_symbol = func.get_symbol().expect("Each function should have a symbol");
        let func_type = func_symbol.get_type().expect("Each function should have a type");

        // If this function overrides another, check that they have the same type.
        let func_env = func.get_env().expect("Every function should have an environment");
        let class_env = func_env.get_super().expect("Every function env should have a class env");
        let class_super_env = class_env.get_super().expect("Every class env should have a super env");
        if let Some(ref other_func) = class_super_env.get(&func.name) {
            let other_type = other_func.get_type().expect("Each function should have a type");
            if other_type != func_type {
                self.errors.push(TypeError::override_type_mismatch(&func.name).into());
            }
        }

        // Typecheck each statement
        for stmt in func.statements.iter() {
            self.check_statement(class, func, stmt);
        }
    }

    fn check_statement(&mut self, class: &Rc<Class>, func: &Rc<Function>, stmt: &Rc<Statement>) {
        match ***stmt {
            Stmt::Block { ref statements, .. } => {
                for stmt in statements.iter() {
                    self.check_statement(class, func, stmt);
                }
            }
            Stmt::While { ref expression, ref statement, .. } => {
                self.check_expression(class, func, expression);
                self.check_statement(class, func, statement);
            }
            Stmt::Print { ref expression, .. } => {
                self.check_expression(class, func, expression);
            }
            Stmt::Assign { ref lhs, ref rhs, .. } => {
                // Get the type of the receiving variable
                if let Some(var_symbol) = lhs.get_symbol() {
                    // Assert that the rhs expression type matches the variable type.
                    let var_type = var_symbol.get_type().expect("Every symbol should have a type");
                    let expr_type = self.check_expression(class, func, rhs);
                    // TODO check that expr is a _subtype_ of var type rather than equal.
                    if expr_type != var_type {
                        self.errors.push(TypeError::VariableAssignMismatch(format!("{}", var_type), format!("{}", expr_type)).into());
                    }
                }
            }
            _ => ()
        }
    }

    fn check_expression(&mut self, class: &Rc<Class>, func: &Rc<Function>, expr: &Rc<Expression>) -> SymbolType {
        match ***expr {
            Expr::TrueLiteral => AtomType::Boolean.into(),
            Expr::FalseLiteral => AtomType::Boolean.into(),
            Expr::Unary(ref unary) => {
                match *unary {
                    UnaryExpression::Parentheses(ref inner_expr) => {
                        self.check_expression(class, func, inner_expr)
                    }
                    UnaryExpression::Not(ref inner_expr) => {
                        // "Not" should only be applied to booleans
                        let inner_type = self.check_expression(class, func, inner_expr);
                        if inner_type != AtomType::Boolean.into() {
                            self.errors.push(TypeError::InvalidOperandType("NOT (!)".to_owned(), format!("{}", inner_type)).into());
                        }
                        AtomType::Boolean.into()
                    }
                    UnaryExpression::Length(ref inner_expr) => {
                        // Assert that the expression is an array type.
                        let inner_type = self.check_expression(class, func, inner_expr);
                        match inner_type {
                            // If the type is one of these, everything is fine.
                            SymbolType::Atom(AtomType::IntArray)
                            | SymbolType::Atom(AtomType::StringArray)
                            | SymbolType::Atom(AtomType::ClassArray(_)) => (),
                            _ => self.errors.push(TypeError::NonArrayLength(format!("{}", inner_type)).into()),
                        }
                        // Regardless of whether it's applied to a valid expression,
                        // .length always returns an integer.
                        AtomType::Int.into()
                    }
                    UnaryExpression::NewArray(ref inner_expr) => {
                        // Assert that the expression in brackets is an integer.
                        let inner_type = self.check_expression(class, func, inner_expr);
                        if inner_type != AtomType::Int.into() {
                            self.errors.push(TypeError::NonIntIndexing(format!("{}", inner_type)).into());
                        }
                        AtomType::IntArray.into()
                    }
                    UnaryExpression::Application { ref id, .. } => {
                        // Get the function type of "id" from the environment.
                        let env = expr.get_env().expect("Every expression should have an env");
                        let _func_symbol = env.get(id);
                        unimplemented!()
                    }
                }
            }
            Expr::Binary(ref binary) => {
                let lhs_type: SymbolType = self.check_expression(class, func, &binary.lhs);
                let rhs_type: SymbolType = self.check_expression(class, func, &binary.rhs);

                // Enforce type rules based on which operator this is.
                match binary.kind {
                    // Assert that these operators are given (int x int)
                    BinaryKind::Divide
                    | BinaryKind::Minus
                    | BinaryKind::Times => {
                        if lhs_type != AtomType::Int.into()
                            || rhs_type != AtomType::Int.into() {
                            self.errors.push(TypeError::InvalidOperandType(format!("{}", binary.kind), format!("{}", lhs_type)).into());
                        }
                        AtomType::Int.into()
                    }
                    BinaryKind::Plus => {
                        // Assert that the operands are either int or String.
                        if lhs_type != AtomType::Int.into() || lhs_type != AtomType::String.into() {
                            self.errors.push(TypeError::InvalidOperandType(format!("{}", binary.kind), format!("{}", lhs_type)).into());
                        }
                        if rhs_type != AtomType::Int.into() || rhs_type != AtomType::String.into() {
                            self.errors.push(TypeError::InvalidOperandType(format!("{}", binary.kind), format!("{}", lhs_type)).into());
                        }

                        // If at least one of the operands is a String, expression is a String.
                        // Otherwise, it must be an int.
                        if lhs_type == AtomType::String.into() || rhs_type == AtomType::String.into() {
                            AtomType::String.into()
                        } else {
                            AtomType::Int.into()
                        }
                    }
                    BinaryKind::Equals => {
                        match (&lhs_type, &rhs_type) {
                            (SymbolType::Atom(AtomType::Int), SymbolType::Atom(AtomType::Int)) => (),
                            (SymbolType::Atom(AtomType::String), SymbolType::Atom(AtomType::String)) => (),
                            (SymbolType::Atom(AtomType::Class(_)), SymbolType::Atom(AtomType::Class(_))) => (),
                            _ => self.errors.push(TypeError::ComparisonMismatch(format!("{}", lhs_type), format!("{}", rhs_type)).into()),
                        }

                        AtomType::Boolean.into()
                    }
                    _ => unimplemented!()
                }
            }
            _ => unimplemented!()
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_type_eq() {
        assert_ne!(AtomType::Int, AtomType::String);
        assert_ne!(
            FunctionType {
                inputs: vec![AtomType::Int],
                output: AtomType::Int,
            },
            FunctionType {
                inputs: vec![AtomType::String],
                output: AtomType::Int,
            },
        )
    }
}
