use std::rc::Rc;
use std::fmt::{
    Display,
    Formatter,
    Result as fmtResult,
};
use failure::Error;

use lexer::OwnedToken;
use syntax::ast::*;
use semantics::Symbol;

#[derive(Debug, Fail)]
pub enum TypeError {
    OverrideTypeMismatch(OwnedToken),
    InvalidOperandType(String, String),
    EqMismatch(String, String),
    LtMismatch(String, String),
    AndMismatch(String, String),
    OrMismatch(String, String),
    NonArrayLength(String),
    NonIntIndexing(String),
    VariableAssignMismatch(String, String),
    AssignArray(String, String),
    ArrayAccess(String),
    NewNonClass(String),
    PrintType(String),
    ConditionType(String),
}

impl Display for TypeError {
    fn fmt(&self, f: &mut Formatter) -> fmtResult {
        match *self {
            TypeError::OverrideTypeMismatch(ref t) => write!(f, "{} type error: function '{}' overrides a function with a different type", t.span.start, t.text),
            TypeError::InvalidOperandType(ref operator, ref operand) => write!(f, "type error: operator {} got operand with invalid type {}", operator, operand),
            TypeError::EqMismatch(ref lhs, ref rhs) => write!(f, "type error: cannot compare (==) type '{}' with '{}'", lhs, rhs),
            TypeError::LtMismatch(ref lhs, ref rhs) => write!(f, "type error: cannot compare (<) type '{}' with '{}'", lhs, rhs),
            TypeError::AndMismatch(ref lhs, ref rhs) => write!(f, "type error: cannot AND (&&) type '{}' with '{}'", lhs, rhs),
            TypeError::OrMismatch(ref lhs, ref rhs) => write!(f, "type error: cannot OR (||) type '{}' with '{}'", lhs, rhs),
            TypeError::NonArrayLength(ref non_array) => write!(f, "type error: cannot apply '.length' to non-array type '{}'", non_array),
            TypeError::NonIntIndexing(ref non_index) => write!(f, "type error: array index must be an int, got a '{}'", non_index),
            TypeError::VariableAssignMismatch(ref var_type, ref expr_type) => write!(f, "type error: expression should match variable type '{}', got '{}'", var_type, expr_type),
            TypeError::AssignArray(ref assign, ref array) => write!(f, "type error: cannot insert type '{}' to array type '{}'", assign, array),
            TypeError::ArrayAccess(ref non_array) => write!(f, "type error: cannot index into non-array type '{}'", non_array),
            TypeError::NewNonClass(ref kind) => write!(f, "type error: cannot use 'new' operator with identifier '{}'", kind),
            TypeError::PrintType(ref kind) => write!(f, "type error: print statement takes 'int', 'String', or 'boolean', got '{}'", kind),
            TypeError::ConditionType(ref kind) => write!(f, "type error: condition must be type 'boolean', got '{}'", kind),
        }
    }
}

impl TypeError {
    fn override_type_mismatch<T: Into<OwnedToken>>(token: T) -> TypeError {
        TypeError::OverrideTypeMismatch(token.into())
    }
}

#[derive(Debug, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum SymbolType {
    Void,
    Int,
    IntArray,
    String,
    StringArray,
    Boolean,
    Class(Rc<Symbol>),
    ClassArray(Rc<Symbol>),
    Function {
        inputs: Vec<SymbolType>,
        output: Rc<SymbolType>,
    },
}

impl Display for SymbolType {
    fn fmt(&self, f: &mut Formatter) -> fmtResult {
        match *self {
            SymbolType::Void => write!(f, "void"),
            SymbolType::Int => write!(f, "int"),
            SymbolType::IntArray => write!(f, "int[]"),
            SymbolType::String => write!(f, "String"),
            SymbolType::StringArray => write!(f, "String[]"),
            SymbolType::Boolean => write!(f, "boolean"),
            SymbolType::Class(ref symbol) => symbol.name.fmt(f),
            SymbolType::ClassArray(ref symbol) => write!(f, "{}[]", symbol.name),
            SymbolType::Function { ref inputs, ref output, .. } => {
                if inputs.len() == 0 { write!(f, "()"); }
                for (i, input) in inputs.iter().enumerate() {
                    if i != 0 { write!(f, " x "); }
                    input.fmt(f);
                }
                write!(f, " -> {}", output)
            },
        }
    }
}

impl<T: AsRef<Type>> From<T> for SymbolType {
    fn from(kind: T) -> Self {
        match *kind.as_ref() {
            Type::Void => SymbolType::Void,
            Type::Int => SymbolType::Int,
            Type::IntArray => SymbolType::IntArray,
            Type::String => SymbolType::String,
            Type::StringArray => SymbolType::StringArray,
            Type::Boolean => SymbolType::Boolean,
            Type::Id(ref id) => SymbolType::Class(id.get_symbol().expect("Class types should have symbols")),
        }
    }
}

pub struct TypeChecker {
    program: Rc<Program>,
    pub errors: Vec<Error>,
}

impl TypeChecker {
    pub fn new(program: &Rc<Program>) -> TypeChecker {
        TypeChecker {
            program: program.clone(),
            errors: vec![]
        }
    }

    pub fn analyze(&mut self) {
        self.assign_program(self.program.clone());
        self.check_program(self.program.clone());
    }

    fn push_err<E: Into<Error>>(&mut self, e: E) {
        self.errors.push(e.into());
    }

    fn assign_program(&mut self, program: Rc<Program>) {
        self.assign_class(program.main.clone());
        for class in program.classes.iter() {
            self.assign_class(class.clone());
        }
    }

    fn assign_class(&mut self, class: Rc<Class>) {

        // Assign this class's type
        let class_symbol = class.id.get_symbol().expect("Every class should have a symbol");
        class_symbol.set_type(SymbolType::Class(class_symbol.clone()));

        // Assign member variable types
        for var in class.variables.iter() {
            let var_symbol = var.name.get_symbol().expect("Every member variable should have a symbol");
            var_symbol.set_type(&var.kind);
        }

        // Assign function types
        for func in class.functions.iter() {
            let func_symbol = func.name.get_symbol().expect("Every function should have a symbol");
            let output = Rc::new(SymbolType::from(&func.kind));
            let inputs = func.args.iter().map(|arg| SymbolType::from(&arg.kind)).collect();
            let func_type = SymbolType::Function { inputs, output };
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
                self.push_err(TypeError::override_type_mismatch(&func.name));
            }
        }

        // Typecheck each statement
        for stmt in func.statements.iter() {
            self.check_statement(class, stmt);
        }
    }

    fn check_statement(&mut self, class: &Rc<Class>, stmt: &Rc<Statement>) {
        match ***stmt {
            Stmt::Block { ref statements, .. } => {
                for stmt in statements.iter() {
                    self.check_statement(class, stmt);
                }
            }
            Stmt::While { ref expression, ref statement, .. } => {
                // Assert that the condition expression is a boolean.
                let condition_type = self.check_expression(class, expression);
                match condition_type {
                    SymbolType::Boolean => (),
                    _ => self.push_err(TypeError::ConditionType(format!("{}", condition_type))),
                }

                self.check_statement(class, statement);
            }
            Stmt::Print { ref expression, .. } => {
                // Assert that the expression to print is an int, String, or boolean.
                let expr_type = self.check_expression(class, expression);
                debug!("Printing type {}", expr_type);
                match expr_type {
                    SymbolType::Int |
                    SymbolType::String |
                    SymbolType::Boolean => (),
                    kind => self.push_err(TypeError::PrintType(format!("{}", kind))),
                }
            }
            Stmt::Assign { ref lhs, ref rhs, .. } => {
                // Get the type of the receiving variable
                if let Some(var_symbol) = lhs.get_symbol() {
                    // Assert that the rhs expression type matches the variable type.
                    let var_type = var_symbol.get_type().expect("Every symbol should have a type");
                    let expr_type = self.check_expression(class, rhs);
                    // TODO check that expr is a _subtype_ of var type rather than equal.
                    if expr_type != var_type {
                        self.push_err(TypeError::VariableAssignMismatch(format!("{}", var_type), format!("{}", expr_type)));
                    }
                }
            }
            Stmt::AssignArray { ref lhs, ref in_bracket, ref rhs, .. } => {
                // Assert that the indexing expression is an integer.
                let bracket_type = self.check_expression(class, in_bracket);
                if bracket_type != SymbolType::Int.into() {
                    self.push_err(TypeError::NonIntIndexing(format!("{}", bracket_type)));
                }

                let rhs_type = self.check_expression(class, rhs);

                // Assert that the receiving variable is the correct array type.
                let env = stmt.get_env().expect("Each statement should have an environment");
                if let Some(var_symbol) = env.get(lhs) {
                    match var_symbol.get_type().expect("Every symbol should have a type") {
                        SymbolType::IntArray => {
                            // Assert that the rhs is an int.
                            if rhs_type != SymbolType::Int {
                                self.push_err(TypeError::AssignArray(format!("{}", rhs_type), format!("{}", SymbolType::IntArray)));
                            }
                        }
                        SymbolType::StringArray => {
                            // Assert that the rhs is a String.
                            if rhs_type != SymbolType::String {
                                self.push_err(TypeError::AssignArray(format!("{}", rhs_type), format!("{}", SymbolType::StringArray)));
                            }
                        }
                        SymbolType::ClassArray(ref array_class) => {
                            // Assert that the class type exactly matches the rhs.
                            match rhs_type {
                                SymbolType::Class(ref rhs_class) if rhs_class == array_class => (),
                                _ => {
                                    self.push_err(TypeError::AssignArray(format!("{}", rhs_type), format!("class {}", array_class)))
                                }
                            }
                        },
                        _ => self.push_err(TypeError::AssignArray(format!("{}", var_symbol), format!("{}", rhs_type))),
                    }
                }
            }
            Stmt::If { ref condition, ref statement, ref otherwise, .. } => {
                // Assert that the condition expression is a boolean.
                let condition_type = self.check_expression(class, condition);
                match condition_type {
                    SymbolType::Boolean => (),
                    _ => self.push_err(TypeError::ConditionType(format!("{}", condition_type))),
                }

                // Typecheck "then" and "else"
                self.check_statement(class, statement);
                if let Some(otherwise) = otherwise {
                    self.check_statement(class, otherwise);
                }
            }
            Stmt::SideEffect { ref expression, .. } => {
                self.check_expression(class, expression);
            }
        }
    }

    fn check_expression(&mut self, class: &Rc<Class>, expr: &Rc<Expression>) -> SymbolType {
        let expr_kind = match ***expr {
            Expr::TrueLiteral => SymbolType::Boolean,
            Expr::FalseLiteral => SymbolType::Boolean,
            Expr::IntLiteral(_) => SymbolType::Int,
            Expr::StringLiteral(_) => SymbolType::String,
            Expr::This => SymbolType::Class(class.id.get_symbol().expect("Each class should have a symbol")),
            Expr::NewClass(ref id) => {
                let symbol = id.get_symbol().expect("Each identifier should have a symbol");
                let class_type = symbol.get_type().expect("Each class symbol should have a type");
                match class_type {
                    // If the class type is indeed a class, everything's fine.
                    SymbolType::Class(_) => class_type,
                    kind => {
                        // If the type is anything else, it's an error.
                        self.push_err(TypeError::NewNonClass(format!("{}", kind)));
                        SymbolType::Void
                    }
                }
            }
            Expr::Identifier(ref id) => {
                let env = expr.get_env().expect("Each expression should have an env");
                match env.get(id) {
                    Some(symbol) => symbol.get_type().expect("Each symbol should have a type"),
                    None => SymbolType::Void,
                }
            },
            Expr::Unary(ref unary) => {
                match *unary {
                    UnaryExpression::Parentheses(ref inner_expr) => {
                        self.check_expression(class, inner_expr)
                    }
                    UnaryExpression::Not(ref inner_expr) => {
                        // "Not" should only be applied to booleans
                        let inner_type = self.check_expression(class, inner_expr);
                        if inner_type != SymbolType::Boolean {
                            self.push_err(TypeError::InvalidOperandType("NOT (!)".to_owned(), format!("{}", inner_type)));
                        }
                        SymbolType::Boolean
                    }
                    UnaryExpression::Length(ref inner_expr) => {
                        // Assert that the expression is an array type.
                        let inner_type = self.check_expression(class, inner_expr);
                        match inner_type {
                            // If the type is one of these, everything is fine.
                            SymbolType::IntArray |
                            SymbolType::StringArray |
                            SymbolType::ClassArray(_) => (),
                            _ => self.push_err(TypeError::NonArrayLength(format!("{}", inner_type))),
                        }
                        // Regardless of whether it's applied to a valid expression,
                        // .length always returns an integer.
                        SymbolType::Int
                    }
                    UnaryExpression::NewArray(ref inner_expr) => {
                        // Assert that the expression in brackets is an integer.
                        let inner_type = self.check_expression(class, inner_expr);
                        if inner_type != SymbolType::Int {
                            self.push_err(TypeError::NonIntIndexing(format!("{}", inner_type)));
                        }
                        SymbolType::IntArray
                    }
                    UnaryExpression::Application { ref expression, ref id, ref list, .. } => {
                        debug!("Typechecking function application");

                        // Assert that the lhs expression evaluates to a class (object) type.
                        let expr_type = self.check_expression(class, expression);
                        match expr_type {
                            SymbolType::Class(ref symbol) => {
                                match self.program.get_class(symbol) {
                                    None => {
                                        self.push_err(format_err!("type error: could not find class '{}'", symbol));
                                        SymbolType::Void
                                    },
                                    Some(object_class) => {
                                        // Find the definition for the function call on the object.
                                        match object_class.get_function_by_identifier(id) {
                                            None => unimplemented!("Some error during application"),
                                            Some(func) => {
                                                // Assert that the parameter list matches the formal arguments.
                                                let func_symbol = func.name.get_symbol().expect("Every function should have a symbol");
                                                let func_type = func_symbol.get_type().expect("Every function should have a type");

                                                match func_type {
                                                    SymbolType::Function { ref inputs, ref output, .. } => {
                                                        if list.len() != list.len() {
                                                            self.push_err(format_err!("type error: wrong number of arguments given for function '{}'", func_symbol));
                                                        }

                                                        for (param_item, formal_arg_type) in list.iter().zip(inputs.iter()) {
                                                            // Check that the types of respective params and formal args match.
                                                            let param_type = self.check_expression(class, param_item);
                                                            if param_type != *formal_arg_type {
                                                                self.push_err(format_err!("type error: mismatching types in function application"));
                                                            }
                                                        }

                                                        let kind: SymbolType = (**output).clone();
                                                        kind
                                                    }
                                                    _ => panic!("Function symbol should have a function type"),
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            _ => {
                                self.push_err(format_err!("type error: cannot call a method on a non-object value"));
                                SymbolType::Void
                            },
                        }
                    }
                }
            }
            Expr::Binary(ref binary) => {
                let lhs_type: SymbolType = self.check_expression(class, &binary.lhs);
                let rhs_type: SymbolType = self.check_expression(class, &binary.rhs);

                // Enforce type rules based on which operator this is.
                match binary.kind {
                    // Assert that these operators are given (int x int)
                    BinaryKind::Divide |
                    BinaryKind::Minus |
                    BinaryKind::Times => {
                        if lhs_type != SymbolType::Int || rhs_type != SymbolType::Int {
                            self.push_err(TypeError::InvalidOperandType(format!("{}", binary.kind), format!("{}", lhs_type)));
                        }
                        SymbolType::Int
                    }
                    BinaryKind::Plus => {
                        // Assert that the operands are either int or String.
                        if lhs_type != SymbolType::Int && lhs_type != SymbolType::String {
                            self.push_err(TypeError::InvalidOperandType(format!("{}", binary.kind), format!("{}", lhs_type)));
                        }
                        if rhs_type != SymbolType::Int && rhs_type != SymbolType::String {
                            self.push_err(TypeError::InvalidOperandType(format!("{}", binary.kind), format!("{}", lhs_type)));
                        }

                        // If at least one of the operands is a String, expression is a String.
                        // Otherwise, it must be an int.
                        if lhs_type == SymbolType::String || rhs_type == SymbolType::String {
                            SymbolType::String
                        } else {
                            SymbolType::Int
                        }
                    }
                    BinaryKind::Equals => {
                        // Assert that the lhs and rhs are comparable types.
                        match (&lhs_type, &rhs_type) {
                            (SymbolType::Int, SymbolType::Int) => (),
                            (SymbolType::String, SymbolType::String) => (),
                            (SymbolType::Class(_), SymbolType::Class(_)) => (),
                            _ => self.push_err(TypeError::EqMismatch(format!("{}", lhs_type), format!("{}", rhs_type))),
                        }
                        SymbolType::Boolean
                    }
                    BinaryKind::LessThan => {
                        // Assert that the lhs and rhs are both 'int'.
                        match (&lhs_type, &rhs_type) {
                            (SymbolType::Int, SymbolType::Int) => (),
                            _ => self.push_err(TypeError::EqMismatch(format!("{}", lhs_type), format!("{}", rhs_type))),
                        }
                        SymbolType::Boolean
                    }
                    kind @ BinaryKind::And |
                    kind @ BinaryKind::Or => {
                        // Assert that the lhs and rhs are both 'boolean'.
                        match (&lhs_type, &rhs_type) {
                            (SymbolType::Boolean, SymbolType::Boolean) => (),
                            _ => {
                                let lhs_str = format!("{}", lhs_type);
                                let rhs_str = format!("{}", rhs_type);
                                let err = match kind {
                                    BinaryKind::And => TypeError::AndMismatch(lhs_str, rhs_str),
                                    BinaryKind::Or => TypeError::OrMismatch(lhs_str, rhs_str),
                                    _ => unreachable!(),
                                };
                                self.push_err(err);
                            },
                        }
                        SymbolType::Boolean
                    }
                    BinaryKind::ArrayLookup => {
                        // Assert that the rhs is a valid index (int).
                        if rhs_type != SymbolType::Int {
                            self.push_err(TypeError::NonIntIndexing(format!("{}", rhs_type)));
                        }

                        // Assert the lhs is an array and return the type of that array.
                        match lhs_type {
                            SymbolType::IntArray => SymbolType::Int,
                            SymbolType::StringArray => SymbolType::String,
                            SymbolType::ClassArray(ref symbol) => SymbolType::Class(symbol.clone()),
                            kind => {
                                self.push_err(TypeError::ArrayAccess(format!("{}", kind)));
                                SymbolType::Void
                            }
                        }
                    }
                }
            }
        };

        expr.set_type(expr_kind.clone());
        expr_kind
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_type_eq() {
        assert_ne!(SymbolType::Int, SymbolType::String);
        assert_ne!(
            SymbolType::Function {
                inputs: vec![SymbolType::Int],
                output: Rc::new(SymbolType::Int),
            },
            SymbolType::Function {
                inputs: vec![SymbolType::String],
                output: Rc::new(SymbolType::Int),
            },
        )
    }
}
