#![allow(warnings)]

use std::rc::Rc;
use std::fmt::{
    Display,
    Formatter,
    Result as fmtResult,
};
use failure::Error;

use lexer::{
    SourceMap,
    OwnedToken,
    Position,
};
use syntax::ast::*;
use semantics::Symbol;

#[derive(Debug, Fail)]
pub enum TypeError {
    OverrideTypeMismatch(OwnedToken),
    InvalidOperandType(Position, String, String),
    EqMismatch(Position, String, String),
    LtMismatch(Position, String, String),
    AndMismatch(Position, String, String),
    OrMismatch(Position, String, String),
    NonArrayLength(Position, String),
    NonIntIndexing(Position, String),
    VariableAssignMismatch(Position, String, String),
    AssignArray(Position, String, String),
    ArrayAccess(Position, String),
    NewNonClass(Position, String),
    PrintType(Position, String),
    ConditionType(Position, String),
    UndefinedFunction(Position, String, String),
    InvalidCallObject(Position, String),
}

use self::TypeError as TE;

impl Display for TE {
    fn fmt(&self, f: &mut Formatter) -> fmtResult {
        match *self {
            TE::OverrideTypeMismatch(ref t) => write!(f, "{:08} type error: function '{}' overrides a function with a different type", t.span.start, t.text),
            TE::InvalidOperandType(ref p, ref operator, ref operand) => write!(f, "{:08} type error: operator {} got operand with invalid type {}", p, operator, operand),
            TE::EqMismatch(ref p, ref lhs, ref rhs) => write!(f, "{:08} type error: cannot compare (==) type '{}' with '{}'", p, lhs, rhs),
            TE::LtMismatch(ref p, ref lhs, ref rhs) => write!(f, "{:08} type error: cannot compare (<) type '{}' with '{}'", p, lhs, rhs),
            TE::AndMismatch(ref p, ref lhs, ref rhs) => write!(f, "{:08} type error: cannot AND (&&) type '{}' with '{}'", p, lhs, rhs),
            TE::OrMismatch(ref p, ref lhs, ref rhs) => write!(f, "{:08} type error: cannot OR (||) type '{}' with '{}'", p, lhs, rhs),
            TE::NonArrayLength(ref p, ref non_array) => write!(f, "{:08} type error: cannot apply '.length' to non-array type '{}'", p, non_array),
            TE::NonIntIndexing(ref p, ref non_index) => write!(f, "{:08} type error: array index must be an int, got a '{}'", p, non_index),
            TE::VariableAssignMismatch(ref p, ref var_type, ref expr_type) => write!(f, "{:08} type error: expression should match variable type '{}', got '{}'", p, var_type, expr_type),
            TE::AssignArray(ref p, ref assign, ref array) => write!(f, "{:08} type error: cannot insert type '{}' to array type '{}'", p, assign, array),
            TE::ArrayAccess(ref p, ref non_array) => write!(f, "{:08} type error: cannot index into non-array type '{}'", p, non_array),
            TE::NewNonClass(ref p, ref kind) => write!(f, "{:08} type error: cannot use 'new' operator with identifier '{}'", p, kind),
            TE::PrintType(ref p, ref kind) => write!(f, "{:08} type error: print statement takes 'int', 'String', or 'boolean', got '{}'", p, kind),
            TE::ConditionType(ref p, ref kind) => write!(f, "{:08} type error: condition must be type 'boolean', got '{}'", p, kind),
            TE::UndefinedFunction(ref p, ref func, ref class) => write!(f, "{:08} type error: no such function '{}' defined for class '{}'", p, func, class),
            TE::InvalidCallObject(ref p, ref func) => write!(f, "{:08} type error: cannot call method '{}' on a non-object value", p, func),
        }
    }
}

impl TE {
    fn override_type_mismatch<T: Into<OwnedToken>>(token: T) -> TE {
        TE::OverrideTypeMismatch(token.into())
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

impl SymbolType {
    fn subtype_of(&self, program: &Rc<Program>, other_type: SymbolType) -> bool {
        match (self, &other_type) {
            (SymbolType::Void, SymbolType::Void) => return true,
            (SymbolType::Int, SymbolType::Int) => return true,
            (SymbolType::IntArray, SymbolType::IntArray) => return true,
            (SymbolType::String, SymbolType::String) => return true,
            (SymbolType::StringArray, SymbolType::StringArray) => return true,
            (SymbolType::Boolean, SymbolType::Boolean) => return true,
            (SymbolType::ClassArray(ref symbol), SymbolType::ClassArray(ref other)) => return symbol == other,
            (SymbolType::Class(ref my_symbol), SymbolType::Class(other_symbol)) => {
                // Every class is a subtype of itself.
                if my_symbol == other_symbol { return true; }

                let my_class = program.get_class(my_symbol);
                if let Some(my_class) = my_class {
                    // Check whether this class extends another.
                    let mut my_super = my_class.extends.as_ref().map(|rc| rc.clone());
                    loop {
                        match my_super {
                            None => return false,
                            // If this class extends another, check if the extended class
                            // matches the one we were given for comparison.
                            Some(self_super) => {
                                match self_super.get_symbol() {
                                    Some(my_super_symbol) => {
                                        if my_super_symbol == *other_symbol { return true; }
                                    }
                                    _ => {
                                        warn!("Could not find class symbol for '{}' to perform subtype comparison", self_super.text);
                                        return false;
                                    }
                                }

                                // Walk up the tree of extending classes.
                                let super_class = self_super.get_symbol().and_then(|ref symbol| program.get_class(symbol));
                                let super_superclass = super_class.and_then(|sc| sc.extends.clone());
                                my_super = super_superclass;
                            }
                        }
                    }
                }
            }
            _ => (),
        }
        false
    }
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

pub struct TypeChecker<'a> {
    program: Rc<Program>,
    source_map: &'a SourceMap,
    pub errors: Vec<Error>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(source_map: &'a SourceMap, program: &Rc<Program>) -> TypeChecker<'a> {
        TypeChecker {
            source_map,
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
                self.push_err(TE::override_type_mismatch(&func.name));
            }
        }

        // Typecheck each statement
        for stmt in func.statements.iter() {
            self.check_statement(class, stmt);
        }

        // Check that the return type is the same as the function's return type.
        if let Some(ref return_expr) = func.expression {
            let return_expr_type = self.check_expression(class, return_expr);
            match func_type {
                SymbolType::Function { output, .. } => {
                    if return_expr_type != *output {
                        self.errors.push(format_err!("{} type error: return value does not match function return type", return_expr.span.start));
                    }
                }
                _ => ()
            }
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
                    _ => self.push_err(TE::ConditionType(expression.span.start, format!("{}", condition_type))),
                }

                self.check_statement(class, statement);
            }
            Stmt::Print { ref expression, .. } => {
                // Assert that the expression to print is an int, String, or boolean.
                let expr_type = self.check_expression(class, expression);
                match expr_type {
                    SymbolType::Int |
                    SymbolType::String |
                    SymbolType::Boolean => (),
                    kind => self.push_err(TE::PrintType(expression.span.start, format!("{}", kind))),
                }
            }
            Stmt::Assign { ref lhs, ref rhs, .. } => {
                // Get the type of the receiving variable
                if let Some(var_symbol) = lhs.get_symbol() {
                    // Assert that the rhs expression type matches the variable type.
                    let var_type = var_symbol.get_type().expect("Every symbol should have a type");
                    let expr_type = self.check_expression(class, rhs);

                    match (var_type, expr_type) {
                        (SymbolType::Class(ref var_class_id), SymbolType::Class(ref expr_class_id)) => {
                            match (var_class_id.get_type(), expr_class_id.get_type()) {
                                (Some(var_class_type), Some(expr_class_type)) => {
                                    if !expr_class_type.subtype_of(&self.program, var_class_type.clone()) {
                                        warn!("Type '{}' is not a subtype of '{}'", expr_class_type, var_class_type);
                                        self.push_err(TE::VariableAssignMismatch(rhs.span.start, format!("{}", var_class_type), format!("{}", expr_class_type)));
                                    }
                                }
                                _ => {
                                    warn!("Missing type for SymbolType::Class");
                                }
                            }
                        }
                        (expr_type, var_type) => if expr_type != var_type {
                            self.push_err(TE::VariableAssignMismatch(rhs.span.start, format!("{}", var_type), format!("{}", expr_type)));
                        }
                    }
                }
            }
            Stmt::AssignArray { ref lhs, ref index, ref rhs, .. } => {
                // Assert that the indexing expression is an integer.
                let bracket_type = self.check_expression(class, index);
                if bracket_type != SymbolType::Int.into() {
                    self.push_err(TE::NonIntIndexing(index.span.start, format!("{}", bracket_type)));
                }

                let rhs_type = self.check_expression(class, rhs);

                // Assert that the receiving variable is the correct array type.
                let env = stmt.get_env().expect("Each statement should have an environment");
                if let Some(var_symbol) = env.get(lhs) {
                    match var_symbol.get_type().expect("Every symbol should have a type") {
                        SymbolType::IntArray => {
                            // Assert that the rhs is an int.
                            if rhs_type != SymbolType::Int {
                                self.push_err(TE::AssignArray(index.span.start, format!("{}", rhs_type), format!("{}", SymbolType::IntArray)));
                            }
                        }
                        SymbolType::StringArray => {
                            // Assert that the rhs is a String.
                            if rhs_type != SymbolType::String {
                                self.push_err(TE::AssignArray(index.span.start, format!("{}", rhs_type), format!("{}", SymbolType::StringArray)));
                            }
                        }
                        SymbolType::ClassArray(ref array_class) => {
                            // Assert that the class type exactly matches the rhs.
                            match rhs_type {
                                SymbolType::Class(ref rhs_class) if rhs_class == array_class => (),
                                _ => {
                                    self.push_err(TE::AssignArray(index.span.start, format!("{}", rhs_type), format!("class {}", array_class)))
                                }
                            }
                        },
                        _ => self.push_err(TE::AssignArray(index.span.start, format!("{}", var_symbol), format!("{}", rhs_type))),
                    }
                }
            }
            Stmt::If { ref condition, ref statement, ref otherwise, .. } => {
                // Assert that the condition expression is a boolean.
                let condition_type = self.check_expression(class, condition);
                match condition_type {
                    SymbolType::Boolean => (),
                    _ => self.push_err(TE::ConditionType(condition.span.start, format!("{}", condition_type))),
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
                match id.get_symbol() {
                    None => SymbolType::Void,
                    Some(symbol) => {
                        let class_type = symbol.get_type().expect("Each class symbol should have a type");
                        match class_type {
                            // If the class type is indeed a class, everything's fine.
                            SymbolType::Class(_) => class_type,
                            kind => {
                                // If the type is anything else, it's an error.
                                self.push_err(TE::NewNonClass(expr.span.start, format!("{}", kind)));
                                SymbolType::Void
                            }
                        }
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
                            self.push_err(TE::InvalidOperandType(inner_expr.span.start, "NOT (!)".to_owned(), format!("{}", inner_type)));
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
                            _ => self.push_err(TE::NonArrayLength(expr.span.start, format!("{}", inner_type))),
                        }
                        // Regardless of whether it's applied to a valid expression,
                        // .length always returns an integer.
                        SymbolType::Int
                    }
                    UnaryExpression::NewArray(ref inner_expr) => {
                        // Assert that the expression in brackets is an integer.
                        let inner_type = self.check_expression(class, inner_expr);
                        if inner_type != SymbolType::Int {
                            self.push_err(TE::NonIntIndexing(expr.span.start, format!("{}", inner_type)));
                        }
                        SymbolType::IntArray
                    }
                    UnaryExpression::Application { ref expression, ref id, ref list, .. } => {
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
                                            None => {
                                                self.push_err(TE::UndefinedFunction(id.span.start, String::from(&id.text), String::from(&object_class.id.text)));
                                                SymbolType::Void
                                            }
                                            Some(func) => {
                                                // Assert that the parameter list matches the formal arguments.
                                                let func_symbol = func.name.get_symbol().expect("Every function should have a symbol");
                                                let func_type = func_symbol.get_type().expect("Every function should have a type");

                                                // Link the function to the symbol which declares it.
                                                id.set_symbol(&func_symbol);

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
                                warn!("Called a method on expression of type '{}'", expr_type);
//                                self.push_err(format_err!("type error: cannot call a method on a non-object value"));
                                self.push_err(TE::InvalidCallObject(id.token.span.start, format!("{}", id.text)));
                                SymbolType::Void
                            },
                        }
                    }
                    UnaryExpression::ArrayLookup { ref lhs, ref index, .. } => {
                        let lhs_type: SymbolType = self.check_expression(class, lhs);
                        let index_type: SymbolType = self.check_expression(class, index);

                        // Assert that the index is a valid index type (int).
                        if index_type != SymbolType::Int {
                            self.push_err(TE::NonIntIndexing(expr.span.start, format!("{}", index_type)));
                        }

                        // Assert the lhs is an array and return the type of that array.
                        match lhs_type {
                            SymbolType::IntArray => SymbolType::Int,
                            SymbolType::StringArray => SymbolType::String,
                            SymbolType::ClassArray(ref symbol) => SymbolType::Class(symbol.clone()),
                            kind => {
                                self.push_err(TE::ArrayAccess(expr.span.start, format!("{}", kind)));
                                SymbolType::Void
                            }
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
                            self.push_err(TE::InvalidOperandType(expr.span.start, format!("{}", binary.kind), format!("{}", lhs_type)));
                        }
                        SymbolType::Int
                    }
                    BinaryKind::Plus => {
                        // Assert that the operands are either int or String.
                        if lhs_type != SymbolType::Int && lhs_type != SymbolType::String {
                            self.push_err(TE::InvalidOperandType(expr.span.start, format!("{}", binary.kind), format!("{}", lhs_type)));
                        }
                        if rhs_type != SymbolType::Int && rhs_type != SymbolType::String {
                            self.push_err(TE::InvalidOperandType(expr.span.start, format!("{}", binary.kind), format!("{}", lhs_type)));
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
                            _ => self.push_err(TE::EqMismatch(expr.span.start, format!("{}", lhs_type), format!("{}", rhs_type))),
                        }
                        SymbolType::Boolean
                    }
                    BinaryKind::LessThan => {
                        // Assert that the lhs and rhs are both 'int'.
                        match (&lhs_type, &rhs_type) {
                            (SymbolType::Int, SymbolType::Int) => (),
                            _ => self.push_err(TE::EqMismatch(expr.span.start, format!("{}", lhs_type), format!("{}", rhs_type))),
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
                                    BinaryKind::And => TE::AndMismatch(expr.span.start, lhs_str, rhs_str),
                                    BinaryKind::Or => TE::OrMismatch(expr.span.start, lhs_str, rhs_str),
                                    _ => unreachable!(),
                                };
                                self.push_err(err);
                            },
                        }
                        SymbolType::Boolean
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
