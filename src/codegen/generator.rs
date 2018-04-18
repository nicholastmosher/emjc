use std::mem;
use std::rc::Rc;
use std::fmt::Write;
use std::collections::HashMap;
use failure::Error;
use itertools::Itertools;

use Result;
use codegen::{
    Bytecode,
    Bytecode::*,
    ClassDecl,
    LabelMaker,
    MemberDecl,
    MethodDecl,
};
use lexer::SourceMap;
use syntax::ast::*;
use semantics::{
    Symbol,
    type_analysis::SymbolType,
};

type VariableMap = Vec<Rc<Symbol>>;
type MemberMap = HashMap<Rc<Symbol>, String>;
type FuncMap = HashMap<Rc<Symbol>, String>;

pub struct CodeGenerator<'a> {
    pub errors: Vec<Error>,
    pub classes: Vec<ClassDecl>,
    program: Rc<Program>,
    variable_map: VariableMap,
    member_map: MemberMap,
    func_map: FuncMap,
    label_maker: LabelMaker,
    source_map: &'a SourceMap,
}

impl<'a> CodeGenerator<'a> {
    pub fn new(source_map: &'a SourceMap, program: &Rc<Program>) -> CodeGenerator<'a> {
        CodeGenerator {
            errors: vec![],
            classes: vec![],
            program: program.clone(),
            variable_map: VariableMap::new(),
            member_map: MemberMap::new(),
            func_map: FuncMap::new(),
            label_maker: LabelMaker::new(),
            source_map,
        }
    }

    fn push_err<E: Into<Error>>(&mut self, e: E) {
        self.errors.push(e.into());
    }

    pub fn generate(&mut self) {
        let main = self.gen_class(&self.program.main.clone(), true);
        self.classes.push(main);
        for class in self.program.clone().classes.iter() {
            let class_decl = self.gen_class(class, false);
            self.classes.push(class_decl);
        }
    }

    fn gen_class(&mut self, class: &Rc<Class>, main: bool) -> ClassDecl {
        let class_symbol = class.id.get_symbol().expect("Each class should have a symbol");
        let name = format!("{}", class_symbol.name);

        // If this class has a superclass, get its name. Otherwise, use java/lang/Object.
        let extends = class.superclass.borrow().as_ref()
            .map(|cls| {
                let cls_symbol = cls.id.get_symbol().expect("Each class should have a symbol");
                format!("{}", cls_symbol.name)
            })
            .unwrap_or("java/lang/Object".to_owned());

        // The member map stores the signature (CLASS/MEMBER_NAME) for each member variable.
        mem::replace(&mut self.member_map, MemberMap::new());
        // The function map stores the signature (CLASS/FUNC_NAME(FUNC_SIGNATURE)) for each function.
        mem::replace(&mut self.func_map, FuncMap::new());
        let mut members = vec![];

        // Declare member variables for this class and super classes.
        let mut class: Rc<Class> = class.clone();
        loop {
            let walk_class_symbol = class.id.get_symbol().expect("Each class should have a symbol");

            // Declare member variables.
            for member in class.variables.iter() {
                let member_symbol = member.name.get_symbol().expect("Each member variable should have a symbol");
                let member_signature = format!("{}/{}", &walk_class_symbol.name, member_symbol.name);
                self.member_map.insert(member_symbol, member_signature);
                members.push(self.gen_member(member));
            }

            // Declare functions.
            for function in class.functions.iter() {
                let func_symbol = function.name.get_symbol().expect("Each function should have a symbol");
                let func_type = func_symbol.get_type().expect("Each function should have a type");
                let func_signature = format!("{}/{}{}",
                                             &walk_class_symbol.name,
                                             func_symbol.name,
                                             self.gen_type(&func_type));
                self.func_map.insert(func_symbol, func_signature);
            }

            // Walk up superclasses if they exist.
            match class.get_superclass() {
                None => break,
                Some(ref superclass) => class = superclass.clone(),
            }
        }

        let methods = class.functions.iter().map(|func| {
            self.gen_func(func, main)
        }).collect();

        ClassDecl { name, extends, members, methods }
    }

    fn gen_type(&mut self, kind: &SymbolType) -> String {
        let mut string = String::new();
        match *kind {
            SymbolType::Void => string.push('V'),
            SymbolType::Int => string.push('I'),
            SymbolType::IntArray => string.push_str("[I"),
            SymbolType::String => string.push_str("Ljava/lang/String;"),
            SymbolType::StringArray => string.push_str("[Ljava/lang/String;"),
            SymbolType::Boolean => string.push('Z'),
            SymbolType::Class(ref symbol) => {
                write!(string, "L{};", symbol.name);
            }
            SymbolType::ClassArray(ref symbol) => {
                write!(string, "[L{};", symbol.name);
            }
            SymbolType::Function { ref inputs, ref output } => {
                string.push('(');
                for (i, input) in inputs.iter().enumerate() {
                    if i != 0 { string.push(','); }
                    string.push_str(&self.gen_type(input));
                }
                string.push(')');
                string.push_str(&self.gen_type(output));
            }
        }
        string
    }

    fn gen_member(&mut self, var: &Variable) -> MemberDecl {
        MemberDecl {
            name: String::from(&var.name.text),
            kind: self.gen_type(&SymbolType::from(&var.kind)),
        }
    }

    fn gen_func(&mut self, func: &Function, main: bool) -> MethodDecl {
        let func_symbol = func.name.get_symbol().expect("Each function should have a symbol");
        let name = format!("{}", func_symbol.name);

        let func_type = func_symbol.get_type().expect("Each function should have a type");
        let signature = self.gen_type(&func_type);

        // Associate each variable symbol with the memory slot which it is stored in.
        // The memory slot where the variable is stored is it's index into the vector plus 1.
        mem::replace(&mut self.variable_map, VariableMap::new());
        let mut code = Vec::<Bytecode>::new();

        for arg in func.args.iter() {
            let arg_symbol = arg.name.get_symbol().expect("Each argument should have a symbol");
            self.variable_map.push(arg_symbol);
        }

        for var in func.variables.iter() {
            let var_symbol = var.name.get_symbol().expect("Each variable should have a symbol");
            self.variable_map.push(var_symbol);
        }

        for stmt in func.statements.iter() {
            self.gen_statement(&mut code, stmt);
        }

        // Generate code for return statement
        match func.expression {
            None => code.push(return_void),
            Some(ref return_expression) => {
                let ret = match return_expression.get_type().expect("Each expression should have a type") {
                    SymbolType::Int |
                    SymbolType::Boolean => ireturn,
                    _ => areturn,
                };

                // Push the value to return onto the stack.
                self.gen_expression(&mut code, return_expression);
                code.push(ret);
            }
        }

        MethodDecl { name, main, signature, code }
    }

    fn gen_statement(&mut self, code: &mut Vec<Bytecode>, statement: &Statement) {
        use syntax::ast::Stmt::*;
        match statement.stmt {
            Block { ref statements, .. } => {
                for statement in statements.iter() {
                    self.gen_statement(code, statement);
                }
            }
            While { ref expression, ref statement, .. } => {
                code.push(comment("WHILE".to_owned()));
                let start = self.label_maker.make_named("nStart");
                code.push(label(start.clone()));
                self.gen_expression(code, expression);
                let exit = self.label_maker.make_named("nAfter");
                code.push(ifeq(exit.clone()));
                code.push(comment("THEN".to_owned()));
                self.gen_statement(code, statement);
                code.push(goto(start));
                code.push(label(exit));
                code.push(comment("END_WHILE".to_owned()));
            }
            Print { ref expression, .. } => {
                let expr_type = expression.get_type().expect("Each expression should have a type");
                // Put the PrintStream object on the stack.
                code.push(getstatic("java/lang/System/out".to_owned(), "Ljava/io/PrintStream;".to_owned()));
                // Put the value of the expression on the stack.
                code.push(comment(format!("PRINT EXPR START: {}", self.source_map.spanning(expression.span))));
                self.gen_expression(code, expression);
                code.push(comment(format!("PRINT EXPR END")));
                match expr_type {
                    SymbolType::Int => {
                        code.push(invokevirtual("java/io/PrintStream/print(I)V".to_owned()));
                    }
                    SymbolType::String => {
                        code.push(invokevirtual("java/io/PrintStream/print(Ljava/lang/String;)V".to_owned()));
                    }
                    _ => panic!("Cannot print expression of type {}", expr_type)
                }
            }
            Assign { ref lhs, ref rhs, .. } => {
                let var_symbol = lhs.get_symbol().expect("Each variable should have a symbol");
                let var_type = var_symbol.get_type().expect("Each variable should have a type");
                code.push(comment(format!("ASSIGN {} = {}", lhs.text, self.source_map.spanning(rhs.span))));

                // Push the value of the expression on the stack.
                self.gen_expression(code, rhs);

                (|| {
                    // If the variable being assigned is a local variable
                    if let Some((index, _)) = self.variable_map.iter().find_position(|symbol| symbol.name == var_symbol.name) {
                        let instruction = match (index + 1, &var_type) {
                            (0, SymbolType::Int) |
                            (0, SymbolType::Boolean) => istore_0,
                            (1, SymbolType::Int) |
                            (1, SymbolType::Boolean) => istore_1,
                            (2, SymbolType::Int) |
                            (2, SymbolType::Boolean) => istore_2,
                            (3, SymbolType::Int) |
                            (3, SymbolType::Boolean) => istore_3,
                            (x, SymbolType::Int) |
                            (x, SymbolType::Boolean) => istore_x(x as u32),
                            (0, _) => astore_0,
                            (1, _) => astore_1,
                            (2, _) => astore_2,
                            (3, _) => astore_3,
                            (x, _) => astore_x(x as u64),
                        };
                        code.push(instruction);
                        return;
                    }

                    // If the variable being assigned is a member variable
                    if self.member_map.contains_key(&var_symbol) {
                        let member_spec = self.member_map.get(&var_symbol).unwrap().clone();
                        let member_type = self.gen_type(&var_type);
                        code.push(putfield(member_spec.to_owned(), member_type));
                        return;
                    }

                    panic!("Variable must be either a local or member variable");
                })();
            }
            AssignArray { ref lhs, ref in_bracket, ref rhs, .. } => {
                let var_symbol = lhs.get_symbol().expect("Each variable should have a symbol");
                let var_type = var_symbol.get_type().expect("Each variable should have a type");
                let (var_index, _) = self.variable_map.iter().find_position(|symbol| **symbol == var_symbol)
                    .expect("Each variable should be entered in the variable map");
                let var_index = var_index + 1;

                code.push(comment(format!("ARRAY ASSIGN {}[{}] = {}",
                                          lhs.text,
                                          self.source_map.spanning(in_bracket.span),
                                          self.source_map.spanning(rhs.span))));

                // Load the array reference onto the stack
                let load = match var_index {
                    0 => aload_0,
                    1 => aload_1,
                    2 => aload_2,
                    3 => aload_3,
                    x => aload_x(x as u64),
                };
                code.push(load);

                // Evaluate the index for the array. some_array[this_part] = ...
                self.gen_expression(code, in_bracket);

                // Push the value to store onto the stack. Stack should look like this:
                //
                // | Xastore         |
                // | Value to store  |
                // | Array index     |
                // | Array reference |
                // |-----------------|
                // Where X indicates the type of array (i for int[], a for Class[])
                self.gen_expression(code, rhs);

                // Choose the correct store instruction based on array type.
                let store = match var_type {
                    SymbolType::StringArray |
                    SymbolType::ClassArray(_) => aastore,
                    SymbolType::IntArray => iastore,
                    _ => panic!("Cannot array-assign into non-array type"),
                };
                code.push(store);
            }
            SideEffect { ref expression, .. } => {
                self.gen_expression(code, expression);
                code.push(pop); // Discard the result of the expression.
            }
            If { ref condition, ref statement, ref otherwise, .. } => {
                // TODO implement short-circuiting.
                code.push(comment("BEGIN_IF".to_owned()));
                self.gen_expression(code, condition);

                // Code is different depending on whether there's an "else" statement or not.
                match otherwise {
                    Some(otherwise) => {
                        let elze = self.label_maker.make_named("nElse");
                        code.push(ifne(elze.clone()));
                        code.push(comment("THEN".to_owned()));
                        self.gen_statement(code, statement);
                        let after = self.label_maker.make_named("nAfter");
                        code.push(goto(after.clone()));
                        code.push(label(elze));
                        code.push(comment("ELSE".to_owned()));
                        self.gen_statement(code, otherwise);
                        code.push(label(after));
                    }
                    None => {
                        let exit = self.label_maker.make_named("nExit");
                        code.push(ifne(exit.clone()));
                        code.push(comment("THEN".to_owned()));
                        self.gen_statement(code, statement);
                        code.push(label(exit));
                    }
                }
                code.push(comment("ENDIF".to_owned()));
            }
        }
    }

    fn gen_expression(&mut self, code: &mut Vec<Bytecode>, expression: &Expression) {
        match expression.expr {
            Expr::FalseLiteral => code.push(iconst_0),
            Expr::TrueLiteral => code.push(iconst_1),
            Expr::IntLiteral(ref token) => {
                let int: u32 = token.text.parse().expect("Int literal should parse to u32");
                let instruction = match int {
                    0 => iconst_0,
                    1 => iconst_1,
                    2 => iconst_2,
                    3 => iconst_3,
                    4 => iconst_4,
                    5 => iconst_5,
                    x => bipush_x(x),
                };
                code.push(instruction);
            }
            Expr::StringLiteral(ref token) => code.push(ldc_str(String::from(&token.text))),
            Expr::NewClass(ref id) => {
                code.push(comment(format!("NEW CLASS {}", &id.text)));
                let class_symbol = id.get_symbol().expect("Each class instantiation should have a symbol");
                // Create the new object reference for the class.
                code.push(new(format!("{}", class_symbol.name)));
                // Duplicate the object reference.
                code.push(dup);
                // Consume the duplicated object reference to execute the constructor.
                code.push(invokespecial(format!("{}", class_symbol.name)))
            }
            Expr::Identifier(ref variable) => {
                let var_symbol = variable.get_symbol().expect("Each variable should have a symbol");
                let var_type = var_symbol.get_type().expect("Each variable should have a type");

                code.push(comment(format!("READ VARIABLE {}", self.source_map.spanning(variable.span))));

                // Early exit this closure on success. If neither case early-exits, panic.
                (|| {
                    // If this variable is a function local variable, use iload/aload.
                    if let Some((index, symbol)) = self.variable_map.iter().find_position(|var| var.name == var_symbol.name) {
                        // Load the value from the index it's stored in memory.
                        let instruction = match (index + 1, &var_type) {
                            // Bools and Ints use 'iload'
                            (0, SymbolType::Int) |
                            (0, SymbolType::Boolean) => iload_0,
                            (1, SymbolType::Int) |
                            (1, SymbolType::Boolean) => iload_1,
                            (2, SymbolType::Int) |
                            (2, SymbolType::Boolean) => iload_2,
                            (3, SymbolType::Int) |
                            (3, SymbolType::Boolean) => iload_3,
                            (x, SymbolType::Int) |
                            (x, SymbolType::Boolean) => iload_x(x as u32),
                            // All other types are references and use 'aload'
                            (0, _) => aload_0,
                            (1, _) => aload_1,
                            (2, _) => aload_2,
                            (3, _) => aload_3,
                            (x, _) => aload_x(x as u64),
                        };
                        code.push(instruction);
                        return;
                    }

                    // If this variable is a class member variable, use getfield
                    if self.member_map.contains_key(&var_symbol) {
                        let member_spec = self.member_map.get(&var_symbol).unwrap().clone();
                        let member_type = self.gen_type(&var_type);
                        code.push(getfield(member_spec.to_owned(), member_type));
                        return;
                    }

                    panic!("Variable must be either a local or member variable");
                })();
            }
            Expr::This => code.push(aload_0),
            Expr::Unary(ref unary) => {
                use syntax::ast::UnaryExpression::*;
                match *unary {
                    NewArray(ref expr) => {

                        // Type checking should have guaranteed that the [expr] was an int, but
                        // just double check to make sure.
                        assert_eq!(expr.get_type().unwrap(), SymbolType::Int, "Array index should be an int");

                        // Load the size of the array onto the stack.
                        self.gen_expression(code, expr);

                        // Invoke 'newarray' with the type
                        let expr_type = expression.get_type().expect("Each expression should have a type");
                        match expr_type {
                            SymbolType::IntArray => {
                                code.push(newarray("int".to_owned()))
                            }
                            SymbolType::StringArray => {
                                unimplemented!()
                            }
                            SymbolType::ClassArray(ref class) => {
                                unimplemented!()
                            }
                            _ => panic!("Cannot create a new array of a non-array type"),
                        }
                    }
                    Not(ref expr) => unimplemented!(),
                    Parentheses(ref expr) => self.gen_expression(code, expr),
                    Length(ref expr) => {
                        // Load the expression onto the stack. Typechecker ensures this is an array.
                        self.gen_expression(code, expr);
                        // Push the array length instruction.
                        code.push(arraylength);
                    }
                    Application { ref expression, ref id, ref list } => {
                        // Assert that the expression is a class type.
                        let expr_type = expression.get_type().expect("Each object must have a type");
                        match expr_type {
                            SymbolType::Class(_) => (),
                            _ => panic!("Cannot invoke a function on non-class type {}", expr_type),
                        }

                        code.push(comment(format!("BEGIN FUNC-CALL {}.{}({})",
                            self.source_map.spanning(expression.span),
                            &id.text,
                            list.iter().map(|ref arg| self.source_map.spanning(arg.span))
                                                      .fold(String::new(), |mut s, arg| { s.push_str(&arg); s })
                        )));

                        // Get the object instance from the expression and push it on the stack.
                        self.gen_expression(code, expression);

                        // Push the arguments onto the stack.
                        for parameter in list.iter() {
                            self.gen_expression(code, parameter);
                        }

                        // Get the function which is being executed.
                        let function_signature = match expr_type {
                            SymbolType::Class(ref class_symbol) => {
                                let object_class = self.program.get_class(class_symbol)
                                    .expect(&format!("Undefined class {}", class_symbol));

                                let function = object_class.get_function_by_identifier(id)
                                    .expect(&format!("Undefined function {}", &id.text));

                                let object_symbol = object_class.id.get_symbol().expect("Each class should have a symbol");
                                let func_symbol = function.get_symbol().expect("Each function should have a symbol");
                                let func_type = func_symbol.get_type().expect("Each function should have a type");
                                let func_signature = self.gen_type(&func_type);
                                format!("{}/{}{}", object_symbol.name, func_symbol.name, func_signature)
                            }
                            _ => panic!("Cannot call function on non-object type"),
                        };

                        // Invoke the function.
                        code.push(invokevirtual(function_signature));
                        code.push(comment(format!("END FUNC-CALL")));
                    }
                    ArrayLookup { ref lhs, ref index, .. } => {
                        // Load the array reference
                        code.push(comment("DOING INT ARRAY LOOKUP".to_owned()));
                    }
                }
            }
            Expr::Binary(ref binary) => {
                let lhs_type = binary.lhs.get_type().expect("Each expression should have a type");
                let rhs_type = binary.rhs.get_type().expect("Each expression should have a type");

                match binary.kind {
                    // For these operators, push both operands right onto the stack.
                    BinaryKind::And |
                    BinaryKind::Or |
                    BinaryKind::Minus |
                    BinaryKind::Times |
                    BinaryKind::Divide |
                    BinaryKind::LessThan => {
                        self.gen_expression(code, &binary.lhs);
                        self.gen_expression(code, &binary.rhs);
                    }
                    // The other operators may need to do stuff differently.
                    _ => (),
                }

                match binary.kind {
                    BinaryKind::And => code.push(iand),
                    BinaryKind::Or => code.push(ior),
                    BinaryKind::Minus => code.push(isub),
                    BinaryKind::Times => code.push(imul),
                    BinaryKind::Divide => code.push(idiv),
                    BinaryKind::Plus => {
                        match (&lhs_type, &rhs_type) {
                            (SymbolType::Int, SymbolType::Int) => {
                                self.gen_expression(code, &binary.lhs);
                                self.gen_expression(code, &binary.rhs);
                                code.push(iadd)
                            }
                            (SymbolType::Int, SymbolType::String) |
                            (SymbolType::String, SymbolType::Int) |
                            (SymbolType::String, SymbolType::String) => {
                                code.push(comment("BEGIN_STRING_CONCATENATION".to_owned()));

                                // Create a StringBuilder
                                code.push(new("Ljava/lang/StringBuilder;".to_owned()));
                                code.push(dup);
                                code.push(invokespecial("Ljava/lang/StringBuilder".to_owned()));

                                // StringBuilder reference is on top of the stack.
                                self.gen_concatenation(code, expression);

                                // Execute StringBuilder::toString to perform the concatenation.
                                code.push(invokevirtual("Ljava/lang/StringBuilder/toString()Ljava/lang/String;".to_owned()));

                                code.push(comment("END_STRING_CONCATENATION".to_owned()));
                            }
                            _ => self.push_err(format_err!("codegen error: plus must have strings or ints"))
                        }
                    }
                    BinaryKind::Equals => {
                        match (lhs_type, rhs_type) {
                            (SymbolType::Int, SymbolType::Int) => {
                                let true_label = self.label_maker.make_named("nEqual");
                                let after_label = self.label_maker.make_named("nAfter");

                                code.push(if_icmpeq(true_label.clone()));
                                code.push(iconst_0); // 0 for not equal.
                                code.push(goto(after_label.clone()));
                                code.push(label(true_label));
                                code.push(iconst_1); // 1 for equal.
                                code.push(label(after_label));
                            }
                            (SymbolType::String, SymbolType::String) => {
                                // Strings one and two should be on the stack.
                                code.push(invokevirtual("java/lang/String/equals".to_owned()));
                            }
                            (SymbolType::Class(ref class1), SymbolType::Class(ref class2)) => {}
                            _ => panic!("Comparing two incompatible types"),
                        }
                    }
                    BinaryKind::LessThan => {
                        let lt = self.label_maker.make_named("nLess");
                        let after = self.label_maker.make_named("nAfter");
                        code.push(if_icmplt(lt.clone()));
                        code.push(iconst_0); // Not less than => 0
                        code.push(goto(after.clone()));
                        code.push(label(lt));
                        code.push(iconst_1); // Less than => 1
                        code.push(label(after));
                    }
                    ref x => unimplemented!("Have not implemented binary expr {:#?}", x),
                }
            }
            ref x => unimplemented!("Have not implemented expr {:#?}", x),
        }
    }

    /// Performs String concatenation of the given Expression. This expression must be
    /// either a String literal, an Int literal, or a Binary expression of "+" with either
    /// Int or String on either side. This function flattens the expression and appends
    /// each term to a StringBuilder.
    ///
    /// Precondition: A StringBuilder object is initialized with its reference on the top
    /// of the stack.
    fn gen_concatenation(&mut self, code: &mut Vec<Bytecode>, expression: &Expression) {
        match expression.expr {
            // If this is a binary expression, concat the left and right sides.
            Expr::Binary(ref binary) => {
                self.gen_concatenation(code, &binary.lhs);
                self.gen_concatenation(code, &binary.rhs);
            }
            // If this is any other kind of expression (i.e. a leaf), append to StringBuilder.
            _ => {
                // Invoke StringBuilder::append
                let expr_type = expression.get_type().expect("Each expression should have a type");
                match expr_type {
                    SymbolType::Int => {
                        // Push the expression onto the stack
                        self.gen_expression(code, expression);
                        code.push(invokevirtual("Ljava/lang/StringBuilder/append(I)Ljava/lang/StringBuilder;".to_owned()));
                    }
                    SymbolType::String => {
                        // Push the expression onto the stack
                        self.gen_expression(code, expression);
                        code.push(invokevirtual("Ljava/lang/StringBuilder/append(Ljava/lang/String;)Ljava/lang/StringBuilder;".to_owned()));
                    }
                    ref t => panic!("Cannot concatenate non-string or non-int type {} at {}", t, expression.span),
                }
            }
        }
    }
}
