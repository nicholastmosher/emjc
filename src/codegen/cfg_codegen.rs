use std::mem;
use std::rc::Rc;
use std::fmt::Write;
use std::collections::{
    HashMap,
    HashSet,
};
use failure::Error;
use itertools::Itertools;

use lexer::SourceMap;
use syntax::ast::*;
use semantics::{
    Symbol,
    type_analysis::SymbolType,
};
use control_flow::{
    Cfg,
    CfgNode,
    EdgeData,
};
use codegen::{
    Bytecode,
    Bytecode::*,
    ClassDecl,
    MethodDecl,
    MemberDecl,
    LabelMaker,
    Label,
};

type VariableMap = Vec<Rc<Symbol>>;
type MemberMap = HashMap<Rc<Symbol>, String>;
type FuncMap = HashMap<Rc<Symbol>, String>;
type LabelMap = HashMap<CfgNode, Label>;

pub struct CodeGenerator<'a> {
    pub classes: Vec<ClassDecl>,
    pub errors: Vec<Error>,
    program: Rc<Program>,
    class_graphs: &'a [(Rc<Class>, Vec<Cfg<'a>>)],
    source_map: &'a SourceMap,
    func_map: FuncMap,
    member_map: MemberMap,
    variable_map: VariableMap,
    label_maker: LabelMaker,
    label_map: LabelMap,
}

impl<'a> CodeGenerator<'a> {
    pub fn new(program: &Rc<Program>, source_map: &'a SourceMap, cfgs: &'a [(Rc<Class>, Vec<Cfg<'a>>)]) -> CodeGenerator<'a> {
        CodeGenerator {
            classes: Vec::new(),
            errors: Vec::new(),
            program: program.clone(),
            class_graphs: cfgs,
            source_map,
            func_map: FuncMap::new(),
            member_map: MemberMap::new(),
            variable_map: VariableMap::new(),
            label_maker: LabelMaker::new(),
            label_map: LabelMap::new(),
        }
    }

    fn push_err<E: Into<Error>>(&mut self, e: E) {
        self.errors.push(e.into());
    }

    pub fn generate(&mut self) {
        for (class, graphs) in self.class_graphs.iter() {
            let class_decl = self.generate_class(class, graphs);
            self.classes.push(class_decl);
        }
    }

    fn generate_class(&mut self, class: &Rc<Class>, cfgs: &[Cfg]) -> ClassDecl {
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
                members.push(self.generate_member(member));
            }

            // Declare functions.
            for function in class.functions.iter() {
                let func_symbol = function.name.get_symbol().expect("Each function should have a symbol");
                let func_type = func_symbol.get_type().expect("Each function should have a type");
                let func_signature = format!("{}/{}{}",
                                             &walk_class_symbol.name,
                                             func_symbol.name,
                                             self.generate_type(&func_type));
                self.func_map.insert(func_symbol, func_signature);
            }

            // Walk up superclasses if they exist.
            match class.get_superclass() {
                None => break,
                Some(ref superclass) => class = superclass.clone(),
            }
        }

        let methods = cfgs.iter().map(|cfg| {
            self.generate_function(&class, cfg)
        }).collect();

        ClassDecl { name, extends, members, methods }
    }

    fn generate_member(&mut self, var: &Variable) -> MemberDecl {
        MemberDecl {
            name: String::from(&var.name.text),
            kind: self.generate_type(&SymbolType::from(&var.kind)),
        }
    }

    fn generate_function(&mut self, class: &Rc<Class>, cfg: &Cfg) -> MethodDecl {
        let func_symbol = cfg.function.name.get_symbol().expect("Each function should have a symbol");

        // Special case for "main"
        let main = func_symbol.name.id == "main";
        let name = if main {
            "main".to_owned()
        } else {
            format!("{}", func_symbol.name)
        };

        let func_type = func_symbol.get_type().expect("Each function should have a type");
        let signature = self.generate_type(&func_type);

        // Associate each variable symbol with the memory slot which it is stored in.
        // The memory slot where the variable is stored is it's index into the vector plus 1.
        mem::replace(&mut self.variable_map, VariableMap::new());
        // The label map stores the jump tags for control points.
        mem::replace(&mut self.label_map, LabelMap::new());
        let mut code = Vec::<Bytecode>::new();

        let mut visited = HashSet::new();
        self.generate_cfg_path(&mut code, &mut visited, cfg, cfg.start);

        MethodDecl { name, signature, code, main }
    }

    fn generate_cfg_path(&mut self, code: &mut Vec<Bytecode>, visited: &mut HashSet<CfgNode>, cfg: &Cfg, node: CfgNode) {
        if visited.contains(&node) {
            warn!("Skipping already-visited node {:?}", node);
            return;
        }
        visited.insert(node);

        let outgoing_edges = cfg.graph.get(&node);
        if outgoing_edges.is_none() { return; }
        let outgoing_edges = outgoing_edges.unwrap();
        let incoming_edges = cfg.predecessors_of(&node).len();

        // If there is more than one incoming edge, create a label here
        // to jump back to (this means there is a loop).
        if incoming_edges > 1 ||
            // If there is an incoming edge on the start, it must be a loop.
            (incoming_edges > 0 && node == cfg.start)
            {
                let loop_label = self.label_maker.make_named("loop_top");
                self.label_map.insert(node, loop_label.clone());
                code.push(label(loop_label));
            }

        match outgoing_edges.len() {
            0 => return,
            // If there is just one edge out, just append the bytecode of that edge.
            1 => {
                let mut next = None;
                let mut edge = None;
                for (n, e) in outgoing_edges.iter() {
                    next = Some(n);
                    edge = Some(e);
                }
                let next = next.expect("There should be one outgoing node");
                let edge = edge.expect("There should be one outgoing edge");

                match edge {
                    EdgeData::Stmt(ref statement) => {
                        self.generate_statement(code, statement);
                    }
                    EdgeData::Var(ref variable) => {
                        let variable_symbol = variable.name.get_symbol().expect("Each variable should have a symbol");
                        self.variable_map.push(variable_symbol);
                    }
                    EdgeData::Return(ref expression) => {
                        // Evaluate the return statement and put the result on the stack.
                        code.push(comment(format!("RETURN {}", self.source_map.spanning(expression.span))));
                        self.generate_expression(code, expression);
                    }
                    _ => panic!("Single out-edge should not have an expression"),
                }

                if self.label_map.contains_key(&next) {
                    // If the next node is in the label map, jump to it.
                    let node_label = self.label_map.get(&next).unwrap();
                    code.push(goto(node_label.clone()));
                } else {
                    // If the next node is not in the label map, recursively generate it.
                    self.generate_cfg_path(code, visited, cfg, *next);
                }
            }
            // If there are two edges out, create branches.
            2 => {
                let mut true_node = None;
                let mut false_node = None;
                let mut expression = None;
                for (end, edge) in outgoing_edges.iter() {
                    match edge {
                        EdgeData::Expr(ref expr) => {
                            true_node = Some(end);
                            expression = Some(expr.clone());
                        }
                        EdgeData::ExprNot(ref expr) => {
                            false_node = Some(end);
                            expression = Some(expr.clone());
                        }
                        ref s => {
                            error!("Error at node {}", node);
                            panic!("Outgoing edges from a branch in cfg should be expressions, got {:?}", s);
                        }
                    }
                }
                let true_node = true_node.expect("True condition should exist");
                let false_node = false_node.expect("False condition should exist");
                let expression = expression.expect("Expression should exist");

                // Create labels for "then", "else" and "after"
                let then = self.label_maker.make_named("then");
                let elze = self.label_maker.make_named("else");
                let after = self.label_maker.make_named("after");

                // Create branches for the expression.
                self.generate_branch(code, &expression, then.clone(), elze.clone());

                // Directly append the code for the first statement.
                code.push(label(then));
                self.generate_cfg_path(code, visited, cfg, *true_node);
                code.push(goto(after.clone()));

                // Add a label to the second statement.
                code.push(label(elze));
                self.generate_cfg_path(code, visited, cfg, *false_node);

                // Add a label for after both statements.
                code.push(label(after));
            }
            _ => panic!("Each node should have a max of two outgoing edges"),
        }
    }

    fn generate_load_variable(&mut self, code: &mut Vec<Bytecode>, var_symbol: &Rc<Symbol>) {
        let var_type = var_symbol.get_type().expect("Each variable should have a type");
        // If the variable being assigned is a local variable
        if let Some((index, _)) = self.variable_map.iter()
            .find_position(|symbol| symbol.name == var_symbol.name) {
            let instruction = match (index + 1, &var_type) {
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
                (0, _) => aload_0,
                (1, _) => aload_1,
                (2, _) => aload_2,
                (3, _) => aload_3,
                (x, _) => aload_x(x as u64),
            };
            code.push(instruction);
            return;
        }

        // If the variable being assigned is a member variable
        if self.member_map.contains_key(var_symbol) {
            let member_spec = self.member_map.get(var_symbol).unwrap().clone();
            let member_type = self.generate_type(&var_type);
            code.push(aload_0);
            code.push(getfield(member_spec.to_owned(), member_type));
            return;
        }

        panic!("Variable must be either a local or member variable");
    }

    fn generate_statement(&mut self, code: &mut Vec<Bytecode>, statement: &Statement) {
        use syntax::ast::Stmt::*;
        match statement.stmt {
            Print { ref expression, .. } => {
                let expr_type = expression.get_type().expect("Each expression should have a type");
                // Put the PrintStream object on the stack.
                code.push(getstatic("java/lang/System/out".to_owned(), "Ljava/io/PrintStream;".to_owned()));
                // Put the value of the expression on the stack.
                code.push(comment(format!("PRINT EXPR START: {}", self.source_map.spanning(expression.span))));
                self.generate_expression(code, expression);
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
                self.generate_expression(code, rhs);

                (|| {
                    // If the variable being assigned is a local variable
                    if let Some((index, _)) = self.variable_map.iter()
                        .find_position(|symbol| symbol.name == var_symbol.name) {
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
                        let member_type = self.generate_type(&var_type);
                        code.push(putfield(member_spec.to_owned(), member_type));
                        return;
                    }

                    panic!("Variable must be either a local or member variable");
                })();
            }
            AssignArray { ref lhs, ref index, ref rhs, .. } => {
                let var_symbol = lhs.get_symbol().expect("Each variable should have a symbol");
                let var_type = var_symbol.get_type().expect("Each variable should have a type");

                code.push(comment(format!("ASSIGN {}[{}] = {}",
                                          self.source_map.spanning(lhs.span),
                                          self.source_map.spanning(index.span),
                                          self.source_map.spanning(rhs.span))));
                self.generate_load_variable(code, &var_symbol);

                // Evaluate the index for the array. some_array[this_part] = ...
                self.generate_expression(code, index);

                // Push the value to store onto the stack. Stack should look like this:
                //
                // | Xastore         |
                // | Value to store  |
                // | Array index     |
                // | Array reference |
                // |-----------------|
                // Where X indicates the type of array (i for int[], a for Class[])
                self.generate_expression(code, rhs);

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
                self.generate_expression(code, expression);
                code.push(pop); // Discard the result of the expression.
            }
            _ => panic!("Control flow graph should not have compound statement"),
        }
    }

    fn generate_expression(&mut self, code: &mut Vec<Bytecode>, expression: &Expression) {
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
                    x => sipush_x(x),
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
                self.generate_load_variable(code, &var_symbol);
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
                        self.generate_expression(code, expr);

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
                    Not(ref expr) => {
                        // Push the expression value onto the stack
                        self.generate_expression(code, expr);

                        // Check if the value is non-zero
                        let n_true = self.label_maker.make_named("nTrue");
                        let n_after = self.label_maker.make_named("nAfter");

                        // Compare if the expression is greater than 0.
                        code.push(iconst_0);
                        code.push(if_icmpge(n_true.clone()));

                        // If the expression was false (0), return 1.
                        code.push(iconst_1);
                        code.push(goto(n_after.clone()));

                        // If the value was true (<0), return 0.
                        code.push(label(n_true));
                        code.push(iconst_0);
                        code.push(label(n_after));
                    }
                    Parentheses(ref expr) => self.generate_expression(code, expr),
                    Length(ref expr) => {
                        // Load the expression onto the stack. Typechecker ensures this is an array.
                        self.generate_expression(code, expr);
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
                                                      .fold(String::new(), |mut s, arg| {
                                                          s.push_str(&arg);
                                                          s.push(',');
                                                          s
                                                      })
                        )));

                        // Get the object instance from the expression and push it on the stack.
                        self.generate_expression(code, expression);

                        // Push the arguments onto the stack.
                        for parameter in list.iter() {
                            self.generate_expression(code, parameter);
                        }

                        // Get the function which is being executed.
                        let function_signature = match expr_type {
                            SymbolType::Class(ref class_symbol) => {
                                let object_class = self.program.get_class(class_symbol)
                                    .expect(&format!("Undefined class {}", class_symbol));

                                let function = Class::get_function_by_identifier(&object_class, id)
                                    .expect(&format!("Undefined function {}", &id.text));

                                let object_symbol = object_class.id.get_symbol().expect("Each class should have a symbol");
                                let func_symbol = function.get_symbol().expect("Each function should have a symbol");
                                let func_type = func_symbol.get_type().expect("Each function should have a type");
                                let func_signature = self.generate_type(&func_type);
                                format!("{}/{}{}", object_symbol.name, func_symbol.name, func_signature)
                            }
                            _ => panic!("Cannot call function on non-object type"),
                        };

                        // Invoke the function.
                        code.push(invokevirtual(function_signature));
                        code.push(comment(format!("END FUNC-CALL")));
                    }
                    ArrayLookup { ref lhs, ref index, .. } => {
                        code.push(comment(format!("READ {}[{}]",
                                                  self.source_map.spanning(lhs.span),
                                                  self.source_map.spanning(index.span))));

                        // Load the array reference onto the stack
                        self.generate_expression(code, lhs);

                        // Load the array index onto the stack
                        self.generate_expression(code, index);

                        let array_type = lhs.get_type().expect("Each expression should have a type");
                        let instruction = match array_type {
                            SymbolType::IntArray => iaload,
                            SymbolType::StringArray |
                            SymbolType::ClassArray(_) => aaload,
                            ref kind => panic!("Cannot index into non-array type {}", kind),
                        };

                        code.push(instruction);
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
                        self.generate_expression(code, &binary.lhs);
                        self.generate_expression(code, &binary.rhs);
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
                                self.generate_expression(code, &binary.lhs);
                                self.generate_expression(code, &binary.rhs);
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
                                self.generate_concatenation(code, expression);

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
    fn generate_concatenation(&mut self, code: &mut Vec<Bytecode>, expression: &Expression) {
        match expression.expr {
            // If this is a binary expression, concat the left and right sides.
            Expr::Binary(ref binary) => {
                self.generate_concatenation(code, &binary.lhs);
                self.generate_concatenation(code, &binary.rhs);
            }
            // If this is any other kind of expression (i.e. a leaf), append to StringBuilder.
            _ => {
                // Invoke StringBuilder::append
                let expr_type = expression.get_type().expect("Each expression should have a type");
                match expr_type {
                    SymbolType::Int => {
                        // Push the expression onto the stack
                        self.generate_expression(code, expression);
                        code.push(invokevirtual("Ljava/lang/StringBuilder/append(I)Ljava/lang/StringBuilder;".to_owned()));
                    }
                    SymbolType::String => {
                        // Push the expression onto the stack
                        self.generate_expression(code, expression);
                        code.push(invokevirtual("Ljava/lang/StringBuilder/append(Ljava/lang/String;)Ljava/lang/StringBuilder;".to_owned()));
                    }
                    ref t => panic!("Cannot concatenate non-string or non-int type {} at {}", t, expression.span),
                }
            }
        }
    }

    fn generate_branch(&mut self, code: &mut Vec<Bytecode>, expression: &Expression, then: Label, elze: Label) {
        match expression.expr {
            Expr::TrueLiteral => {
                code.push(goto(then.clone()));
                return;
            }
            Expr::FalseLiteral => {
                code.push(goto(elze.clone()));
                return;
            }
            Expr::Binary(ref binary) => {
                match binary.kind {
                    BinaryKind::And => {
                        let next = self.label_maker.make_named("nNext");
                        self.generate_branch(code, &binary.lhs, next.clone(), elze.clone());
                        code.push(label(next));
                        self.generate_branch(code, &binary.rhs, then, elze);
                    }
                    BinaryKind::Or => {
                        let next = self.label_maker.make_named("nNext");
                        self.generate_branch(code, &binary.lhs, then.clone(), next.clone());
                        code.push(label(next));
                        self.generate_branch(code, &binary.lhs, then, elze);
                    }
                    BinaryKind::LessThan => {
                        self.generate_expression(code, &binary.lhs);
                        self.generate_expression(code, &binary.rhs);
                        code.push(if_icmplt(then));
                        code.push(goto(elze));
                    }
                    BinaryKind::Equals => {
                        self.generate_expression(code, &binary.lhs);
                        self.generate_expression(code, &binary.rhs);
                        code.push(if_icmpeq(elze)); // If condition == 0 goto else
                        code.push(goto(then));
                    }
                    _ => panic!("Cannot branch on non-boolean condition"),
                }
                return;
            }
            Expr::Unary(ref unary) => {
                match unary {
                    UnaryExpression::Not(ref expr) => {
                        self.generate_branch(code, expr, elze, then);
                        return;
                    }
                    _ => (),
                }
            }
            _ => (),
        }

        // For any other expression, just evaluate it
        self.generate_expression(code, expression);
        code.push(ifeq(elze)); // If expression is false, goto else.
        code.push(goto(then)); // Otherwise goto then.
    }

    fn generate_type(&self, kind: &SymbolType) -> String {
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
                    string.push_str(&self.generate_type(input));
                }
                string.push(')');
                string.push_str(&self.generate_type(output));
            }
        }
        string
    }
}