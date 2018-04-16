use std::rc::Rc;
use std::fmt::Write;
use failure::Error;

use Result;
use codegen::{
    Bytecode,
    Bytecode::*,
    ClassDecl,
    LabelMaker,
    MethodDecl,
};
use syntax::ast::*;
use semantics::type_analysis::SymbolType;

pub enum CodegenError {

}

pub struct CodeGenerator {
    pub errors: Vec<Error>,
    pub classes: Vec<ClassDecl>,
    label_maker: LabelMaker,
}

impl CodeGenerator {
    pub fn new() -> CodeGenerator {
        CodeGenerator {
            errors: vec![],
            classes: vec![],
            label_maker: LabelMaker::new(),
        }
    }

    fn push_err<E: Into<Error>>(&mut self, e: E) {
        self.errors.push(e.into());
    }

    pub fn gen_program(&mut self, program: &Program) {
        let main = self.gen_class(&program.main, true);
        self.classes.push(main);
        for class in program.classes.iter() {
            let class_decl = self.gen_class(class, false);
            self.classes.push(class_decl);
        }
    }

    fn gen_class(&mut self, class: &Class, main: bool) -> ClassDecl {
        let class_symbol = class.id.get_symbol().expect("Each class should have a symbol");
        let name = format!("{}", class_symbol.name);

        // If this class has a superclass, get its name. Otherwise, use java/lang/Object.
        let extends = class.superclass.borrow().as_ref()
            .map(|cls| {
                let cls_symbol = cls.id.get_symbol().expect("Each class should have a symbol");
                format!("{}", cls_symbol.name)
            })
            .unwrap_or("java/lang/Object".to_owned());

        let methods = class.functions.iter().map(|func| {
            self.gen_func(func, main)
        }).collect();

        ClassDecl { name, extends, methods }
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
            },
            SymbolType::ClassArray(ref symbol) => {
                write!(string, "[L{};", symbol.name);
            },
            SymbolType::Function { ref inputs, ref output } => {
                string.push('(');
                for (i, input) in inputs.iter().enumerate() {
                    if i != 0 { string.push(','); }
                    string.push_str(&self.gen_type(input));
                }
                string.push(')');
                string.push_str(&self.gen_type(output));
            },
        }
        string
    }

    fn gen_func(&mut self, func: &Function, main: bool) -> MethodDecl {
        let func_symbol = func.name.get_symbol().expect("Each function should have a symbol");
        let name = format!("{}", func_symbol.name);

        let func_type = func_symbol.get_type().expect("Each function should have a type");
        let signature = self.gen_type(&func_type);

        MethodDecl { name, main, signature, code: vec![] }
    }

    fn gen_expression(&mut self, expression: &Expression) -> Vec<Bytecode> {
        let mut code = vec![];
        match expression.expr {
            Expr::FalseLiteral => code.push(iconst_0),
            Expr::TrueLiteral => code.push(iconst_1),
            Expr::StringLiteral(ref token) => code.push(ldc_str(String::from(&token.text))),
            Expr::This => {
                match expression.get_type().expect("Each expression should have a type") {
                    SymbolType::Class(ref _symbol) => {
                        unimplemented!()
                    }
                    _ => panic!("Each 'this' expression must have a class type"),
                }
            }
            Expr::Unary(ref unary) => {
                use syntax::ast::UnaryExpression::*;
                match *unary {
                    NewArray(ref expr) => unimplemented!(),
                    Not(ref expr) => unimplemented!(),
                    Parentheses(ref expr) => unimplemented!(),
                    Length(ref expr) => unimplemented!(),
                    Application { ref expression, ref id, ref list } => unimplemented!(),
                }
            }
            Expr::Binary(ref binary) => {
                self.gen_expression(&binary.lhs);
                self.gen_expression(&binary.rhs);

                match binary.kind {
                    BinaryKind::And => code.push(iand),
                    BinaryKind::Or => code.push(ior),
                    BinaryKind::Minus => code.push(isub),
                    BinaryKind::Times => code.push(imul),
                    BinaryKind::Divide => code.push(idiv),
                    BinaryKind::Plus => {
                        let lhs_type = binary.lhs.get_type();
                        let rhs_type = binary.rhs.get_type();
                        match (lhs_type, rhs_type) {
                            (Some(SymbolType::Int), Some(SymbolType::Int)) => code.push(iadd),
                            (Some(SymbolType::Int), Some(SymbolType::String)) |
                            (Some(SymbolType::String), Some(SymbolType::Int)) |
                            (Some(SymbolType::String), Some(SymbolType::String)) => {
                                // String concatentation
                            }
                            _ => self.push_err(format_err!("codegen error: plus must have strings or ints"))
                        }
                    }
                    _ => unimplemented!()
                }
            }
            _ => unimplemented!()
        }
        code
    }

    fn gen_binary_expression(&mut self, binary: &BinaryExpression) {
        match binary.kind {
            _ => unimplemented!()
        }
    }
}
