use std::cell::RefCell;
use parser::ast::*;
use super::Visitor;

pub struct Printer {
    indent: RefCell<usize>,
}

impl Printer {
    pub fn new() -> Self { Printer { indent: RefCell::new(0) } }
    fn do_indent(&self) { for _ in 0..*self.indent.borrow() { print!("\t"); } }

    fn increment_indent(&self) {
        *self.indent.borrow_mut() += 1;
    }

    fn decrement_indent(&self) {
        if *self.indent.borrow() > 0 {
            *self.indent.borrow_mut() -= 1;
        }
    }
}

impl Visitor for Printer {
    fn visit_program(&self, program: &Program) {
        self.visit_main(&program.main);
        println!();
        for class in program.classes.iter() {
            self.visit_class(class);
        }
        println!();
    }

    fn visit_main(&self, main: &Main) {
        print!("(MAIN-CLASS-DECL ");
        self.visit_identifier(&main.id);
        println!();

        self.increment_indent();
        self.do_indent();
        print!("(MAIN-FUN-CALL (STRING-ARRAY ");
        self.visit_identifier(&main.args);
        print!(")\n");

        self.increment_indent();
        self.visit_statement(&main.body);
        self.decrement_indent();

        print!("\n");
        self.do_indent();
        print!(")\n");
        self.decrement_indent();
        self.do_indent();
        print!(")");
    }

    fn visit_identifier(&self, id: &Identifier) {
        print!("(ID {})", id.0.text)
    }

    fn visit_class(&self, class: &Class) {
        print!("(CLASS-DECL ");
        self.visit_identifier(&class.id);

        if let Some(ref extends) = class.extends {
            print!(" ");
            self.visit_extends(extends);
        }

        print!("\n");
        self.increment_indent();

        for var in class.variables.iter() {
            self.visit_variable(var);
            print!("\n");
        }
        for method in class.functions.iter() {
            self.visit_function(method);
            print!("\n");
        }

        self.decrement_indent();
        self.do_indent();
        print!(")");
    }

    fn visit_extends(&self, extends: &Extends) {
        print!("(EXTENDS ");
        self.visit_identifier(&extends.extended);
        print!(")");
    }

    fn visit_variable(&self, variable: &Variable) {
        self.do_indent();
        print!("(VAR-DECL ");
        self.visit_type(&variable.kind);
        print!(" ");
        self.visit_identifier(&variable.name);
        print!(")");
    }

    fn visit_function(&self, function: &Function) {
        self.do_indent();
        print!("(MTD-DECL ");
        self.visit_type(&function.kind);
        print!(" ");
        self.visit_identifier(&function.name);

        print!(" (TY-ID-LIST");
        for arg in function.args.iter() {
            print!(" ");
            self.visit_argument(arg);
        }
        print!(")\n");

        self.increment_indent();
        for v in function.variables.iter() {
            self.visit_variable(v);
            print!("\n");
        }
        for s in function.statements.iter() {
            self.visit_statement(s);
            print!("\n");
        }
        self.decrement_indent();

        self.do_indent();
        print!(")");
    }

    fn visit_type(&self, ty: &Type) {
        match *ty {
            Type::Int => print!("INT"),
            Type::IntArray => print!("INT-ARRAY"),
            Type::String => print!("STRING"),
            Type::Boolean => print!("BOOLEAN"),
            Type::Id(ref id) => self.visit_identifier(id),
        }
    }

    fn visit_argument(&self, argument: &Argument) {
        print!("(");
        self.visit_type(&argument.kind);
        print!(" ");
        self.visit_identifier(&argument.name);
        print!(")");
    }

    fn visit_statement(&self, statement: &Statement) {
        match *statement {
            Statement::Print { ref expression, .. } => {
                self.do_indent();
                print!("(PRINTLN ");
                self.visit_expression(expression);
                print!(")");
            }
            Statement::Braced { ref statements, .. } => {
                self.do_indent();
                print!("(BLOCK\n");
                self.increment_indent();
                for (i, statement) in statements.iter().enumerate() {
                    if i != 0 { println!() }
                    self.visit_statement(statement);
                }
                println!();
                self.decrement_indent();
                self.do_indent();
                print!(")\n");
            }
            Statement::Assign { ref lhs, ref rhs, .. } => {
                self.do_indent();
                print!("(EQSIGN ");
                self.visit_identifier(lhs);
                print!(" ");
                self.visit_expression(rhs);
                print!(")");
            },
            Statement::While { ref expression, ref statement, .. } => {
                self.do_indent();
                print!("(WHILE ");
                self.visit_expression(expression);
                print!("\n");
                self.increment_indent();
                self.visit_statement(statement);
                self.decrement_indent();
                print!("\n");
                self.do_indent();
                print!(")");
            },
            Statement::SideEffect { ref expression, .. } => {
                self.do_indent();
                print!("(SIDEF ");
                self.visit_expression(expression);
                print!(")");
            },
            Statement::AssignArray { ref lhs, ref in_bracket, ref rhs } => {
                self.do_indent();
                print!("(EQSIGN (ARRAY-ASSIGN ");
                self.visit_identifier(lhs);
                self.visit_expression(in_bracket);
                print!(") ");
                self.visit_expression(rhs);
                print!(")");
            },
            Statement::If { ref condition, ref statement, ref otherwise } => {
                self.do_indent();
                print!("(IF ");
                self.visit_expression(condition);
                print!("\n");

                self.increment_indent();
                self.visit_statement(statement);

                if let Some(otherwise) = otherwise.as_ref() {
                    self.visit_statement(otherwise);
                }
                self.decrement_indent();

                println!();
                self.do_indent();
                print!(")");
            },
        }
    }

    fn visit_expression(&self, expression: &Expression) {
        match *expression {
            Expression::Identifier(ref id) => {
                self.visit_identifier(id);
            },
            Expression::IntLiteral(ref token) => {
                print!("(INTLIT {})", token.text);
            },
            Expression::StringLiteral(ref token) => {
                print!("(STRINGLIT {})", token.text);
            },
            Expression::Unary(ref unary) => {
                self.visit_unary_expression(unary);
            },
            Expression::Binary(ref binary) => {
                self.visit_binary_expression(binary);
            },
            Expression::NewClass(ref id) => {
                print!("(NEW-INSTANCE ");
                self.visit_identifier(id);
                print!(")");
            },
            Expression::This => {
                print!("THIS");
            },
            Expression::TrueLiteral => {
                print!("TRUE");
            },
            Expression::FalseLiteral => {
                print!("FALSE");
            },
        }
    }

    fn visit_unary_expression(&self, unary_expression: &UnaryExpression) {
        match *unary_expression {
            UnaryExpression::Not(ref expression) => {
                print!("(! ");
                self.visit_expression(expression);
                print!(")");
            },
            UnaryExpression::Application { ref expression, ref id, ref list } => {
                print!("(DOT ");
                self.visit_expression(expression);
                print!(" (FUN-CALL ");
                self.visit_identifier(id);
                for expression in list.0.iter() {
                    self.visit_expression(expression);
                }
                print!("))");
            },
            UnaryExpression::NewArray(ref expression) => {
                print!("(NEW-INT-ARRAY ");
                self.visit_expression(expression);
                print!(")");
            },
            UnaryExpression::Length(ref expression) => {
                print!("(DOT ");
                self.visit_expression(expression);
                print!(" LENGTH)");
            },
            UnaryExpression::Parentheses(ref expression) => {
                self.visit_expression(expression);
            },
        }
    }

    fn visit_binary_expression(&self, binary_expression: &BinaryExpression) {
        print!("(");
        match binary_expression.kind {
            BinaryKind::LessThan => print!("<"),
            BinaryKind::Equals => print!("EQUALS"),
            BinaryKind::And => print!("&&"),
            BinaryKind::Or => print!("||"),
            BinaryKind::Plus => print!("PLUS"),
            BinaryKind::Minus => print!("-"),
            BinaryKind::Times => print!("*"),
            BinaryKind::Divide => print!("/"),
            BinaryKind::ArrayLookup => print!("ARRAY-LOOKUP"),
        }
        print!(" ");
        self.visit_expression(&binary_expression.lhs);
        print!(" ");
        self.visit_expression(&binary_expression.rhs);
        print!(")");
    }

    fn visit_expression_list(&self, list: &ExpressionList) {
        for expression in list.0.iter() {
            self.visit_expression(expression);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use tendril::StrTendril;
    use lexer::{
        Token,
        TokenType,
    };

    #[test]
    fn test_print_ast() {
        let program: Program = Program {
            main: Main {
                id: Identifier(Token { ty: TokenType::ID, text: StrTendril::from("Main"), line: 0, column: 0 }),
                args: Identifier(Token { ty: TokenType::ID, text: StrTendril::from("argus"), line: 0, column: 0 }),
                body: Statement::Braced {
                    statements: vec![
                        Statement::Assign {
                            lhs: Identifier(Token { ty: TokenType::ID, text: StrTendril::from("varx"), line: 0, column: 0 }),
                            rhs: Expression::StringLiteral(
                                Token { ty: TokenType::STRINGLIT, text: StrTendril::from("blah bby"), line: 0, column: 0 },
                            ),
                        },
                    ],
                },
            },
            classes: vec![
                Class {
                    id: Identifier(Token { ty: TokenType::ID, text: StrTendril::from("Boof"), line: 0, column: 0 }),
                    extends: Some(Extends {
                        extended: Identifier(Token { ty: TokenType::ID, text: StrTendril::from("SuperBoof"), line: 0, column: 0, })
                    }),
                    variables: vec![
                        Variable {
                            kind: Type::Boolean,
                            name: Identifier(Token { ty: TokenType::ID, text: StrTendril::from("is_boofable"), line: 0, column: 0 }),
                        },
                    ],
                    functions: vec![
                        Function {
                            kind: Type::IntArray,
                            name: Identifier(Token { ty: TokenType::ID, text: StrTendril::from("get_the_boofs"), line: 0, column: 0 }),
                            args: Vec::new(),
                            variables: vec![
                                Variable {
                                    kind: Type::String,
                                    name: Identifier(Token { ty: TokenType::ID, text: StrTendril::from("boof_store"), line: 0, column: 0 }),
                                },
                            ],
                            statements: vec![
                                Statement::Assign {
                                    lhs: Identifier(Token { ty: TokenType::ID, text: StrTendril::from("boof_store"), line: 0, column: 0 }),
                                    rhs: Expression::StringLiteral(
                                        Token { ty: TokenType::STRINGLIT, text: StrTendril::from("BOOFALICIOUS"), line: 0, column: 0 },
                                    ),
                                },
                                Statement::While {
                                    expression: Expression::Unary(
                                        UnaryExpression::Not(Expression::Binary(
                                            BinaryExpression {
                                                kind: BinaryKind::LessThan,
                                                lhs: Expression::Identifier(
                                                    Identifier(Token { ty: TokenType::ID, text: StrTendril::from("i"), line: 0, column: 0 }),
                                                ).into(),
                                                rhs: Expression::IntLiteral(
                                                    Token { ty: TokenType::INTLIT, text: StrTendril::from("10"), line: 0, column: 0 }
                                                ).into(),
                                            },
                                        ).into()),
                                    ),
                                    statement: Box::new(Statement::Assign {
                                        lhs: Identifier(Token { ty: TokenType::ID, text: StrTendril::from("i"), line: 0, column: 0 }),
                                        rhs: Expression::IntLiteral(Token { ty: TokenType::INTLIT, text: StrTendril::from("12"), line: 0, column: 0, }),
                                    }),
                                },
                            ],
                            expression: Expression::IntLiteral(
                                Token { ty: TokenType::INTLIT, text: StrTendril::from("12"), line: 0, column: 0 }
                            ),
                        }
                    ],
                },
            ],
        };

        let printer = Printer::new();
        printer.visit_program(&program);
    }
}
