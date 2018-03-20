#![allow(unused_must_use)]

use std::rc::Rc;
use std::fmt::Write;
use syntax::ast::*;
use super::Visitor;

pub struct Printer {
    indent: usize,
    buffer: String,
}

impl Printer {
    pub fn new() -> Self { Printer { indent: 0, buffer: String::new() } }
    pub fn contents(self) -> String { self.buffer }

    fn indent(&mut self) { for _ in 0..self.indent { write!(self.buffer, "\t"); } }
    fn inc(&mut self) { self.indent += 1; }
    fn dec(&mut self) { if self.indent > 0 { self.indent -= 1; } }
}

impl<'a> Visitor<&'a Program> for Printer {
    fn visit(&mut self, program: &'a Program) {
        self.visit(program.main.clone());
        writeln!(self.buffer);
        for class in program.classes.iter() {
            self.visit(class.clone());
        }
        writeln!(self.buffer);
    }
}

impl Visitor<Rc<Main>> for Printer {
    fn visit(&mut self, main: Rc<Main>) {
        write!(self.buffer, "(MAIN-CLASS-DECL ");
        self.visit(main.id.clone());
        writeln!(self.buffer);

        self.inc();
        self.indent();
        write!(self.buffer, "(MAIN-FUN-CALL (STRING-ARRAY ");
        self.visit(main.args.clone());
        write!(self.buffer, ")\n");

        self.inc();
        self.visit(main.body.clone());
        self.dec();

        write!(self.buffer, "\n");
        self.indent();
        write!(self.buffer, ")\n");
        self.dec();
        self.indent();
        write!(self.buffer, ")");
    }
}

impl Visitor<Rc<Class>> for Printer {
    fn visit(&mut self, class: Rc<Class>) {
        write!(self.buffer, "(CLASS-DECL ");
        self.visit(class.id.clone());

        if let Some(ref extends) = class.extends {
            write!(self.buffer, " ");
            self.visit(extends.clone());
        }

        write!(self.buffer, "\n");
        self.inc();

        for var in class.variables.iter() {
            self.visit(var.clone());
            write!(self.buffer, "\n");
        }
        for method in class.functions.iter() {
            self.visit(method.clone());
            write!(self.buffer, "\n");
        }

        self.dec();
        self.indent();
        write!(self.buffer, ")");
    }
}

impl Visitor<Rc<Identifier>> for Printer {
    fn visit(&mut self, id: Rc<Identifier>) {
        write!(self.buffer, "(ID {})", id.text);
    }
}

impl Visitor<Rc<Extends>> for Printer {
    fn visit(&mut self, extends: Rc<Extends>) {
        write!(self.buffer, "(EXTENDS ");
        self.visit(extends.extended.clone());
        write!(self.buffer, ")");
    }
}

impl Visitor<Rc<Variable>> for Printer {
    fn visit(&mut self, variable: Rc<Variable>) {
        self.indent();
        write!(self.buffer, "(VAR-DECL ");
        self.visit(variable.kind.clone());
        write!(self.buffer, " ");
        self.visit(variable.name.clone());
        write!(self.buffer, ")");
    }
}

impl Visitor<Rc<Function>> for Printer {
    fn visit(&mut self, function: Rc<Function>) {
        self.indent();
        write!(self.buffer, "(MTD-DECL ");
        self.visit(function.kind.clone());
        write!(self.buffer, " ");
        self.visit(function.name.clone());

        write!(self.buffer, " (TY-ID-LIST");
        for arg in function.args.iter() {
            write!(self.buffer, " ");
            self.visit(arg.clone());
        }
        write!(self.buffer, ")\n");

        self.inc();
        for v in function.variables.iter() {
            self.visit(v.clone());
            write!(self.buffer, "\n");
        }
        for s in function.statements.iter() {
            self.visit(s.clone());
            write!(self.buffer, "\n");
        }
        self.dec();

        self.indent();
        write!(self.buffer, ")");
    }
}

impl Visitor<Rc<Type>> for Printer {
    fn visit(&mut self, kind: Rc<Type>) {
        match *kind {
            Type::Int => { write!(self.buffer, "INT"); },
            Type::IntArray => { write!(self.buffer, "INT-ARRAY"); },
            Type::String => { write!(self.buffer, "STRING"); },
            Type::Boolean => { write!(self.buffer, "BOOLEAN"); },
            Type::Id(ref id) => self.visit(id.clone()),
        };
    }
}

impl Visitor<Rc<Argument>> for Printer {
    fn visit(&mut self, argument: Rc<Argument>) {
        write!(self.buffer, "(");
        self.visit(argument.kind.clone());
        write!(self.buffer, " ");
        self.visit(argument.name.clone());
        write!(self.buffer, ")");
    }
}

impl Visitor<Rc<Statement>> for Printer {
    fn visit(&mut self, statement: Rc<Statement>) {
        match *statement {
            Statement::Print { ref expression, .. } => {
                self.indent();
                write!(self.buffer, "(PRINTLN ");
                self.visit(expression.clone());
                write!(self.buffer, ")");
            }
            Statement::Block { ref statements, .. } => {
                self.indent();
                write!(self.buffer, "(BLOCK\n");
                self.inc();
                for (i, statement) in statements.iter().enumerate() {
                    if i != 0 { writeln!(self.buffer); }
                    self.visit(statement.clone());
                }
                writeln!(self.buffer);
                self.dec();
                self.indent();
                write!(self.buffer, ")\n");
            }
            Statement::Assign { ref lhs, ref rhs, .. } => {
                self.indent();
                write!(self.buffer, "(EQSIGN ");
                self.visit(lhs.clone());
                write!(self.buffer, " ");
                self.visit(rhs.clone());
                write!(self.buffer, ")");
            },
            Statement::While { ref expression, ref statement, .. } => {
                self.indent();
                write!(self.buffer, "(WHILE ");
                self.visit(expression.clone());
                write!(self.buffer, "\n");
                self.inc();
                self.visit(statement.clone());
                self.dec();
                write!(self.buffer, "\n");
                self.indent();
                write!(self.buffer, ")");
            },
            Statement::SideEffect { ref expression, .. } => {
                self.indent();
                write!(self.buffer, "(SIDEF ");
                self.visit(expression.clone());
                write!(self.buffer, ")");
            },
            Statement::AssignArray { ref lhs, ref in_bracket, ref rhs } => {
                self.indent();
                write!(self.buffer, "(EQSIGN (ARRAY-ASSIGN ");
                self.visit(lhs.clone());
                self.visit(in_bracket.clone());
                write!(self.buffer, ") ");
                self.visit(rhs.clone());
                write!(self.buffer, ")");
            },
            Statement::If { ref condition, ref statement, ref otherwise } => {
                self.indent();
                write!(self.buffer, "(IF ");
                self.visit(condition.clone());
                write!(self.buffer, "\n");

                self.inc();
                self.visit(statement.clone());

                if let Some(otherwise) = otherwise.as_ref() {
                    self.visit(otherwise.clone());
                }
                self.dec();

                writeln!(self.buffer);
                self.indent();
                write!(self.buffer, ")");
            },
        }
    }
}

impl Visitor<Rc<Expression>> for Printer {
    fn visit(&mut self, expression: Rc<Expression>) {
        match *expression {
            Expression::Identifier(ref id) => {
                self.visit(id.clone());
            },
            Expression::IntLiteral(ref token) => {
                write!(self.buffer, "(INTLIT {})", token.text);
            },
            Expression::StringLiteral(ref token) => {
                write!(self.buffer, "(STRINGLIT {})", token.text);
            },
            Expression::Unary(ref unary) => {
                self.visit(unary.clone());
            },
            Expression::Binary(ref binary) => {
                self.visit(binary.clone());
            },
            Expression::NewClass(ref id) => {
                write!(self.buffer, "(NEW-INSTANCE ");
                self.visit(id.clone());
                write!(self.buffer, ")");
            },
            Expression::This => {
                write!(self.buffer, "THIS");
            },
            Expression::TrueLiteral => {
                write!(self.buffer, "TRUE");
            },
            Expression::FalseLiteral => {
                write!(self.buffer, "FALSE");
            },
        }
    }
}

impl Visitor<UnaryExpression> for Printer {
    fn visit(&mut self, unary: UnaryExpression) {
        match unary {
            UnaryExpression::Not(expression) => {
                write!(self.buffer, "(! ");
                self.visit(expression.clone());
                write!(self.buffer, ")");
            },
            UnaryExpression::Application { expression, id, list } => {
                write!(self.buffer, "(DOT ");
                self.visit(expression.clone());
                write!(self.buffer, " (FUN-CALL ");
                self.visit(id);
                for expression in list.iter() {
                    self.visit(expression.clone());
                }
                write!(self.buffer, "))");
            },
            UnaryExpression::NewArray(expression) => {
                write!(self.buffer, "(NEW-INT-ARRAY ");
                self.visit(expression.clone());
                write!(self.buffer, ")");
            },
            UnaryExpression::Length(expression) => {
                write!(self.buffer, "(DOT ");
                self.visit(expression.clone());
                write!(self.buffer, " LENGTH)");
            },
            UnaryExpression::Parentheses(expression) => {
                self.visit(expression.clone());
            },
        }
    }
}

impl Visitor<BinaryExpression> for Printer {
    fn visit(&mut self, binary: BinaryExpression) {
        write!(self.buffer, "(");
        match binary.kind {
            BinaryKind::LessThan => write!(self.buffer, "<"),
            BinaryKind::Equals => write!(self.buffer, "EQUALS"),
            BinaryKind::And => write!(self.buffer, "&&"),
            BinaryKind::Or => write!(self.buffer, "||"),
            BinaryKind::Plus => write!(self.buffer, "PLUS"),
            BinaryKind::Minus => write!(self.buffer, "-"),
            BinaryKind::Times => write!(self.buffer, "*"),
            BinaryKind::Divide => write!(self.buffer, "/"),
            BinaryKind::ArrayLookup => write!(self.buffer, "ARRAY-LOOKUP"),
        };
        write!(self.buffer, " ");
        self.visit(binary.lhs);
        write!(self.buffer, " ");
        self.visit(binary.rhs);
        write!(self.buffer, ")");
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
                body: Statement::Block {
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

        let mut printer = Printer::new();
        printer.visit(&program);
    }
}
