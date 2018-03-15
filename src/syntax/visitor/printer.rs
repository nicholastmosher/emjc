#![allow(unused_must_use)]

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

impl Visitor<Program> for Printer {
    fn visit(&mut self, program: &Program) {
        self.visit(&program.main);
        writeln!(self.buffer);
        for class in program.classes.iter() {
            self.visit(class);
        }
        writeln!(self.buffer);
    }
}

impl Visitor<Main> for Printer {
    fn visit(&mut self, main: &Main) {
        write!(self.buffer, "(MAIN-CLASS-DECL ");
        self.visit(&main.id);
        writeln!(self.buffer);

        self.inc();
        self.indent();
        write!(self.buffer, "(MAIN-FUN-CALL (STRING-ARRAY ");
        self.visit(&main.args);
        write!(self.buffer, ")\n");

        self.inc();
        self.visit(&main.body);
        self.dec();

        write!(self.buffer, "\n");
        self.indent();
        write!(self.buffer, ")\n");
        self.dec();
        self.indent();
        write!(self.buffer, ")");
    }
}

impl Visitor<Class> for Printer {
    fn visit(&mut self, class: &Class) {
        write!(self.buffer, "(CLASS-DECL ");
        self.visit(&class.id);

        if let Some(ref extends) = class.extends {
            write!(self.buffer, " ");
            self.visit(extends);
        }

        write!(self.buffer, "\n");
        self.inc();

        for var in class.variables.iter() {
            self.visit(var);
            write!(self.buffer, "\n");
        }
        for method in class.functions.iter() {
            self.visit(method);
            write!(self.buffer, "\n");
        }

        self.dec();
        self.indent();
        write!(self.buffer, ")");
    }
}

impl Visitor<Identifier> for Printer {
    fn visit(&mut self, id: &Identifier) {
        write!(self.buffer, "(ID {})", id.0.text);
    }
}

impl Visitor<Extends> for Printer {
    fn visit(&mut self, extends: &Extends) {
        write!(self.buffer, "(EXTENDS ");
        self.visit(&extends.extended);
        write!(self.buffer, ")");
    }
}

impl Visitor<Variable> for Printer {
    fn visit(&mut self, variable: &Variable) {
        self.indent();
        write!(self.buffer, "(VAR-DECL ");
        self.visit(&variable.kind);
        write!(self.buffer, " ");
        self.visit(&variable.name);
        write!(self.buffer, ")");
    }
}

impl Visitor<Function> for Printer {
    fn visit(&mut self, function: &Function) {
        self.indent();
        write!(self.buffer, "(MTD-DECL ");
        self.visit(&function.kind);
        write!(self.buffer, " ");
        self.visit(&function.name);

        write!(self.buffer, " (TY-ID-LIST");
        for arg in function.args.iter() {
            write!(self.buffer, " ");
            self.visit(arg);
        }
        write!(self.buffer, ")\n");

        self.inc();
        for v in function.variables.iter() {
            self.visit(v);
            write!(self.buffer, "\n");
        }
        for s in function.statements.iter() {
            self.visit(s);
            write!(self.buffer, "\n");
        }
        self.dec();

        self.indent();
        write!(self.buffer, ")");
    }
}

impl Visitor<Type> for Printer {
    fn visit(&mut self, kind: &Type) {
        match *kind {
            Type::Int => { write!(self.buffer, "INT"); },
            Type::IntArray => { write!(self.buffer, "INT-ARRAY"); },
            Type::String => { write!(self.buffer, "STRING"); },
            Type::Boolean => { write!(self.buffer, "BOOLEAN"); },
            Type::Id(ref id) => self.visit(id),
        };
    }
}

impl Visitor<Argument> for Printer {
    fn visit(&mut self, argument: &Argument) {
        write!(self.buffer, "(");
        self.visit(&argument.kind);
        write!(self.buffer, " ");
        self.visit(&argument.name);
        write!(self.buffer, ")");
    }
}

impl Visitor<Statement> for Printer {
    fn visit(&mut self, statement: &Statement) {
        match *statement {
            Statement::Print { ref expression, .. } => {
                self.indent();
                write!(self.buffer, "(PRINTLN ");
                self.visit(expression);
                write!(self.buffer, ")");
            }
            Statement::Block { ref statements, .. } => {
                self.indent();
                write!(self.buffer, "(BLOCK\n");
                self.inc();
                for (i, statement) in statements.iter().enumerate() {
                    if i != 0 { writeln!(self.buffer); }
                    self.visit(statement);
                }
                writeln!(self.buffer);
                self.dec();
                self.indent();
                write!(self.buffer, ")\n");
            }
            Statement::Assign { ref lhs, ref rhs, .. } => {
                self.indent();
                write!(self.buffer, "(EQSIGN ");
                self.visit(lhs);
                write!(self.buffer, " ");
                self.visit(rhs);
                write!(self.buffer, ")");
            },
            Statement::While { ref expression, ref statement, .. } => {
                self.indent();
                write!(self.buffer, "(WHILE ");
                self.visit(expression);
                write!(self.buffer, "\n");
                self.inc();
                self.visit(statement);
                self.dec();
                write!(self.buffer, "\n");
                self.indent();
                write!(self.buffer, ")");
            },
            Statement::SideEffect { ref expression, .. } => {
                self.indent();
                write!(self.buffer, "(SIDEF ");
                self.visit(expression);
                write!(self.buffer, ")");
            },
            Statement::AssignArray { ref lhs, ref in_bracket, ref rhs } => {
                self.indent();
                write!(self.buffer, "(EQSIGN (ARRAY-ASSIGN ");
                self.visit(lhs);
                self.visit(in_bracket);
                write!(self.buffer, ") ");
                self.visit(rhs);
                write!(self.buffer, ")");
            },
            Statement::If { ref condition, ref statement, ref otherwise } => {
                self.indent();
                write!(self.buffer, "(IF ");
                self.visit(condition);
                write!(self.buffer, "\n");

                self.inc();
                self.visit(statement);

                if let Some(otherwise) = otherwise.as_ref() {
                    self.visit(otherwise);
                }
                self.dec();

                writeln!(self.buffer);
                self.indent();
                write!(self.buffer, ")");
            },
        }
    }
}

impl Visitor<Box<Statement>> for Printer {
    fn visit(&mut self, statement: &Box<Statement>) {
        self.visit(statement.as_ref());
    }
}

impl Visitor<Expression> for Printer {
    fn visit(&mut self, expression: &Expression) {
        match *expression {
            Expression::Identifier(ref id) => {
                self.visit(id);
            },
            Expression::IntLiteral(ref token) => {
                write!(self.buffer, "(INTLIT {})", token.text);
            },
            Expression::StringLiteral(ref token) => {
                write!(self.buffer, "(STRINGLIT {})", token.text);
            },
            Expression::Unary(ref unary) => {
                self.visit(unary);
            },
            Expression::Binary(ref binary) => {
                self.visit(binary);
            },
            Expression::NewClass(ref id) => {
                write!(self.buffer, "(NEW-INSTANCE ");
                self.visit(id);
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

impl Visitor<Box<Expression>> for Printer {
    fn visit(&mut self, expression: &Box<Expression>) {
        self.visit(expression.as_ref());
    }
}

impl Visitor<UnaryExpression> for Printer {
    fn visit(&mut self, unary: &UnaryExpression) {
        match *unary {
            UnaryExpression::Not(ref expression) => {
                write!(self.buffer, "(! ");
                self.visit(expression);
                write!(self.buffer, ")");
            },
            UnaryExpression::Application { ref expression, ref id, ref list } => {
                write!(self.buffer, "(DOT ");
                self.visit(expression);
                write!(self.buffer, " (FUN-CALL ");
                self.visit(id);
                for expression in list.0.iter() {
                    self.visit(expression);
                }
                write!(self.buffer, "))");
            },
            UnaryExpression::NewArray(ref expression) => {
                write!(self.buffer, "(NEW-INT-ARRAY ");
                self.visit(expression);
                write!(self.buffer, ")");
            },
            UnaryExpression::Length(ref expression) => {
                write!(self.buffer, "(DOT ");
                self.visit(expression);
                write!(self.buffer, " LENGTH)");
            },
            UnaryExpression::Parentheses(ref expression) => {
                self.visit(expression);
            },
        }
    }
}

impl Visitor<BinaryExpression> for Printer {
    fn visit(&mut self, binary: &BinaryExpression) {
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
        self.visit(&binary.lhs);
        write!(self.buffer, " ");
        self.visit(&binary.rhs);
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
