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
    pub fn print(&mut self, program: &Rc<Program>) {
        self.visit(program.clone());
        print!("{}", self.buffer);
    }

    fn indent(&mut self) { for _ in 0..self.indent { write!(self.buffer, "\t"); } }
    fn inc(&mut self) { self.indent += 1; }
    fn dec(&mut self) { if self.indent > 0 { self.indent -= 1; } }
}

impl Visitor<Rc<Program>> for Printer {
    fn visit(&mut self, program: Rc<Program>) {
        let main = program.main.clone();
        write!(self.buffer, "(MAIN-CLASS-DECL ");
        self.visit(main.id.clone());
        writeln!(self.buffer);

        self.inc();
        self.indent();
        write!(self.buffer, "(MAIN-FUN-CALL (STRING-ARRAY ");
        self.visit(main.functions[0].args[0].name.clone());
        write!(self.buffer, ")\n");

        self.inc();
        self.visit(main.functions[0].statements[0].clone());
        self.dec();

        write!(self.buffer, "\n");
        self.indent();
        write!(self.buffer, ")\n");
        self.dec();
        self.indent();
        write!(self.buffer, ")");


        writeln!(self.buffer);
        for class in program.classes.iter() {
            self.visit(class.clone());
        }
        writeln!(self.buffer);
    }
}

impl Visitor<Rc<Class>> for Printer {
    fn visit(&mut self, class: Rc<Class>) {
        write!(self.buffer, "(CLASS-DECL ");
        self.visit(class.id.clone());

        if let Some(ref extends) = class.extends {
            write!(self.buffer, " (EXTENDS ");
            self.visit(extends.clone());
            write!(self.buffer, ")");
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
        writeln!(self.buffer, ")");
    }
}

impl Visitor<Rc<Identifier>> for Printer {
    fn visit(&mut self, id: Rc<Identifier>) {
        write!(self.buffer, "(ID {})", id.text);
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
//            self.visit(arg.clone());
            write!(self.buffer, "(");
            self.visit(arg.kind.clone());
            write!(self.buffer, " ");
            self.visit(arg.name.clone());
            write!(self.buffer, ")");
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

        if let Some(ref return_expr) = function.expression {
            self.indent();
            write!(self.buffer, "(RETURN ");
            self.visit(return_expr.clone());
            writeln!(self.buffer, ")");
        }

        self.dec();
        self.indent();
        write!(self.buffer, ")");
    }
}

impl Visitor<Rc<Type>> for Printer {
    fn visit(&mut self, kind: Rc<Type>) {
        match *kind {
            Type::Void => { write!(self.buffer, "VOID"); },
            Type::Int => { write!(self.buffer, "INT"); },
            Type::IntArray => { write!(self.buffer, "INT-ARRAY"); },
            Type::String => { write!(self.buffer, "STRING"); },
            Type::StringArray => { write!(self.buffer, "STRING-ARRAY"); },
            Type::Boolean => { write!(self.buffer, "BOOLEAN"); },
            Type::Id(ref id) => self.visit(id.clone()),
        };
    }
}

impl Visitor<Rc<Statement>> for Printer {
    fn visit(&mut self, statement: Rc<Statement>) {
        match **statement {
            Stmt::Print { ref expression, .. } => {
                self.indent();
                write!(self.buffer, "(PRINTLN ");
                self.visit(expression.clone());
                write!(self.buffer, ")");
            }
            Stmt::Block { ref statements, .. } => {
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
                write!(self.buffer, ")");
            }
            Stmt::Assign { ref lhs, ref rhs, .. } => {
                self.indent();
                write!(self.buffer, "(EQSIGN ");
                self.visit(lhs.clone());
                write!(self.buffer, " ");
                self.visit(rhs.clone());
                write!(self.buffer, ")");
            },
            Stmt::While { ref expression, ref statement, .. } => {
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
            Stmt::SideEffect { ref expression, .. } => {
                self.indent();
                write!(self.buffer, "(SIDEF ");
                self.visit(expression.clone());
                write!(self.buffer, ")");
            },
            Stmt::AssignArray { ref lhs, ref in_bracket, ref rhs } => {
                self.indent();
                write!(self.buffer, "(EQSIGN (ARRAY-ASSIGN ");
                self.visit(lhs.clone());
                self.visit(in_bracket.clone());
                write!(self.buffer, ") ");
                self.visit(rhs.clone());
                write!(self.buffer, ")");
            },
            Stmt::If { ref condition, ref statement, ref otherwise } => {
                self.indent();
                write!(self.buffer, "(IF ");
                self.visit(condition.clone());
                write!(self.buffer, "\n");

                self.inc();
                self.visit(statement.clone());

                if let Some(otherwise) = otherwise.as_ref() {
                    writeln!(self.buffer);
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
        match **expression {
            Expr::Identifier(ref id) => {
                self.visit(id.clone());
            },
            Expr::IntLiteral(ref token) => {
                write!(self.buffer, "(INTLIT {})", token.text);
            },
            Expr::StringLiteral(ref token) => {
                write!(self.buffer, "(STRINGLIT {})", token.text);
            },
            Expr::Unary(ref unary) => {
                self.visit(unary.clone());
            },
            Expr::Binary(ref binary) => {
                self.visit(binary.clone());
            },
            Expr::NewClass(ref id) => {
                write!(self.buffer, "(NEW-INSTANCE ");
                self.visit(id.clone());
                write!(self.buffer, ")");
            },
            Expr::This => {
                write!(self.buffer, "THIS");
            },
            Expr::TrueLiteral => {
                write!(self.buffer, "TRUE");
            },
            Expr::FalseLiteral => {
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
            UnaryExpression::ArrayLookup { ref lhs, ref index, .. } => {
                write!(self.buffer, "(ARRAY-LOOKUP ");
                self.visit(lhs.clone());
                write!(self.buffer, " ");
                self.visit(index.clone());
                write!(self.buffer, ")");
            }
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
//            BinaryKind::ArrayLookup => write!(self.buffer, "ARRAY-LOOKUP"),
        };
        write!(self.buffer, " ");
        self.visit(binary.lhs);
        write!(self.buffer, " ");
        self.visit(binary.rhs);
        write!(self.buffer, ")");
    }
}
