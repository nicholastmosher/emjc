use std::fmt::Write;
use std::rc::Rc;
use syntax::visitor::Visitor;
use syntax::ast::*;

/// As the pretty printer traverses the AST, it assigns new symbols to all
/// identifiers which represent declarations.
pub struct PrettyPrinter {
    buffer: String,
    indent: usize,
}

impl PrettyPrinter {
    pub fn new() -> PrettyPrinter {
        PrettyPrinter {
            buffer: String::new(),
            indent: 0,
        }
    }

    pub fn print(&mut self, program: &Rc<Program>) {
        self.visit(program.clone());
        debug!("About to pretty print program");
        print!("{}", self.buffer);
    }

    pub fn contents(self) -> String { self.buffer }

    fn indent(&mut self) {
        for _ in 0..self.indent { write!(self.buffer, "    "); }
    }

    fn inc(&mut self) {
        self.indent += 1;
    }

    fn dec(&mut self) {
        if self.indent > 0 { self.indent -= 1; }
    }
}

impl Visitor<Rc<Program>> for PrettyPrinter {
    fn visit(&mut self, program: Rc<Program>) {
        let main = program.main.clone();
        let id = self.visit(main.id.clone());
        writeln!(self.buffer, "class {} {{", id);

        self.inc();
        self.indent();
        let args = self.visit(main.functions[0].args[0].clone());
        write!(self.buffer, "public static void main({}) {{", args);

        let main_body = main.functions[0].statements[0].clone();
        self.inc();
        if let Stmt::Block { ref statements, .. } = **main_body {
            writeln!(self.buffer, "{{");
            for stmt in statements.iter() {
                self.visit(stmt.clone());
            }
            self.dec();
            self.indent();
            write!(self.buffer, "}}");
        } else {
            writeln!(self.buffer);
            self.visit(main_body.clone());
            self.dec();
            self.indent();
        }

        writeln!(self.buffer, "}}");

        self.dec();
        self.indent();
        writeln!(self.buffer, "}}");
        writeln!(self.buffer);

        for class in program.classes.iter() {
            self.visit(class.clone());
        }
    }
}

impl Visitor<Rc<Class>> for PrettyPrinter {
    fn visit(&mut self, class: Rc<Class>) {
        let id = self.visit(class.id.clone());
        writeln!(self.buffer, "class {} {{", id);

        self.inc();
        for var in class.variables.iter() {
            self.visit(var.clone());
        }

        for func in class.functions.iter() {
            self.visit(func.clone());
        }

        self.dec();
        self.indent();
        writeln!(self.buffer, "}}");
        writeln!(self.buffer);
    }
}

impl Visitor<Rc<Identifier>, String> for PrettyPrinter {
    fn visit(&mut self, id: Rc<Identifier>) -> String {
        match id.get_symbol().as_ref() {
            Some(symbol) => format!("{}", symbol.name),
            None => format!("{}_#error#_", String::from(&id.text)),
        }
    }
}

impl Visitor<Rc<Type>, String> for PrettyPrinter {
    fn visit(&mut self, kind: Rc<Type>) -> String {
        match *kind {
            Type::Void => "void".to_owned(),
            Type::Id(ref id) => self.visit(id.clone()),
            Type::Boolean => "boolean".to_owned(),
            Type::String => "String".to_owned(),
            Type::StringArray => "String[]".to_owned(),
            Type::Int => "int".to_owned(),
            Type::IntArray => "int[]".to_owned(),
        }
    }
}

impl Visitor<Rc<Argument>, String> for PrettyPrinter {
    fn visit(&mut self, arg: Rc<Argument>) -> String {
        format!("{} {}", self.visit(arg.kind.clone()), self.visit(arg.name.clone()))
    }
}

impl Visitor<Rc<Variable>, String> for PrettyPrinter {
    fn visit(&mut self, variable: Rc<Variable>) -> String {
        format!("{} {}", self.visit(variable.kind.clone()), self.visit(variable.name.clone()))
    }
}

impl Visitor<Rc<Statement>> for PrettyPrinter {
    fn visit(&mut self, statement: Rc<Statement>) {
        match **statement {
            Stmt::Assign { ref lhs, ref rhs, .. } => {
                let lhs = self.visit(lhs.clone());
                let rhs = self.visit(rhs.clone());
                self.indent();
                writeln!(self.buffer, "{} = {};", lhs, rhs);
            }
            Stmt::SideEffect { ref expression, .. } => {
                let expr = self.visit(expression.clone());

                self.indent();
                writeln!(self.buffer, "sidef({});", expr);
            }
            Stmt::AssignArray { ref lhs, ref in_bracket, ref rhs, .. } => {
                let lhs = self.visit(lhs.clone());
                let inner = self.visit(in_bracket.clone());
                let rhs = self.visit(rhs.clone());

                self.indent();
                writeln!(self.buffer, "{}[{}] = {};", lhs, inner, rhs);
            }
            Stmt::If { ref condition, ref statement, ref otherwise, .. } => {
                let condition = self.visit(condition.clone());

                self.indent();
                write!(self.buffer, "if({}) ", condition);
                self.visit(statement.clone());

                if let Some(otherwise) = otherwise.as_ref() {
                    self.indent();
                    write!(self.buffer, "else ");
                    self.visit(otherwise.clone());
                }
            }
            Stmt::Block { ref statements } => {
                writeln!(self.buffer, "{{");
                self.inc();

                for stmt in statements.iter() {
                    self.visit(stmt.clone());
                }

                self.dec();
                self.indent();
                writeln!(self.buffer, "}}");
            }
            Stmt::Print { ref expression, .. } => {
                let expr = self.visit(expression.clone());
                self.indent();
                writeln!(self.buffer, "System.out.println({});", expr);
            }
            Stmt::While { ref expression, ref statement } => {
                let expr = self.visit(expression.clone());
                self.indent();
                write!(self.buffer, "while({}) ", expr);
                self.visit(statement.clone());
            }
        }
    }
}

impl Visitor<Rc<Function>> for PrettyPrinter {
    fn visit(&mut self, function: Rc<Function>) {
        let kind = self.visit(function.kind.clone());
        let name = self.visit(function.name.clone());

        self.indent();
        write!(self.buffer, "{} {}(", kind, name);

        for (i, arg) in function.args.iter().enumerate() {
            if i != 0 { write!(self.buffer, ", "); }
            let arg = self.visit(arg.clone());
            write!(self.buffer, "{}", arg);
        }
        writeln!(self.buffer, ") {{");

        self.inc();
        for var in function.variables.iter() {
            self.indent();
            let var = self.visit(var.clone());
            writeln!(self.buffer, "{};", var);
        }

        for stmt in function.statements.iter() {
            self.visit(stmt.clone());
        }

        if let Some(ref ret_expr) = function.expression {
            let ret = self.visit(ret_expr.clone());
            self.indent();
            writeln!(self.buffer, "return {};", ret);
        }

        self.dec();
        self.indent();
        writeln!(self.buffer, "}}");
    }
}

impl Visitor<Rc<Expression>, String> for PrettyPrinter {
    fn visit(&mut self, expression: Rc<Expression>) -> String {
        match **expression {
            Expr::This => "this".to_owned(),
            Expr::TrueLiteral => "true".to_owned(),
            Expr::FalseLiteral => "false".to_owned(),
            Expr::Identifier(ref id) => self.visit(id.clone()),
            Expr::IntLiteral(ref token) => format!("{}", &token.text),
            Expr::StringLiteral(ref token) => String::from(&token.text),
            Expr::NewClass(ref id) => format!("new {}()", self.visit(id.clone())),
            Expr::Unary(ref unary) => self.visit(unary),
            Expr::Binary(ref binary) => {
                let lhs = self.visit(binary.lhs.clone());
                let rhs = self.visit(binary.rhs.clone());
                if binary.kind == BinaryKind::ArrayLookup {
                    format!("{}[{}]", lhs, rhs)
                } else {
                    format!("{} {} {}", lhs, self.visit(&binary.kind), rhs)
                }
            }
        }
    }
}

impl<'a> Visitor<&'a BinaryKind, String> for PrettyPrinter {
    fn visit(&mut self, kind: &'a BinaryKind) -> String {
        match *kind {
            BinaryKind::And => "&&".to_owned(),
            BinaryKind::Or => "||".to_owned(),
            BinaryKind::Equals => "==".to_owned(),
            BinaryKind::LessThan => "<".to_owned(),
            BinaryKind::Plus => "+".to_owned(),
            BinaryKind::Minus => "-".to_owned(),
            BinaryKind::Times => "*".to_owned(),
            BinaryKind::Divide => "/".to_owned(),
            _ => unreachable!(),
        }
    }
}

impl<'a> Visitor<&'a UnaryExpression, String> for PrettyPrinter {
    fn visit(&mut self, unary: &'a UnaryExpression) -> String {
        match *unary {
            UnaryExpression::Application { ref expression, ref id, ref list, .. } => {
                let expr = self.visit(expression.clone());
                let id = self.visit(id.clone());
                let mut list_str = String::new();

                for (i, arg) in list.iter().enumerate() {
                    if i != 0 { list_str.push_str(", "); }
                    list_str.push_str(&self.visit(arg.clone()));
                }

                format!("{}.{}({})", expr, id, list_str)
            }
            UnaryExpression::NewArray(ref array) => {
                format!("new int[{}]", self.visit(array.clone()))
            }
            UnaryExpression::Length(ref expr) => {
                format!("{}.length", self.visit(expr.clone()))
            }
            UnaryExpression::Parentheses(ref expr) => {
                format!("({})", self.visit(expr.clone()))
            }
            ref u => unimplemented!("Unary expression variant {:?}", u)
        }
    }
}
