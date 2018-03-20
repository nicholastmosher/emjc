use Result;
use lexer::{
    Lexer,
    TokenType,
};

use super::ast::*;

pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Parser { lexer }
    }
}

impl Parser {

    pub fn parse_program(&mut self) -> Result<Program> {
        info!("Parsing program");
        let main = self.parse_main()?;
        let mut classes = vec![];
        while let TokenType::CLASS = self.lexer.peek() {
            classes.push(self.parse_class()?)
        }
        Ok(Program::new(main, classes))
    }

    fn parse_main(&mut self) -> Result<Main> {
        info!("Parsing main");
        self.lexer.munch(TokenType::CLASS)?;
        let id = self.parse_identifier()?;
        self.lexer.munch_by(TokenType::LBRACE, "main")?;
        self.lexer.munch_by(TokenType::PUBLIC, "main")?;
        self.lexer.munch_by(TokenType::STATIC, "main")?;
        self.lexer.munch_by(TokenType::VOID, "main")?;
        self.lexer.munch_by(TokenType::MAIN, "main")?;
        self.lexer.munch_by(TokenType::LPAREN, "main")?;
        self.lexer.munch_by(TokenType::STRING, "main")?;
        self.lexer.munch_by(TokenType::LBRACKET, "main")?;
        self.lexer.munch_by(TokenType::RBRACKET, "main")?;
        let args = self.parse_identifier()?;
        self.lexer.munch_by(TokenType::RPAREN, "main")?;
        self.lexer.munch_by(TokenType::LBRACE, "main")?;
        let body = self.parse_statement()?;
        self.lexer.munch_by(TokenType::RBRACE, "main")?;
        self.lexer.munch_by(TokenType::RBRACE, "main")?;
        Ok(Main::new(id, args, body))
    }

    fn parse_class(&mut self) -> Result<Class> {
        info!("Parsing class");
        let result: Result<Class> = (|| {
            self.lexer.munch_by(TokenType::CLASS, "class")?;
            let id = self.parse_identifier()?;

            // Parse the extends clause if it's present
            let extends = if let TokenType::EXTENDS = self.lexer.peek() {
                self.lexer.munch_by(TokenType::EXTENDS, "class")?;
                let extended = self.parse_identifier()?;
                Some(Extends::new(extended))
            } else { None };
            self.lexer.munch_by(TokenType::LBRACE, "class")?;

            // Parse variables of the class
            let variables = self.parse_variables()?;

            // Parse functions of the class
            let mut functions = Vec::new();
            while FUNCTION_FIRST.contains(&self.lexer.peek()) {
                let f = self.parse_function()?;
                functions.push(f);
            }

            Ok(Class::new(id, extends, variables, functions))
        })();

        match result {
            Ok(class) => Ok(class),
            Err(e) => Err(format_err!("class -> {}", e)),
        }
    }

    fn parse_identifier(&mut self) -> Result<Identifier> {
        info!("Parsing identifier");
        let id = self.lexer.munch_by(TokenType::ID, "identifier")?;
        Ok(id.into())
    }

    fn parse_variables(&mut self) -> Result<Vec<Variable>> {
        info!("Parsing variables");
        let mut variables = vec![];
        while TYPE_FIRST.contains(&self.lexer.peek()) {
            let v = self.parse_variable()?;
            variables.push(v);
        }
        Ok(variables)
    }

    fn parse_variable(&mut self) -> Result<Variable> {
        info!("Parsing variable");
        let result: Result<Variable> = (|| {
            let kind = self.parse_type()?;
            let name = self.parse_identifier()?;
            self.lexer.munch_by(TokenType::SEMICOLON, "variable")?;
            Ok(Variable::new(kind, name))
        })();

        match result {
            Ok(variable) => Ok(variable),
            Err(e) => Err(format_err!("variable -> {}", e)),
        }
    }

    fn parse_function(&mut self) -> Result<Function> {
        info!("Parsing function");
        let result: Result<Function> = (|| {
            self.lexer.munch_by(TokenType::PUBLIC, "function")?;
            let kind = self.parse_type()?;
            let name = self.parse_identifier()?;
            self.lexer.munch_by(TokenType::LPAREN, "function")?;
            let args = self.parse_arguments()?;
            self.lexer.munch_by(TokenType::RPAREN, "function")?;
            self.lexer.munch_by(TokenType::LBRACE, "function")?;

            let mut variables = vec![];
            loop {
                match self.lexer.peek() {
                    TokenType::STRING | TokenType::BOOLEAN | TokenType::INT => {
                        let v = self.parse_variable()?;
                        variables.push(v);
                    },
                    TokenType::ID => {
                        if self.lexer.peek_num(1) == TokenType::ID {
                            let v = self.parse_variable()?;
                            variables.push(v);
                        } else {
                            // Stop parsing variables if we have ID [^ID]
                            break;
                        }
                    },
                    // If the next token is not the start of a variable, move to statements
                    _ => break,
                }
            }

            let mut statements = vec![];
            while STATEMENT_FIRST.contains(&self.lexer.peek()) {
                let s = self.parse_statement()?;
                statements.push(s);
            }

            self.lexer.munch_by(TokenType::RETURN, "function")?;
            let expression = self.parse_expression()?;
            self.lexer.munch_by(TokenType::SEMICOLON, "function")?;
            self.lexer.munch_by(TokenType::RBRACE, "function")?;

            Ok(Function::new(kind, name, args, variables, statements, expression))
        })();

        match result {
            Ok(f) => Ok(f),
            Err(e) => Err(format_err!("function -> {}", e)),
        }
    }

    fn parse_type(&mut self) -> Result<Type> {
        info!("Parsing type");
        let ty = match self.lexer.peek() {
            TokenType::ID => {
                let id = self.parse_identifier()?;
                Type::Id(id.into())
            },
            TokenType::BOOLEAN => {
                self.lexer.munch_by(TokenType::BOOLEAN, "type")?;
                Type::Boolean
            },
            TokenType::STRING => {
                self.lexer.munch_by(TokenType::STRING, "type")?;
                Type::String
            },
            TokenType::INT => {
                self.lexer.munch_by(TokenType::INT, "type")?;
                match self.lexer.peek() {
                    TokenType::LBRACKET => {
                        self.lexer.munch_by(TokenType::LBRACKET, "type")?;
                        self.lexer.munch_by(TokenType::RBRACKET, "type")?;
                        Type::IntArray
                    },
                    _ => Type::Int,
                }
            },
            _ => Err(format_err!("Unexpected token {} while parsing type", self.lexer.peek()))?,
        };
        Ok(ty)
    }

    fn parse_arguments(&mut self) -> Result<Vec<Argument>> {
        info!("Parsing arguments");

        let mut args = vec![];
        if !TYPE_FIRST.contains(&self.lexer.peek()) { return Ok(args) }

        loop {
            if args.len() != 0 {
                if self.lexer.peek() != TokenType::COMMA { break }
                self.lexer.munch(TokenType::COMMA)?;
            }
            let kind = self.parse_type()?;
            let name = self.parse_identifier()?;
            let arg = Argument::new(kind, name);
            args.push(arg);
        }

        Ok(args)
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        info!("Parsing statement");
        let result: Result<Statement> = (|| {
            let stmt = match self.lexer.peek() {
                TokenType::LBRACE => {
                    self.lexer.munch_by(TokenType::LBRACE, "statement_block")?;
                    let mut statements = vec![];
                    while STATEMENT_FIRST.contains(&self.lexer.peek()) {
                        let s = self.parse_statement()?;
                        statements.push(s);
                    }
                    self.lexer.munch_by(TokenType::RBRACE, "statement_block")?;
                    Statement::new_block(statements)
                },
                TokenType::WHILE => {
                    self.lexer.munch_by(TokenType::WHILE, "statement_while")?;
                    self.lexer.munch_by(TokenType::LPAREN, "statement_while")?;
                    let expression = self.parse_expression()?;
                    self.lexer.munch_by(TokenType::RPAREN, "statement_while")?;
                    let statement = self.parse_statement()?;
                    Statement::new_while(expression, statement)
                },
                TokenType::PRINTLN => {
                    self.lexer.munch_by(TokenType::PRINTLN, "statement_println")?;
                    self.lexer.munch_by(TokenType::LPAREN, "statement_println")?;
                    let expression = self.parse_expression()?;
                    self.lexer.munch_by(TokenType::RPAREN, "statement_println")?;
                    self.lexer.munch_by(TokenType::SEMICOLON, "statement_println")?;
                    Statement::new_print(expression)
                },
                TokenType::ID => {
                    let id = self.parse_identifier()?;
                    match self.lexer.peek() {
                        TokenType::EQSIGN => self.parse_assign_statement(id)?,
                        TokenType::LBRACKET => self.parse_array_assign(id)?,
                        _ => Err(format_err!("Unexpected token while parsing statement->ID"))?,
                    }
                },
                TokenType::SIDEF => {
                    self.lexer.munch_by(TokenType::SIDEF, "statement_sidef")?;
                    self.lexer.munch_by(TokenType::LPAREN, "statement_sidef")?;
                    let expression = self.parse_expression()?;
                    self.lexer.munch_by(TokenType::RPAREN, "statement_sidef")?;
                    self.lexer.munch_by(TokenType::SEMICOLON, "statement_sidef")?;
                    Statement::new_sidef(expression)
                },
                TokenType::IF => {
                    self.lexer.munch_by(TokenType::IF, "statement_if")?;
                    self.lexer.munch_by(TokenType::LPAREN, "statement_if")?;
                    let condition = self.parse_expression()?;
                    self.lexer.munch_by(TokenType::RPAREN, "statement_if")?;
                    let statement = self.parse_statement()?;
                    let otherwise = if self.lexer.peek() == TokenType::ELSE {
                        self.lexer.munch_by(TokenType::ELSE, "statement_if")?;
                        let statement = self.parse_statement()?;
                        Some(statement)
                    } else { None };

                    Statement::new_if(condition, statement, otherwise)
                },
                _ => Err(format_err!("Unexpected token {} while parsing statement", self.lexer.peek()))?,
            };
            Ok(stmt)
        })();
        result.map_err(|e| format_err!("statement -> {}", e))
    }

    fn parse_assign_statement(&mut self, lhs: Identifier) -> Result<Statement> {
        info!("Parsing assign statement");
        self.lexer.munch(TokenType::EQSIGN)?;
        let rhs = self.parse_expression()?;
        self.lexer.munch(TokenType::SEMICOLON)?;
        Ok(Statement::new_assign(lhs, rhs))
    }

    fn parse_array_assign(&mut self, lhs: Identifier) -> Result<Statement> {
        info!("Parsing array assign");
        self.lexer.munch_by(TokenType::LBRACKET, "array assign")?;
        let in_bracket = self.parse_expression()?;
        self.lexer.munch_by(TokenType::RBRACKET, "array assign")?;
        self.lexer.munch_by(TokenType::EQSIGN, "array assign")?;
        let rhs = self.parse_expression()?;
        self.lexer.munch_by(TokenType::SEMICOLON, "array assign")?;
        Ok(Statement::new_assign_array(lhs, in_bracket, rhs))
    }

    fn parse_expression(&mut self) -> Result<Expression> {
        info!("Parsing expression");
        let lhs = self.parse_and_term()?.associate_left();
        let expr = match self.lexer.peek() {
            TokenType::OR => {
                self.lexer.munch_by(TokenType::OR, "or_term")?;
                let rhs = self.parse_expression()?;
                let binary = BinaryExpression {
                    kind: BinaryKind::Or,
                    lhs: lhs.into(),
                    rhs: rhs.into(),
                };
                Expression::Binary(binary)
            },
            _ => lhs,
        };
        Ok(expr)
    }

    fn parse_and_term(&mut self) -> Result<Expression> {
        info!("Parsing and_term");
        let lhs = self.parse_cmp_term()?.associate_left();
        let expr = match self.lexer.peek() {
            TokenType::AND => {
                self.lexer.munch_by(TokenType::AND, "and_term")?;
                let rhs = self.parse_and_term()?;
                BinaryExpression {
                    kind: BinaryKind::And,
                    lhs: lhs.into(),
                    rhs: rhs.into(),
                }.into()
            },
            _ => lhs,
        };
        Ok(expr)
    }

    fn parse_cmp_term(&mut self) -> Result<Expression> {
        info!("Parsing cmp_term");
        let lhs = self.parse_plus_minus_term()?.associate_left();
        let expr = match self.lexer.peek() {
            TokenType::EQUALS => {
                self.lexer.munch_by(TokenType::EQUALS, "less than")?;
                let rhs = self.parse_cmp_term()?;
                BinaryExpression {
                    kind: BinaryKind::Equals,
                    lhs: lhs.into(),
                    rhs: rhs.into(),
                }.into()
            },
            TokenType::LESSTHAN => {
                self.lexer.munch_by(TokenType::LESSTHAN, "less than")?;
                let rhs = self.parse_cmp_term()?.associate_left();
                BinaryExpression {
                    kind: BinaryKind::LessThan,
                    lhs: lhs.into(),
                    rhs: rhs.into(),
                }.into()
            },
            _ => lhs,
        };
        Ok(expr)
    }

    fn parse_plus_minus_term(&mut self) -> Result<Expression> {
        info!("Parsing plus_minus_term");
        let lhs = self.parse_times_div_term()?.associate_left();
        let expr = match self.lexer.peek() {
            TokenType::PLUS => {
                self.lexer.munch_by(TokenType::PLUS, "plus")?;
                let rhs = self.parse_plus_minus_term()?;
                BinaryExpression {
                    kind: BinaryKind::Plus,
                    lhs: lhs.into(),
                    rhs: rhs.into(),
                }.into()
            },
            TokenType::MINUS => {
                self.lexer.munch_by(TokenType::MINUS, "minus")?;
                let rhs = self.parse_plus_minus_term()?;
                BinaryExpression {
                    kind: BinaryKind::Minus,
                    lhs: lhs.into(),
                    rhs: rhs.into(),
                }.into()
            },
            _ => lhs,
        };
        Ok(expr)
    }

    fn parse_times_div_term(&mut self) -> Result<Expression> {
        info!("Parsing times_div_term");
        let lhs = self.parse_postfix_term()?;
        let expr = match self.lexer.peek() {
            TokenType::TIMES => {
                self.lexer.munch_by(TokenType::TIMES, "div")?;
                let rhs = self.parse_times_div_term()?;
                BinaryExpression {
                    kind: BinaryKind::Times,
                    lhs: lhs.into(),
                    rhs: rhs.into(),
                }.into()
            },
            TokenType::DIV => {
                self.lexer.munch_by(TokenType::DIV, "div")?;
                let rhs = self.parse_times_div_term()?;
                BinaryExpression {
                    kind: BinaryKind::Divide,
                    lhs: lhs.into(),
                    rhs: rhs.into(),
                }.into()
            },
            _ => lhs,
        };
        Ok(expr)
    }

    fn parse_postfix_term(&mut self) -> Result<Expression> {
        info!("Parsing postfix_term");
        let base = self.parse_base_expression()?;
        let expr = match self.lexer.peek() {
            TokenType::LBRACKET => {
                self.lexer.munch_by(TokenType::LBRACKET, "postfix")?;
                let rhs = self.parse_expression()?;
                self.lexer.munch_by(TokenType::RBRACKET, "postfix")?;
                BinaryExpression {
                    kind: BinaryKind::ArrayLookup,
                    lhs: base.into(),
                    rhs: rhs.into(),
                }.into()
            },
            TokenType::DOT => {
                self.lexer.munch_by(TokenType::DOT, "postfix")?;
                match self.lexer.peek() {
                    TokenType::LENGTH => {
                        self.lexer.munch_by(TokenType::LENGTH, "postfix")?;
                        Expression::new_length(base)
                    },
                    TokenType::ID => {
                        let id = self.parse_identifier()?;
                        self.lexer.munch_by(TokenType::LPAREN, "postfix")?;
                        let list = self.parse_expression_list()?;
                        self.lexer.munch_by(TokenType::RPAREN, "postfix")?;
                        Expression::new_application(base, id, list)
                    },
                    _ => Err(format_err!("Expected LENGTH or ID, found {}", self.lexer.peek()))?,
                }
            },
            _ => base,
        };
        Ok(expr)
    }

    fn parse_base_expression(&mut self) -> Result<Expression> {
        info!("Parsing base expression");
        let result: Result<Expression> = (|| {
            let expr = match self.lexer.peek() {
                TokenType::NEW => {
                    self.lexer.munch_by(TokenType::NEW, "expression")?;
                    match self.lexer.peek() {
                        TokenType::INT => self.parse_expression_new_array()?,
                        TokenType::ID => {
                            let id = self.parse_identifier()?;
                            self.lexer.munch_by(TokenType::LPAREN, "expression")?;
                            self.lexer.munch_by(TokenType::RPAREN, "expression")?;
                            Expression::new_class(id)
                        },
                        t => Err(format_err!("Unexpected token while parsing expression: {}", t))?,
                    }
                },
                TokenType::STRINGLIT => {
                    let string = self.lexer.munch_by(TokenType::STRINGLIT, "expression")?;
                    Expression::new_stringlit(string)
                },
                TokenType::INTLIT => {
                    let int = self.lexer.munch_by(TokenType::INTLIT, "expression")?;
                    Expression::new_intlit(int)
                },
                TokenType::TRUE => {
                    self.lexer.munch_by(TokenType::TRUE, "expression")?;
                    Expression::TrueLiteral
                },
                TokenType::FALSE => {
                    self.lexer.munch_by(TokenType::FALSE, "expression")?;
                    Expression::FalseLiteral
                },
                TokenType::ID => {
                    let id = self.parse_identifier()?;
                    Expression::new_identifier(id)
                },
                TokenType::THIS => {
                    self.lexer.munch_by(TokenType::THIS, "expression")?;
                    Expression::This
                },
                TokenType::BANG => {
                    self.lexer.munch_by(TokenType::BANG, "expression")?;
                    let e = self.parse_expression()?;
                    Expression::new_not(e)
                },
                TokenType::LPAREN => {
                    self.lexer.munch_by(TokenType::LPAREN, "expression")?;
                    let e = self.parse_expression()?;
                    self.lexer.munch_by(TokenType::RPAREN, "expression")?;
                    Expression::new_parentheses(e)
                },
                _ => Err(format_err!("Unexpected token {} while parsing statement", self.lexer.peek()))?,
            };
            Ok(expr)
        })();
        result.map_err(|e| format_err!("expression -> {}", e))
    }

    fn parse_expression_new_array(&mut self) -> Result<Expression> {
        info!("Parsing expression new array");
        self.lexer.munch_by(TokenType::INT, "expression_new_array")?;
        self.lexer.munch_by(TokenType::LBRACKET, "expression_new_array")?;
        let in_brackets = self.parse_expression()?;
        self.lexer.munch_by(TokenType::RBRACKET, "expression_new_array")?;
        Ok(Expression::new_array(in_brackets))
    }

    fn parse_expression_list(&mut self) -> Result<Vec<Expression>> {
        let mut exprs = vec![];
        if self.lexer.peek() == TokenType::RPAREN { return Ok(exprs) }

        loop {
            let e = self.parse_expression()?;
            exprs.push(e);

            if self.lexer.peek() != TokenType::COMMA { break }
            self.lexer.munch(TokenType::COMMA)?;
        }

        Ok(exprs)
    }
}

static EXPRESSION_FIRST: [TokenType; 9] = [
    TokenType::INTLIT,
    TokenType::STRINGLIT,
    TokenType::TRUE,
    TokenType::FALSE,
    TokenType::ID,
    TokenType::THIS,
    TokenType::NEW,
    TokenType::BANG,
    TokenType::LPAREN,
];

static EXTENSION_FIRST: [TokenType; 10] = [
    TokenType::OR,
    TokenType::AND,
    TokenType::EQUALS,
    TokenType::LESSTHAN,
    TokenType::PLUS,
    TokenType::MINUS,
    TokenType::TIMES,
    TokenType::DIV,
    TokenType::LBRACKET,
    TokenType::DOT,
];

static TYPE_FIRST: [TokenType; 4] = [
    TokenType::ID,
    TokenType::BOOLEAN,
    TokenType::STRING,
    TokenType::INT,
];

static FUNCTION_FIRST: [TokenType; 1] = [
    TokenType::PUBLIC,
];

static STATEMENT_FIRST: [TokenType; 6] = [
    TokenType::LBRACE,
    TokenType::WHILE,
    TokenType::ID,
    TokenType::PRINTLN,
    TokenType::SIDEF,
    TokenType::IF,
];
