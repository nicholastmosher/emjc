use Result;
use lexer::{
    Lexer,
    TokenType,
};

use super::ast;
//use parser::ParseError;

pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Parser { lexer }
    }
}

impl Parser {

    pub fn parse_program(&mut self) -> Result<ast::Program> {
        info!("Parsing program");
        let main = self.parse_main()?;
        let mut classes: Vec<ast::Class> = vec![];
        while let TokenType::CLASS = self.lexer.peek() {
            classes.push(self.parse_class()?)
        }
        Ok(ast::Program { main, classes })
    }

    fn parse_main(&mut self) -> Result<ast::Main> {
        info!("Parsing main");
        self.lexer.munch(TokenType::CLASS)?;
        debug!("{:25} munched {}", "main", "CLASS");
        let id = self.parse_identifier()?;
        self.lexer.munch_some(&[
            TokenType::LBRACE,
            TokenType::PUBLIC,
            TokenType::STATIC,
            TokenType::VOID,
            TokenType::MAIN,
            TokenType::LPAREN,
            TokenType::STRING,
            TokenType::LBRACKET,
            TokenType::RBRACKET,
        ])?;
        debug!("{:25} munched {}", "main", "LBRACE");
        debug!("{:25} munched {}", "main", "PUBLIC");
        debug!("{:25} munched {}", "main", "STATIC");
        debug!("{:25} munched {}", "main", "VOID");
        debug!("{:25} munched {}", "main", "MAIN");
        debug!("{:25} munched {}", "main", "LPAREN");
        debug!("{:25} munched {}", "main", "STRING");
        debug!("{:25} munched {}", "main", "LBRACKET");
        debug!("{:25} munched {}", "main", "RBRACKET");
        let args = self.parse_identifier()?;
        debug!("{:25} munched {}", "main", "ID");
        self.lexer.munch_some(&[
            TokenType::RPAREN,
            TokenType::LBRACE,
        ])?;
        debug!("{:25} munched {}", "main", "RPAREN");
        debug!("{:25} munched {}", "main", "LBRACE");
        let body = self.parse_statement()?;
        self.lexer.munch_some(&[
            TokenType::RBRACE,
            TokenType::RBRACE,
        ])?;
        debug!("{:25} munched {}", "main", "RBRACE");
        debug!("{:25} munched {}", "main", "RBRACE");
        Ok(ast::Main { id, args, body })
    }

    fn parse_class(&mut self) -> Result<ast::Class> {
        info!("Parsing class");
        let result: Result<ast::Class> = (|| {
            self.lexer.munch(TokenType::CLASS)?;
            debug!("{:25} munched {}", "class", "CLASS");
            let id = self.parse_identifier()?;

            // Parse the extends clause if it's present
            let extends = if let TokenType::EXTENDS = self.lexer.peek() {
                self.lexer.munch(TokenType::EXTENDS)?;
                debug!("{:25} munched {}", "class", "EXTENDS");
                let extended = self.parse_identifier()?;
                Some(ast::Extends { extended })
            } else { None };

            self.lexer.munch(TokenType::LBRACE)?;
            debug!("{:25} munched {}", "class", "LBRACE");

            // Parse variables of the class
            let variables = self.parse_variables()?;

            // Parse functions of the class
            let mut functions = Vec::new();
            while FUNCTION_FIRST.contains(&self.lexer.peek()) {
                let f = self.parse_function()?;
                functions.push(f);
            }

            Ok(ast::Class { id, extends, variables, functions })
        })();

        match result {
            Ok(class) => Ok(class),
            Err(e) => Err(format_err!("class -> {}", e)),
        }
    }

    fn parse_identifier(&mut self) -> Result<ast::Identifier> {
        info!("Parsing identifier");
        let id = self.lexer.munch(TokenType::ID)?;
        debug!("{:25} munched {} {}", "identifier", "ID", id.text);
        let id = ast::Identifier(id);
        Ok(id)
    }

    fn parse_variables(&mut self) -> Result<Vec<ast::Variable>> {
        info!("Parsing variables");
        let mut variables = vec![];
        while TYPE_FIRST.contains(&self.lexer.peek()) {
            let v = self.parse_variable()?;
            variables.push(v);
        }
        Ok(variables)
    }

    fn parse_variable(&mut self) -> Result<ast::Variable> {
        info!("Parsing variable");
        let result: Result<ast::Variable> = (|| {
            let kind = self.parse_type()?;
            let name = self.parse_identifier()?;
            self.lexer.munch(TokenType::SEMICOLON)?;
            debug!("{:25} munched {}", "variable", "SEMICOLON");
            Ok(ast::Variable { kind, name })
        })();

        match result {
            Ok(variable) => Ok(variable),
            Err(e) => Err(format_err!("variable -> {}", e)),
        }
    }

    fn parse_function(&mut self) -> Result<ast::Function> {
        info!("Parsing function");
        let result: Result<ast::Function> = (|| {
            self.lexer.munch(TokenType::PUBLIC)?;
            debug!("{:25} munched {}", "function", "PUBLIC");
            let kind = self.parse_type()?;
            let name = self.parse_identifier()?;
            self.lexer.munch(TokenType::LPAREN)?;
            debug!("{:25} munched {}", "function", "LPAREN");
            let args = self.parse_arguments()?;
            self.lexer.munch(TokenType::RPAREN)?;
            debug!("{:25} munched {}", "function", "RPAREN");
            self.lexer.munch(TokenType::LBRACE)?;
            debug!("{:25} munched {}", "function", "LBRACE");

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

            self.lexer.munch(TokenType::RETURN)?;
            debug!("{:25} munched {}", "function", "RETURN");
            let expression = self.parse_expression()?;
            self.lexer.munch(TokenType::SEMICOLON)?;
            debug!("{:25} munched {}", "function", "SEMICOLON");
            self.lexer.munch(TokenType::RBRACE)?;
            debug!("{:25} munched {}", "function", "RBRACE");

            Ok(ast::Function { kind, name, args, variables, statements, expression })
        })();

        match result {
            Ok(f) => Ok(f),
            Err(e) => Err(format_err!("function -> {}", e)),
        }
    }

    fn parse_type(&mut self) -> Result<ast::Type> {
        info!("Parsing type");
        let ty = match self.lexer.peek() {
            TokenType::ID => {
                let id = self.parse_identifier()?;
                ast::Type::Id(id)
            },
            TokenType::BOOLEAN => {
                self.lexer.munch(TokenType::BOOLEAN)?;
                ast::Type::Boolean
            },
            TokenType::STRING => {
                self.lexer.munch(TokenType::STRING)?;
                ast::Type::String
            },
            TokenType::INT => {
                self.lexer.munch(TokenType::INT)?;
                debug!("{:25} munched {}", "type", "INT");
                match self.lexer.peek() {
                    TokenType::LBRACKET => {
                        self.lexer.munch(TokenType::LBRACKET)?;
                        debug!("{:25} munched {}", "type", "LBRACKET");
                        self.lexer.munch(TokenType::RBRACKET)?;
                        debug!("{:25} munched {}", "type", "RBRACKET");
                        ast::Type::IntArray
                    },
                    _ => ast::Type::Int,
                }
            },
            _ => Err(format_err!("Unexpected token {} while parsing type", self.lexer.peek()))?,
        };
        Ok(ty)
    }

    fn parse_arguments(&mut self) -> Result<Vec<ast::Argument>> {
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
            let arg = ast::Argument { kind, name };
            args.push(arg);
        }

        Ok(args)
    }

    fn parse_statement(&mut self) -> Result<ast::Statement> {
        info!("Parsing statement");
        let result: Result<ast::Statement> = (|| {
            let stmt = match self.lexer.peek() {
                TokenType::LBRACE => {
                    self.lexer.munch(TokenType::LBRACE)?;
                    debug!("{:25} munched {}", "statement_block", "LBRACE");
                    let mut statements = vec![];
                    while STATEMENT_FIRST.contains(&self.lexer.peek()) {
                        let s = self.parse_statement()?;
                        statements.push(s);
                    }
                    self.lexer.munch(TokenType::RBRACE)?;
                    debug!("{:25} munched {}", "statement_block", "RBRACE");
                    ast::Statement::Braced { statements }
                },
                TokenType::WHILE => {
                    self.lexer.munch(TokenType::WHILE)?;
                    debug!("{:25} munched {}", "statement_while", "WHILE");
                    self.lexer.munch(TokenType::LPAREN)?;
                    debug!("{:25} munched {}", "statement_while", "LPAREN");
                    let expression = self.parse_expression()?;
                    self.lexer.munch(TokenType::RPAREN)?;
                    debug!("{:25} munched {}", "statement_while", "RPAREN");
                    let statement = self.parse_statement()?;
                    ast::Statement::While { expression, statement: Box::new(statement) }
                },
                TokenType::PRINTLN => {
                    self.lexer.munch(TokenType::PRINTLN)?;
                    debug!("{:25} munched {}", "statement_println", "PRINTLN");
                    self.lexer.munch(TokenType::LPAREN)?;
                    debug!("{:25} munched {}", "statement_println", "LPAREN");
                    let expression = self.parse_expression()?;
                    self.lexer.munch(TokenType::RPAREN)?;
                    debug!("{:25} munched {}", "statement_println", "RPAREN");
                    self.lexer.munch(TokenType::SEMICOLON)?;
                    debug!("{:25} munched {}", "statement_println", "SEMICOLON");
                    ast::Statement::Print { expression }
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
                    self.lexer.munch(TokenType::SIDEF)?;
                    debug!("{:25} munched {}", "statement_sidef", "SIDEF");
                    self.lexer.munch(TokenType::LPAREN)?;
                    debug!("{:25} munched {}", "statement_sidef", "LPAREN");
                    let expression = self.parse_expression()?;
                    self.lexer.munch(TokenType::RPAREN)?;
                    debug!("{:25} munched {}", "statement_sidef", "RPAREN");
                    self.lexer.munch(TokenType::SEMICOLON)?;
                    debug!("{:25} munched {}", "statement_sidef", "SEMICOLON");
                    ast::Statement::SideEffect { expression }
                },
                TokenType::IF => {
                    self.lexer.munch(TokenType::IF)?;
                    debug!("{:25} munched {}", "statement_if", "IF");
                    self.lexer.munch(TokenType::LPAREN)?;
                    debug!("{:25} munched {}", "statement_if", "LPAREN");
                    let condition = self.parse_expression()?;
                    self.lexer.munch(TokenType::RPAREN)?;
                    debug!("{:25} munched {}", "statement_if", "RPAREN");
                    let statement = self.parse_statement()?;
                    let statement = Box::new(statement);

                    let otherwise = if self.lexer.peek() == TokenType::ELSE {
                        self.lexer.munch(TokenType::ELSE)?;
                        debug!("{:25} munched {}", "statement_if", "else");
                        let statement = self.parse_statement()?;
                        Some(Box::new(statement))
                    } else { None };

                    ast::Statement::If { condition, statement, otherwise }
                },
                _ => Err(format_err!("Unexpected token {} while parsing statement", self.lexer.peek()))?,
            };
            Ok(stmt)
        })();
        result.map_err(|e| format_err!("statement -> {}", e))
    }

    fn parse_assign_statement(&mut self, lhs: ast::Identifier) -> Result<ast::Statement> {
        info!("Parsing assign statement");
        self.lexer.munch(TokenType::EQSIGN)?;
        let rhs = self.parse_expression()?;
        self.lexer.munch(TokenType::SEMICOLON)?;
        Ok(ast::Statement::Assign { lhs, rhs })
    }

    fn parse_array_assign(&mut self, lhs: ast::Identifier) -> Result<ast::Statement> {
        info!("Parsing array assign");
        self.lexer.munch(TokenType::LBRACKET)?;
        let in_bracket = self.parse_expression()?;
        self.lexer.munch(TokenType::RBRACKET)?;
        self.lexer.munch(TokenType::EQSIGN)?;
        let rhs = self.parse_expression()?;
        self.lexer.munch(TokenType::SEMICOLON)?;
        Ok(ast::Statement::AssignArray { lhs, in_bracket, rhs })
    }

    fn parse_expression(&mut self) -> Result<ast::Expression> {
        info!("Parsing expression");
        let lhs = self.parse_and_term()?;
        let expr = match self.lexer.peek() {
            TokenType::OR => {
                self.lexer.munch(TokenType::OR)?;
                let rhs = self.parse_expression()?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::Or, lhs, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            _ => lhs,
        };
        Ok(expr)
    }

    fn parse_and_term(&mut self) -> Result<ast::Expression> {
        info!("Parsing and_term");
        let lhs = self.parse_cmp_term()?;
        let expr = match self.lexer.peek() {
            TokenType::AND => {
                self.lexer.munch(TokenType::AND)?;
                let rhs = self.parse_and_term()?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::And, lhs, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            _ => lhs,
        };
        Ok(expr)
    }

    fn parse_cmp_term(&mut self) -> Result<ast::Expression> {
        info!("Parsing cmp_term");
        let lhs = self.parse_plus_minus_term()?;
        let expr = match self.lexer.peek() {
            TokenType::EQUALS => {
                self.lexer.munch(TokenType::EQUALS)?;
                let rhs = self.parse_cmp_term()?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::Equals, lhs, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            TokenType::LESSTHAN => {
                self.lexer.munch(TokenType::LESSTHAN)?;
                let rhs = self.parse_cmp_term()?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::LessThan, lhs, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            _ => lhs,
        };
        Ok(expr)
    }

    fn parse_plus_minus_term(&mut self) -> Result<ast::Expression> {
        info!("Parsing plus_minus_term");
        let lhs = self.parse_times_div_term()?;
        let expr = match self.lexer.peek() {
            TokenType::PLUS => {
                self.lexer.munch(TokenType::PLUS)?;
                let rhs = self.parse_plus_minus_term()?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::Plus, lhs, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            TokenType::MINUS => {
                self.lexer.munch(TokenType::MINUS)?;
                let rhs = self.parse_plus_minus_term()?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::Minus, lhs, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            _ => lhs,
        };
        Ok(expr)
    }

    fn parse_times_div_term(&mut self) -> Result<ast::Expression> {
        info!("Parsing times_div_term");
        let lhs = self.parse_postfix_term()?;
        let expr = match self.lexer.peek() {
            TokenType::TIMES => {
                self.lexer.munch(TokenType::TIMES)?;
                let rhs = self.parse_times_div_term()?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::Times, lhs, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            TokenType::DIV => {
                self.lexer.munch(TokenType::DIV)?;
                let rhs = self.parse_times_div_term()?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::Divide, lhs, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            _ => lhs,
        };
        Ok(expr)
    }

    fn parse_postfix_term(&mut self) -> Result<ast::Expression> {
        info!("Parsing postfix_term");
        let base = self.parse_base_expression()?;
        let expr = match self.lexer.peek() {
            TokenType::LBRACKET => {
                self.lexer.munch(TokenType::LBRACKET)?;
                let rhs = self.parse_expression()?;
                self.lexer.munch(TokenType::RBRACKET)?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::ArrayLookup, lhs: base, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            TokenType::DOT => {
                self.lexer.munch(TokenType::DOT)?;
                match self.lexer.peek() {
                    TokenType::LENGTH => {
                        self.lexer.munch(TokenType::LENGTH)?;
                        let unary = ast::UnaryExpression::Length(base);
                        ast::Expression::Unary(Box::new(unary))
                    },
                    TokenType::ID => {
                        let id = self.parse_identifier()?;
                        self.lexer.munch(TokenType::LPAREN)?;
                        let list = self.parse_expression_list()?;
                        self.lexer.munch(TokenType::RPAREN)?;
                        let unary = ast::UnaryExpression::Application { expression: base, id, list };
                        ast::Expression::Unary(Box::new(unary))
                    },
                    _ => Err(format_err!("Expected LENGTH or ID, found {}", self.lexer.peek()))?,
                }
            },
            _ => base,
        };
        Ok(expr)
    }

    fn parse_base_expression(&mut self) -> Result<ast::Expression> {
        info!("Parsing base expression");
        let result: Result<ast::Expression> = (|| {
            let expr = match self.lexer.peek() {
                TokenType::NEW => {
                    self.lexer.munch(TokenType::NEW)?;
                    debug!("{:25} munched {}", "expression", "NEW");
                    match self.lexer.peek() {
                        TokenType::INT => self.parse_expression_new_array()?,
                        TokenType::ID => {
                            let id = self.parse_identifier()?;
                            self.lexer.munch_some(&[
                                TokenType::LPAREN,
                                TokenType::RPAREN,
                            ])?;
                            debug!("{:25} munched {}", "expression", "LPAREN");
                            debug!("{:25} munched {}", "expression", "RPAREN");
                            ast::Expression::NewClass(id)
                        },
                        t => Err(format_err!("Unexpected token while parsing expression: {}", t))?,
                    }
                },
                TokenType::STRINGLIT => {
                    let string = self.lexer.munch(TokenType::STRINGLIT)?;
                    debug!("{:25} munched {}", "expression", "STRINGLIT");
                    ast::Expression::StringLiteral(string)
                },
                TokenType::INTLIT => {
                    let int = self.lexer.munch(TokenType::INTLIT)?;
                    debug!("{:25} munched {}", "expression", "INTLIT");
                    ast::Expression::IntLiteral(int)
                },
                TokenType::TRUE => {
                    self.lexer.munch(TokenType::TRUE)?;
                    debug!("{:25} munched {}", "expression", "TRUE");
                    ast::Expression::TrueLiteral
                },
                TokenType::FALSE => {
                    self.lexer.munch(TokenType::FALSE)?;
                    debug!("{:25} munched {}", "expression", "FALSE");
                    ast::Expression::FalseLiteral
                },
                TokenType::ID => {
                    let id = self.parse_identifier()?;
                    ast::Expression::Identifier(id)
                },
                TokenType::THIS => {
                    self.lexer.munch(TokenType::THIS)?;
                    debug!("{:25} munched {}", "expression", "THIS");
                    ast::Expression::This
                },
                TokenType::BANG => {
                    self.lexer.munch(TokenType::BANG)?;
                    debug!("{:25} munched {}", "expression", "BANG");
                    let not = ast::UnaryExpression::Not(self.parse_expression()?);
                    ast::Expression::Unary(Box::new(not))
                },
                TokenType::LPAREN => {
                    self.lexer.munch(TokenType::LPAREN)?;
                    debug!("{:25} munched {}", "expression", "LPAREN");
                    let e = self.parse_expression()?;
                    self.lexer.munch(TokenType::RPAREN)?;
                    debug!("{:25} munched {}", "expression", "RPAREN");
                    let unary = ast::UnaryExpression::Parentheses(e);
                    ast::Expression::Unary(Box::new(unary))
                },
                _ => Err(format_err!("Unexpected token {} while parsing statement", self.lexer.peek()))?,
            };
            Ok(expr)
        })();
        result.map_err(|e| format_err!("expression -> {}", e))
    }

    fn parse_expression_new_array(&mut self) -> Result<ast::Expression> {
        info!("Parsing expression new array");
        self.lexer.munch(TokenType::INT)?;
        debug!("{:25} munched {}", "expression_new_array", "INT");
        self.lexer.munch(TokenType::LBRACKET)?;
        debug!("{:25} munched {}", "expression_new_array", "LBRACKET");
        let in_brackets = self.parse_expression()?;
        self.lexer.munch(TokenType::RBRACKET)?;
        debug!("{:25} munched {}", "expression_new_array", "RBRACKET");
        let unary = ast::UnaryExpression::NewArray(in_brackets);
        Ok(ast::Expression::Unary(Box::new(unary)))
    }

    fn parse_expression_list(&mut self) -> Result<ast::ExpressionList> {
        let mut exprs = vec![];
        if self.lexer.peek() == TokenType::RPAREN { return Ok(ast::ExpressionList(exprs)) }

        loop {
            let e = self.parse_expression()?;
            exprs.push(e);

            if self.lexer.peek() != TokenType::COMMA { break }
            self.lexer.munch(TokenType::COMMA)?;
        }

        Ok(ast::ExpressionList(exprs))
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
