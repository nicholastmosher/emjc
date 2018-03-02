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
        println!("Parsing program");
        let main = self.parse_main()?;
        let mut classes: Vec<ast::Class> = vec![];
        while let TokenType::CLASS = self.lexer.peek() {
            classes.push(self.parse_class()?)
        }
        Ok(ast::Program { main, classes })
    }

    fn parse_main(&mut self) -> Result<ast::Main> {
        println!("Parsing main");
        self.lexer.munch(TokenType::CLASS)?;
        debug!("{:25} munched {}", "main", "CLASS");
        let id: ast::Identifier = self.lexer.munch(TokenType::ID)?.into();
        debug!("{:25} munched {}", "main", "ID");
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
        let args: ast::Identifier = self.lexer.munch(TokenType::ID)?.into();
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
        self.lexer.munch(TokenType::CLASS)?;
        debug!("{:25} munched {}", "class", "CLASS");
        let id = self.lexer.munch(TokenType::ID)?;
        debug!("{:25} munched {} {}", "class", "ID", id.text);
        let extends = if let TokenType::EXTENDS = self.lexer.peek() {
            self.lexer.munch(TokenType::EXTENDS)?;
            debug!("{:25} munched {}", "class", "EXTENDS");
            let id = self.lexer.munch(TokenType::ID)?;
            Some(ast::Extends { extended: ast::Identifier(id) })
        } else { None };

        let id = ast::Identifier(id);
        Ok(ast::Class { id, extends, variables: Vec::new(), functions: Vec::new() })
    }

    fn parse_identifier(&mut self) -> Result<ast::Identifier> {
        unimplemented!()
    }

    fn parse_extends(&mut self) -> Result<ast::Extends> {
        unimplemented!()
    }

    fn parse_variable(&mut self) -> Result<ast::Variable> {
        unimplemented!()
    }

    fn parse_function(&mut self) -> Result<ast::Function> {
        unimplemented!()
    }

    fn parse_type(&mut self) -> Result<ast::Type> {
        unimplemented!()
    }

    fn parse_argument(&mut self) -> Result<ast::Argument> {
        unimplemented!()
    }

    fn parse_statement(&mut self) -> Result<ast::Statement> {
        let stmt = match self.lexer.peek() {
            TokenType::LBRACE => {
                self.lexer.munch(TokenType::LBRACE)?;
                unimplemented!()
            },
            TokenType::WHILE => {
                self.lexer.munch(TokenType::WHILE)?;
                self.lexer.munch(TokenType::LPAREN)?;
                let expression = self.parse_expression()?;
                self.lexer.munch(TokenType::RPAREN)?;
                let statement = self.parse_statement()?;
                ast::Statement::While { expression, statement: Box::new(statement) }
            },
            TokenType::PRINTLN => {
                self.lexer.munch(TokenType::PRINTLN)?;
                self.lexer.munch(TokenType::LPAREN)?;
                let expression = self.parse_expression()?;
                self.lexer.munch(TokenType::RPAREN)?;
                ast::Statement::Print { expression }
            },
            TokenType::ID => {
                let id = self.lexer.munch(TokenType::ID)?;
                let id = ast::Identifier(id);
                match self.lexer.peek() {
                    TokenType::EQSIGN => self.parse_assign_statement(id)?,
                    TokenType::LBRACKET => self.parse_array_assign(id)?,
                    _ => Err(format_err!("Unexpected token while parsing statement->ID"))?,
                }
            },
            TokenType::SIDEF => {
                self.lexer.munch(TokenType::SIDEF)?;
                self.lexer.munch(TokenType::LPAREN)?;
                let expression = self.parse_expression()?;
                self.lexer.munch(TokenType::RPAREN)?;
                self.lexer.munch(TokenType::SEMICOLON)?;
                ast::Statement::SideEffect { expression }
            },
            _ => Err(format_err!("Unexpected token {} while parsing statement", self.lexer.peek()))?,
        };
        Ok(stmt)
    }

    fn parse_assign_statement(&mut self, lhs: ast::Identifier) -> Result<ast::Statement> {
        self.lexer.munch(TokenType::EQSIGN)?;
        let rhs = self.parse_expression()?;
        self.lexer.munch(TokenType::SEMICOLON)?;
        Ok(ast::Statement::Assign { lhs, rhs })
    }

    fn parse_array_assign(&mut self, lhs: ast::Identifier) -> Result<ast::Statement> {
        self.lexer.munch(TokenType::LBRACKET)?;
        let in_bracket = self.parse_expression()?;
        self.lexer.munch(TokenType::RBRACKET)?;
        self.lexer.munch(TokenType::EQSIGN)?;
        let rhs = self.parse_expression()?;
        self.lexer.munch(TokenType::SEMICOLON)?;
        Ok(ast::Statement::AssignArray { lhs, in_bracket, rhs })
    }

    fn parse_expression(&mut self) -> Result<ast::Expression> {
        let expr = match self.lexer.peek() {
            TokenType::NEW => {
                self.lexer.munch(TokenType::NEW)?;
                debug!("{:25} munched {}", "expression", "NEW");
                match self.lexer.peek() {
                    TokenType::INT => self.parse_expression_new_array()?,
                    TokenType::ID => {
                        let id = self.lexer.munch(TokenType::ID)?;
                        debug!("{:25} munched {}", "expression", "ID");
                        self.lexer.munch_some(&[
                            TokenType::LPAREN,
                            TokenType::RPAREN,
                        ])?;
                        debug!("{:25} munched {}", "expression", "LPAREN");
                        debug!("{:25} munched {}", "expression", "RPAREN");
                        ast::Expression::NewClass(ast::Identifier(id))
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
                let id = self.lexer.munch(TokenType::ID)?;
                debug!("{:25} munched {}", "expression", "ID");
                ast::Expression::Identifier(ast::Identifier(id))
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
            _ => Err(format_err!("Unexpected token {} while parsing statement", self.lexer.peek()))?,
        };

        let expr = self.parse_expression_extension(expr)?;

        Ok(expr)
    }

    fn parse_expression_extension(&mut self, lhs: ast::Expression) -> Result<ast::Expression> {
        let expr = match self.lexer.peek() {
            TokenType::OR => {
                self.lexer.munch(TokenType::OR)?;
                debug!("{:25} munched {}", "expression_extension", "OR");
                let rhs = self.parse_expression()?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::Or, lhs, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            TokenType::AND => {
                self.lexer.munch(TokenType::AND)?;
                debug!("{:25} munched {}", "expression_extension", "AND");
                let rhs = self.parse_expression()?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::And, lhs, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            TokenType::LESSTHAN => {
                self.lexer.munch(TokenType::LESSTHAN)?;
                debug!("{:25} munched {}", "expression_extension", "LESSTHAN");
                let rhs = self.parse_expression()?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::LessThan, lhs, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            TokenType::EQUALS => {
                self.lexer.munch(TokenType::EQUALS)?;
                debug!("{:25} munched {}", "expression_extension", "EQUALS");
                let rhs = self.parse_expression()?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::Equals, lhs, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            TokenType::PLUS => {
                self.lexer.munch(TokenType::PLUS)?;
                debug!("{:25} munched {}", "expression_extension", "PLUS");
                let rhs = self.parse_expression()?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::Plus, lhs, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            TokenType::MINUS => {
                self.lexer.munch(TokenType::MINUS)?;
                debug!("{:25} munched {}", "expression_extension", "MINUS");
                let rhs = self.parse_expression()?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::Minus, lhs, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            TokenType::TIMES => {
                self.lexer.munch(TokenType::TIMES)?;
                debug!("{:25} munched {}", "expression_extension", "TIMES");
                let rhs = self.parse_expression()?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::Times, lhs, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            TokenType::DIV => {
                self.lexer.munch(TokenType::DIV)?;
                debug!("{:25} munched {}", "expression_extension", "DIV");
                let rhs = self.parse_expression()?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::Divide, lhs, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            TokenType::LBRACKET => {
                self.lexer.munch(TokenType::LBRACKET)?;
                debug!("{:25} munched {}", "expression_extension", "LBRACKET");
                let rhs = self.parse_expression()?;
                self.lexer.munch(TokenType::RBRACKET)?;
                debug!("{:25} munched {}", "expression_extension", "RBRACKET");
                let unary = ast::UnaryExpression::Brackets(rhs);
                ast::Expression::Unary(Box::new(unary))
            },
            TokenType::DOT => {
                self.lexer.munch(TokenType::DOT)?;
                debug!("{:25} munched {}", "expression_extension", "DOT");
                match self.lexer.peek() {
                    TokenType::LENGTH => {
                        self.lexer.munch(TokenType::LENGTH)?;
                        debug!("{:25} munched {}", "expression_extension", "LENGTH");
                        let unary = ast::UnaryExpression::Length(lhs);
                        ast::Expression::Unary(Box::new(unary))
                    },
                    TokenType::ID => {
                        let id = self.lexer.munch(TokenType::ID)?;
                        debug!("{:25} munched {}", "expression_extension", "ID");
                        self.parse_expression_application(lhs, ast::Identifier(id))?
                    },
                    _ => Err(format_err!("Unexpected token {} while parsing statement", self.lexer.peek()))?,
                }
            },
            _ => lhs,
        };
        Ok(expr)
    }

    fn parse_expression_application(&mut self, base: ast::Expression, id: ast::Identifier) -> Result<ast::Expression> {
        self.lexer.munch(TokenType::LPAREN)?;
        debug!("{:25} munched {}", "expression_application", "LPAREN");
        let list = self.parse_expression_list()?;
        self.lexer.munch(TokenType::RPAREN)?;
        debug!("{:25} munched {}", "expression_application", "RPAREN");
        let application = ast::UnaryExpression::Application { expression: base, id, list };
        Ok(ast::Expression::Unary(Box::new(application)))
    }

    fn parse_expression_new_array(&mut self) -> Result<ast::Expression> {
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
        let mut list: Vec<ast::Expression> = vec![];
        if EXPRESSION_FIRST.contains(&self.lexer.peek()) {
            let e = self.parse_expression()?;
            list.push(e);
            self.parse_expression_list_repeat(&mut list)?;
        }
        Ok(ast::ExpressionList(list))
    }

    fn parse_expression_list_repeat(&mut self, list: &mut Vec<ast::Expression>) -> Result<()> {
        while self.lexer.peek() == TokenType::COMMA {
            self.lexer.munch(TokenType::COMMA)?;
            let e = self.parse_expression()?;
            list.push(e);
        }
        Ok(())
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
