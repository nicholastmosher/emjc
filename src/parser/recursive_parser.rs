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
        let id: ast::Identifier = self.lexer.munch(TokenType::ID)?.into();
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
        let args: ast::Identifier = self.lexer.munch(TokenType::ID)?.into();
        self.lexer.munch_some(&[
            TokenType::RPAREN,
            TokenType::LBRACE,
        ])?;
        let body = self.parse_statement()?;
        self.lexer.munch_some(&[
            TokenType::RBRACE,
            TokenType::RBRACE,
        ])?;
        Ok(ast::Main { id, args, body })
    }

    fn parse_class(&mut self) -> Result<ast::Class> {
        self.lexer.munch(TokenType::CLASS)?;
        let id = self.lexer.munch(TokenType::ID)?;
        let extends = if let TokenType::EXTENDS = self.lexer.peek() {
            self.lexer.munch(TokenType::EXTENDS)?;
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
                self.lexer.munch(TokenType::ID)?;
                match self.lexer.peek() {
                    TokenType::EQSIGN => self.parse_assign_statement()?,
                    TokenType::LBRACKET => self.parse_array_assign()?,
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

    fn parse_assign_statement(&mut self) -> Result<ast::Statement> {
        unimplemented!()
    }

    fn parse_array_assign(&mut self) -> Result<ast::Statement> {
        unimplemented!()
    }

    fn parse_expression(&mut self) -> Result<ast::Expression> {
        let expr = match self.lexer.peek() {
            TokenType::NEW => {
                self.lexer.munch(TokenType::NEW)?;
                match self.lexer.peek() {
                    TokenType::INT => self.parse_expression_new_array()?,
                    TokenType::ID => {
                        let id = self.lexer.munch(TokenType::ID)?;
                        self.lexer.munch_some(&[
                            TokenType::LPAREN,
                            TokenType::RPAREN,
                        ])?;
                        ast::Expression::NewClass(ast::Identifier(id))
                    },
                    t => Err(format_err!("Unexpected token while parsing expression: {}", t))?,
                }
            },
            TokenType::STRINGLIT => {
                let string = self.lexer.munch(TokenType::STRINGLIT)?;
                ast::Expression::StringLiteral(string)
            },
            TokenType::INTLIT => {
                let int = self.lexer.munch(TokenType::INTLIT)?;
                ast::Expression::IntLiteral(int)
            },
            TokenType::TRUE => {
                self.lexer.munch(TokenType::TRUE)?;
                ast::Expression::TrueLiteral
            },
            TokenType::FALSE => {
                self.lexer.munch(TokenType::FALSE)?;
                ast::Expression::FalseLiteral
            },
            TokenType::ID => {
                let id = self.lexer.munch(TokenType::ID)?;
                ast::Expression::Identifier(ast::Identifier(id))
            },
            TokenType::THIS => {
                self.lexer.munch(TokenType::THIS)?;
                ast::Expression::This
            },
            TokenType::BANG => {
                self.lexer.munch(TokenType::BANG)?;
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
                let rhs = self.parse_expression()?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::Or, lhs, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            TokenType::AND => {
                self.lexer.munch(TokenType::AND)?;
                let rhs = self.parse_expression()?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::And, lhs, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            TokenType::LESSTHAN => {
                self.lexer.munch(TokenType::LESSTHAN)?;
                let rhs = self.parse_expression()?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::LessThan, lhs, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            TokenType::EQUALS => {
                self.lexer.munch(TokenType::EQUALS)?;
                let rhs = self.parse_expression()?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::Equals, lhs, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            TokenType::PLUS => {
                self.lexer.munch(TokenType::PLUS)?;
                let rhs = self.parse_expression()?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::Plus, lhs, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            TokenType::MINUS => {
                self.lexer.munch(TokenType::MINUS)?;
                let rhs = self.parse_expression()?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::Minus, lhs, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            TokenType::TIMES => {
                self.lexer.munch(TokenType::TIMES)?;
                let rhs = self.parse_expression()?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::Times, lhs, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            TokenType::DIV => {
                self.lexer.munch(TokenType::DIV)?;
                let rhs = self.parse_expression()?;
                let binary = ast::BinaryExpression { kind: ast::BinaryKind::Divide, lhs, rhs };
                ast::Expression::Binary(Box::new(binary))
            },
            TokenType::LBRACKET => {
                self.lexer.munch(TokenType::LBRACKET)?;
                let rhs = self.parse_expression()?;
                self.lexer.munch(TokenType::RBRACKET)?;
                let unary = ast::UnaryExpression::Brackets(rhs);
                ast::Expression::Unary(Box::new(unary))
            },
            TokenType::DOT => {
                self.lexer.munch(TokenType::DOT)?;
                match self.lexer.peek() {
                    TokenType::LENGTH => {
                        self.lexer.munch(TokenType::LENGTH)?;
                        let unary = ast::UnaryExpression::Length(lhs);
                        ast::Expression::Unary(Box::new(unary))
                    },
                    TokenType::ID => {
                        let id = self.lexer.munch(TokenType::ID)?;
                        self.parse_expression_application(lhs, ast::Identifier(id))?
                    },
                    _ => Err(format_err!("Unexpected token {} while parsing statement", self.lexer.peek()))?,
                }
            },
            _ => lhs,
        };
        Ok(expr)
    }

    fn parse_expression_application(&mut self, _base: ast::Expression, _id: ast::Identifier) -> Result<ast::Expression> {
        unimplemented!()
    }

    fn parse_expression_new_array(&mut self) -> Result<ast::Expression> {
        unimplemented!()
    }

    fn parse_expression_list(&mut self) -> Result<ast::ExpressionList> {
        unimplemented!()
    }
}
