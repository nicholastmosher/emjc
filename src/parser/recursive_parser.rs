use Result;
use lexer::{
    Lexer,
    TokenType,
};

use super::ast;

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
        let main = self.parse_main()?;
        let mut classes: Vec<ast::Class> = vec![];
        while let TokenType::CLASS = self.lexer.peek() {
            classes.push(self.parse_class()?)
        }
        Ok(ast::Program { main, classes })
    }

    fn parse_main(&mut self) -> Result<ast::Main> {
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
            TokenType::ID,
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
        unimplemented!()
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
        unimplemented!()
    }

    fn parse_expression(&mut self) -> Result<ast::Expression> {
        unimplemented!()
    }

    fn parse_expression_list(&mut self) -> Result<ast::ExpressionList> {
        unimplemented!()
    }
}
