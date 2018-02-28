use std::iter::Peekable;
use std::ops::{
    Deref,
    DerefMut,
};
use failure::Error;
use lexer::{
    Token,
    TokenType,
};

use super::ParseError;
use super::ast;

pub struct Parser<'a, T: Iterator<Item=Token<'a>>> {
    token_stream: Peekable<T>,
    current_token: Option<Token<'a>>,
}

impl<'a, T: 'a + Iterator<Item=Token<'a>>> Parser<'a, T> {
    pub fn new(token_stream: T) -> Result<Self, Error> {
        let mut token_stream = token_stream.peekable();
        let first = token_stream.next();
        let current = match first {
            None | Some(Token { ty: TokenType::EOF, .. }) => Err(ParseError::PrematureConclusion)?,
            Some(token) => token,
        };
        Ok(Parser { token_stream, current_token: Some(current) })
    }
}

/// Allow Parser to be treated as if it were an iterator of tokens
impl<'a, T: Iterator<Item=Token<'a>>> Deref for Parser<'a, T> {
    type Target = Peekable<T>;
    fn deref(&self) -> &Self::Target { &self.token_stream }
}

impl<'a, T: Iterator<Item=Token<'a>>> DerefMut for Parser<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.token_stream }
}

impl<'a, T: 'a + Iterator<Item=Token<'a>>> Parser<'a, T> {
    fn advance(&mut self) -> Result<Token<'a>, Error> {
        let token = self.next();
        let new = match token {
            None | Some(Token { ty: TokenType::EOF, .. }) => Err(ParseError::PrematureConclusion)?,
            Some(token) => token,
        };
        let last = self.current_token.take().unwrap();
        self.current_token.get_or_insert(new);
        Ok(last)
    }

    fn take_one(&mut self, kind: TokenType) -> Result<Token<'a>, Error> {
        let ty = self.current_token.as_ref().unwrap().ty;
        if ty == kind {
            self.advance()
        } else {
            Err(ParseError::UnexpectedToken.into())
        }
    }

    fn take_some(&mut self, kinds: &[TokenType]) -> Result<(), Error> {
        for kind in kinds {
            self.take_one(*kind)?;
        }
        Ok(())
    }

    pub fn parse_program(&mut self) -> Result<ast::Program, Error> {
        let main = self.parse_main()?;
        let classes: Vec<ast::Class> = vec![];
        Ok(ast::Program { main, classes })
    }

    fn parse_main(&mut self) -> Result<ast::Main, Error> {
        unimplemented!()
    }

    fn parse_class(&mut self) -> Result<ast::Class, Error> {
        unimplemented!()
    }

    fn parse_identifier(&mut self) -> Result<ast::Identifier, Error> {
        unimplemented!()
    }

    fn parse_extends(&mut self) -> Result<ast::Extends, Error> {
        unimplemented!()
    }

    fn parse_variable(&mut self) -> Result<ast::Variable, Error> {
        unimplemented!()
    }

    fn parse_function(&mut self) -> Result<ast::Function, Error> {
        unimplemented!()
    }

    fn parse_type(&mut self) -> Result<ast::Type, Error> {
        unimplemented!()
    }

    fn parse_argument(&mut self) -> Result<ast::Argument, Error> {
        unimplemented!()
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, Error> {
        unimplemented!()
    }

    fn parse_expression(&mut self) -> Result<ast::Expression, Error> {
        unimplemented!()
    }

    fn parse_expression_list(&mut self) -> Result<ast::ExpressionList, Error> {
        unimplemented!()
    }
}
