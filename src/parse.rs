use crate::ast::{Expression, Statement};
use assert_matches::assert_matches;
use std::iter::Peekable;

use crate::{ast, lex::*, StopIdentifier};

pub type ParserError = String;

pub struct Parser<T: Iterator<Item = Token>> {
    inner: Peekable<T>,
}

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn new(inner: T) -> Self {
        Parser {
            inner: inner.peekable(),
        }
    }

    fn read(&mut self) -> Option<Token> {
        self.inner.next()
    }

    fn peek(&mut self) -> Option<&Token> {
        self.inner.peek()
    }

    fn expect_identifier(&mut self) -> Result<StopIdentifier, ParserError> {
        let token = self.read();
        match token {
            Some(Token::Ident(ident)) => Ok(ident),
            Some(token) => Err(format!("expected identifier, got {:?}", token)),
            None => Err("expected identifier, got end of input".to_string()),
        }
    }

    fn expect_token(&mut self, expected: Token) -> Result<Token, ParserError> {
        let token = self.read();
        match token {
            Some(token) if token == expected => Ok(token),
            Some(token) => Err(format!("expected {:?}, got {:?}", expected, token)),
            None => Err(format!("expected {:?}, got end of input", expected)),
        }
    }

    fn parse_let_statement(&mut self) -> Result<ast::Statement, ParserError> {
        let name = self.expect_identifier()?;

        // TODO Expressions
        while self.peek() != Some(&Token::Semicolon) {
            self.read();
        }

        self.expect_token(Token::Semicolon)?;

        Ok(ast::Statement::LetStatement {
            name,
            value: ast::Expression::IntegerLiteral(3),
        })
    }
}

impl<T: Iterator<Item = Token>> Iterator for Parser<T> {
    type Item = Result<ast::Statement, ParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(match self.read() {
            Some(Token::Let) => self.parse_let_statement(),
            _ => Err("unimplemented".to_string()),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_parses_let_statement() {
        let input = "let x = 5;";
        let lexer = Lexer::new(input.chars());
        let mut parser = Parser::new(lexer);

        let actual = parser.next();
        let actual_name = assert_matches!(actual,
            Some(Ok(ast::Statement::LetStatement {
                name,
                value: Expression::IntegerLiteral(3),
            })) => name);
        assert_eq!("x".to_string(), actual_name);
    }
}
