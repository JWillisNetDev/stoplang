use crate::{
    ast::{Expression, Program, Statement},
    lex::*,
    StopIdentifier, StopInteger,
};
use assert_matches::assert_matches;
use phf::phf_map;
use std::{fmt::format, iter::Peekable};

pub type ParserError = String;

pub struct Parser<T: Iterator<Item = Token>> {
    inner: Peekable<T>,
    peek_lazy: Option<Token>,
}

type PrefixParser = fn(Token) -> Result<Expression, ParserError>;
type InfixParser = fn(Expression, Token) -> Result<Expression, ParserError>;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest = 0,
    Eq = 1,
    LessGreater = 2,
    Sum = 3,
    Product = 4,
    Call = 5,
}

fn get_precedence(token: Token) -> Precedence {
    match token {
        Token::Eq | Token::Neq => Precedence::Eq,
        Token::LessThan | Token::LessThanEq | Token::GreaterThan | Token::GreaterThanEq => {
            Precedence::LessGreater
        }
        Token::Plus | Token::Minus => Precedence::Sum,
        Token::Splat | Token::Slash => Precedence::Product,
        Token::OpenParen => Precedence::Call,
        _ => Precedence::Lowest,
    }
}

fn get_prefix_parser(
    token: Token,
) -> Option<Box<dyn FnOnce(Token) -> Result<Expression, ParserError>>> {
    match token {
        Token::Ident(ident) => Some(Box::new(move |_| Ok(Expression::IdentifierLiteral(ident)))),
        Token::Int(i) => Some(Box::new(move |_| {
            let parsed = i.parse::<StopInteger>();
            match parsed {
                Ok(i) => Ok(Expression::IntegerLiteral(i)),
                Err(err) => Err(format!("failed to parse integer: {}", err)),
            }
        })),
        _ => todo!(),
    }
}

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn new(inner: T) -> Self {
        Parser {
            inner: inner.peekable(),
            peek_lazy: None,
        }
    }

    pub fn create_program(&mut self) -> Result<Program, ParserError> {
        let mut statements = Vec::new();
        for statement in self {
            match statement {
                Ok(statement) => statements.push(statement),
                Err(err) => return Err(err),
            }
        }

        Ok(Program { statements })
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

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParserError> {
        todo!()
    }

    fn parse_prefix(&mut self, token: Token) -> Result<Expression, ParserError> {
        todo!()
    }

    fn parse_prefix_expr(&mut self) -> Result<Expression, ParserError> {
        todo!()
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        // let <identifier> = <expression>;
        let name = self.expect_identifier()?;
        self.expect_token(Token::Assign)?;

        // TODO Expressions
        while self.peek() != Some(&Token::Semicolon) {
            self.read();
        }

        self.expect_token(Token::Semicolon)?;

        Ok(Statement::LetStatement {
            name,
            value: Expression::IntegerLiteral(3),
        })
    }

    fn parse_program(&mut self) -> Result<Program, ParserError> {
        let statements = self.collect::<Result<Vec<_>, _>>()?;
        Ok(Program::new(statements))
    }
}

impl<T: Iterator<Item = Token>> Iterator for Parser<T> {
    type Item = Result<Statement, ParserError>;

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

        let program = parser.create_program();
        if let Err(err) = program {
            panic!("failed to parse program: {}", err);
        }

        let statements = program.unwrap().statements;
        assert_eq!(1, statements.len());

        let statement = &statements[0];
        let name = assert_matches!(
            statement,
            Statement::LetStatement {
                name,
                value: Expression::IntegerLiteral(5),
            } => name.clone()
        );

        assert_eq!("x".to_string(), name);

        let actual = parser.next();
        let actual_name = assert_matches!(actual,
            Some(Ok(Statement::LetStatement {
                name,
                value: Expression::IntegerLiteral(3),
            })) => name);
        assert_eq!("x".to_string(), actual_name);
    }

    #[test]
    fn it_parses_return_statement() {
        let input = "return 5;";
        let lexer = Lexer::new(input.chars());
        let mut parser = Parser::new(lexer);

        let program = parser.create_program();
        if let Err(err) = program {
            panic!("failed to parse program: {}", err);
        }

        let statements = program.unwrap().statements;
        assert_eq!(1, statements.len());

        let statement = &statements[0];
        assert_matches!(
            statement,
            Statement::ReturnStatement {
                value: Expression::IntegerLiteral(5),
            }
        );
    }
}
