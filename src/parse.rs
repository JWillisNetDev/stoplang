use crate::{
    ast::{Expression, Program, Statement},
    lex::*,
    StopRawLiteral,
};
use std::iter::Peekable;

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

    pub fn parse_program(&mut self) -> Result<Program, ParserError> {
        let statements = self.collect::<Result<Vec<_>, _>>()?;
        Ok(Program::new(statements))
    }

    fn read(&mut self) -> Option<Token> {
        self.inner.next()
    }

    fn peek(&mut self) -> Option<&Token> {
        self.inner.peek()
    }

    fn expect_identifier(&mut self) -> Result<StopRawLiteral, ParserError> {
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
            Some(token) => Err(format!("expected token {:?}, got {:?}", expected, token)),
            None => Err(format!("expected {:?}, got end of input", expected)),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        // let <identifier> = <expression>;
        let name = self.expect_identifier()?;
        self.expect_token(Token::Op(Operator::Assign))?;

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

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        // return <expression>;

        // TODO Expressions
        while self.peek() != Some(&Token::Semicolon) {
            self.read();
        }

        self.expect_token(Token::Semicolon)?;

        Ok(Statement::ReturnStatement {
            value: Expression::IntegerLiteral(3),
        })
    }
}

impl<T: Iterator<Item = Token>> Iterator for Parser<T> {
    type Item = Result<Statement, ParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.read()?;
        Some(match current {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => todo!("not implemented: {:?}", current),
        })
    }
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;

    use super::*;

    #[test]
    fn it_parses_let_statement() {
        let input = "let x = 5;";
        let lexer = Lexer::new(input.chars());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
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
                .. // value: Expression::IntegerLiteral(5),
            } => name.clone()
        );

        assert_eq!("x".to_string(), name);
    }

    #[test]
    fn it_parses_return_statement() {
        let input = "return 5;";
        let lexer = Lexer::new(input.chars());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        if let Err(err) = program {
            panic!("failed to parse program: {}", err);
        }

        let statements = program.unwrap().statements;
        assert_eq!(1, statements.len());

        let statement = &statements[0];
        assert_matches!(
            statement,
            Statement::ReturnStatement {
                .. //value: Expression::IntegerLiteral(5),
            }
        );
    }
}
