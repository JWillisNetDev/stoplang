use crate::{
    ast::{Expression, Program, Statement},
    lex::*,
    StopRawLiteral,
};
use std::iter::Peekable;

pub type ParseError = String;
pub type ParseResult<T> = Result<T, ParseError>;

pub enum Precedence {
    Lowest = 1,
    Equals = 2,
    LessGreater = 3,
    Sum = 4,
    Product = 5,
    Prefix = 6,
    Call = 7,
}

pub struct Parser<T: Iterator<Item = Token>> {
    inner: Peekable<T>,
}

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn new(inner: T) -> Self {
        Parser {
            inner: inner.peekable(),
        }
    }

    pub fn parse_program(&mut self) -> ParseResult<Program> {
        let statements = self.collect::<Result<Vec<_>, _>>()?;
        Ok(Program::new(statements))
    }

    fn read(&mut self) -> Option<Token> {
        self.inner.next()
    }

    fn peek(&mut self) -> Option<&Token> {
        self.inner.peek()
    }

    fn expect_identifier(&mut self) -> ParseResult<StopRawLiteral> {
        let token = self.read();
        match token {
            Some(Token::Ident(ident)) => Ok(ident),
            Some(token) => Err(format!("expected identifier, got {:?}", token)),
            None => Err("expected identifier, got end of input".to_string()),
        }
    }

    fn expect_token(&mut self, expected: Token) -> ParseResult<Token> {
        let token = self.read();
        match token {
            Some(token) if token == expected => Ok(token),
            Some(token) => Err(format!("expected token {:?}, got {:?}", expected, token)),
            None => Err(format!("expected {:?}, got end of input", expected)),
        }
    }

    fn parse_let_statement(&mut self) -> ParseResult<Statement> {
        // let <identifier> = <expression>;
        let name = self.expect_identifier()?;
        self.expect_token(Token::Op(Operator::Assign))?;

        // TODO Expressions
        while self.peek() != Some(&Token::Semicolon) {
            self.read();
        }

        self.expect_token(Token::Semicolon)?;

        Ok(Statement::LetStatement {
            ident: name,
            expr: Expression::IntegerLiteral(3),
        })
    }

    fn parse_return_statement(&mut self) -> ParseResult<Statement> {
        // return <expression>;

        // TODO Expressions
        while self.peek() != Some(&Token::Semicolon) {
            self.read();
        }

        self.expect_token(Token::Semicolon)?;

        Ok(Statement::ReturnStatement {
            expr: Expression::IntegerLiteral(3),
        })
    }

    fn parse_expression(
        &mut self,
        token: Token,
        precedence: Precedence,
    ) -> ParseResult<Expression> {
        match token {
            Token::Ident(ident) => Ok(Expression::IdentifierLiteral(ident)),
            Token::Int(value) => {
                let value: crate::StopInteger = value
                    .parse()
                    .map_err(|e| format!("failed to parse integer: {}", e))?;
                Ok(Expression::IntegerLiteral(value))
            }
            Token::Op(op) if [Operator::Minus, Operator::Bang].contains(&op) => {
                let next_token = match self.read() {
                    Some(t) => t,
                    None => Err(format!(
                        "expected expression after prefix operator {}",
                        <Operator as Into<&str>>::into(op)
                    ))?, // Improve error message
                };
                let right = Box::new(self.parse_expression(next_token, Precedence::Prefix)?);
                Ok(Expression::PrefixExpression { op, right })
            }
            _ => todo!("parse_expression: {:?}", token),
        }
    }

    fn parse_expression_statement(&mut self, token: Token) -> ParseResult<Statement> {
        // <expression>;
        let expr = self.parse_expression(token, Precedence::Lowest)?;
        if let Some(Token::Semicolon) = self.peek() {
            self.read();
        }

        return Ok(Statement::ExpressionStatement { expr });
    }
}

impl<T: Iterator<Item = Token>> Iterator for Parser<T> {
    type Item = Result<Statement, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.read()?;
        Some(match next {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(next),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_matches::assert_matches;

    type TestResult = ParseResult<()>;

    #[test]
    fn it_parses_let_statement() -> TestResult {
        // let x = 5;
        let input = vec![
            Token::Let,
            Token::Ident("x".to_string()),
            Token::Op(Operator::Assign),
            Token::Int("5".to_string()),
            Token::Semicolon,
        ];
        let mut parser = Parser::new(input.into_iter());
        let program = parser.parse_program()?;
        let statements = program.statements;
        assert_eq!(1, statements.len());

        let statement = &statements[0];
        let ident = assert_matches!(
            statement,
            Statement::LetStatement {
                ident,
                .. // value: Expression::IntegerLiteral(5),
            } => ident.clone()
        );
        assert_eq!("x".to_string(), ident);

        Ok(())
    }

    #[test]
    fn it_parses_return_statement() -> TestResult {
        // return 5;
        let input = vec![Token::Return, Token::Int("5".to_string()), Token::Semicolon];
        let mut parser = Parser::new(input.into_iter());
        let program = parser.parse_program()?;
        let statements = program.statements;
        assert_eq!(1, statements.len());

        let statement = &statements[0];
        assert_matches!(
            statement,
            Statement::ReturnStatement {
                .. //value: Expression::IntegerLiteral(5),
            }
        );

        Ok(())
    }

    #[test]
    fn it_parses_identifier_expr() -> TestResult {
        // foobar;
        let input = vec![Token::Ident("foobar".to_string()), Token::Semicolon];
        let mut parser = Parser::new(input.into_iter());
        let program = parser.parse_program()?;

        assert_eq!(1, program.statements.len());
        let statement = &program.statements[0];
        let identifier = assert_matches!(
            statement,
            Statement::ExpressionStatement {
                expr: Expression::IdentifierLiteral(ident),
            } => ident.clone()
        );
        assert_eq!("foobar".to_string(), identifier);

        Ok(())
    }

    #[test]
    fn it_parses_integer_expr() -> TestResult {
        // 5;
        let input = vec![Token::Int("5".to_string()), Token::Semicolon];
        let mut parser = Parser::new(input.into_iter());
        let program = parser.parse_program()?;
        assert_eq!(1, program.statements.len());

        let statement = &program.statements[0];
        let value = assert_matches!(
            statement,
            Statement::ExpressionStatement {
                expr: Expression::IntegerLiteral(value),
            } => *value
        );
        assert_eq!(5, value);

        Ok(())
    }

    #[test]
    fn it_parses_prefix_expr() -> TestResult {
        // -5;
        let input = vec![
            Token::Op(Operator::Minus),
            Token::Int("5".to_string()),
            Token::Semicolon,
        ];
        let mut parser = Parser::new(input.into_iter());
        let program = parser.parse_program()?;
        assert_eq!(1, program.statements.len());

        let statement = &program.statements[0];
        let prefix = assert_matches!(
            statement,
            Statement::ExpressionStatement {
                expr: Expression::PrefixExpression { op, right },
            } => (op.clone(), right.clone())
        );
        assert_eq!(Operator::Minus, prefix.0);
        assert_eq!(Expression::IntegerLiteral(5), *prefix.1);

        // !5;
        let input = vec![
            Token::Op(Operator::Bang),
            Token::Int("5".to_string()),
            Token::Semicolon,
        ];
        let mut parser = Parser::new(input.into_iter());
        let program = parser.parse_program()?;
        assert_eq!(1, program.statements.len());

        let statement = &program.statements[0];
        let prefix = assert_matches!(
            statement,
            Statement::ExpressionStatement {
                expr: Expression::PrefixExpression { op, right },
            } => (op.clone(), right.clone())
        );
        assert_eq!(Operator::Bang, prefix.0);
        assert_eq!(Expression::IntegerLiteral(5), *prefix.1);

        Ok(())
    }
}
