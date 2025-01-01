use crate::{
    ast::{Expression, Program, Statement},
    lex::*,
    StopRawLiteral,
};
use std::iter::Peekable;

pub type ParseError = String;
pub type ParseResult<T> = Result<T, ParseError>;

fn is_prefix(token: &Token) -> bool {
    use Operator::*;
    matches!(
        token,
        Token::Op(Bang) | Token::Op(Minus) | Token::Int(_) | Token::Ident(_)
    )
}

fn is_infix(token: &Token) -> bool {
    use Operator::*;
    matches!(
        token,
        Token::Op(Plus)
            | Token::Op(Minus)
            | Token::Op(Splat)
            | Token::Op(Slash)
            | Token::Op(Gt)
            | Token::Op(Gte)
            | Token::Op(Lt)
            | Token::Op(Lte)
            | Token::Op(Eq)
            | Token::Op(Neq)
    )
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest = 1,
    Equals = 2,
    LessGreater = 3,
    Sum = 4,
    Product = 5,
    Prefix = 6,
    Call = 7,
}

impl Precedence {
    fn of(token: &Token) -> Self {
        if let Token::Op(op) = token {
            match op {
                Operator::Eq | Operator::Neq => Precedence::Equals,
                Operator::Lt | Operator::Lte | Operator::Gt | Operator::Gte => {
                    Precedence::LessGreater
                }
                Operator::Plus | Operator::Minus => Precedence::Sum,
                Operator::Splat | Operator::Slash => Precedence::Product,
                _ => Precedence::Lowest,
            }
        } else {
            Precedence::Lowest
        }
    }
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

    fn try_parse_statement(&mut self) -> Option<ParseResult<Statement>> {
        let token = self.read();
        match token {
            Some(token) => match token {
                Token::Let => Some(self.parse_let_statement()),
                Token::Return => Some(self.parse_return_statement()),
                Token::OpenBrace => Some(self.parse_block_statement()),
                _ => Some(self.parse_expression_statement(&token)),
            },
            None => None,
        }
    }

    fn parse_let_statement(&mut self) -> ParseResult<Statement> {
        // let <identifier> = <expression>;
        let ident = self.expect_identifier()?;
        self.expect_token(Token::Op(Operator::Assign))?;

        let token = self.read().ok_or("expected an expression following let")?;
        let expr = self.parse_expression(&token, Precedence::Lowest)?;

        self.expect_token(Token::Semicolon)?;

        Ok(Statement::LetStatement { ident, expr })
    }

    fn parse_return_statement(&mut self) -> ParseResult<Statement> {
        // return <expression>;
        let token = self.read().ok_or("expected an expression following return")?;
        let expr = self.parse_expression(&token, Precedence::Lowest)?;

        self.expect_token(Token::Semicolon)?;

        Ok(Statement::ReturnStatement {
            expr
        })
    }

    fn parse_block_statement(&mut self) -> ParseResult<Statement> {
        // { <statement>* }
        let mut statements = Vec::new();
        while self.peek() != Some(&Token::CloseBrace) {
            let statement = self.try_parse_statement();
            match statement {
                Some(Ok(statement)) => statements.push(Box::new(statement)),
                Some(Err(e)) => return Err(e),
                None => return Err("unexpected end of input".to_string()),
            }
        }

        self.expect_token(Token::CloseBrace)?;

        Ok(Statement::BlockStatement { statements })
    }

    fn parse_expression_statement(&mut self, token: &Token) -> ParseResult<Statement> {
        // <expression>;
        let expr = self.parse_expression(token, Precedence::Lowest)?;
        if let Some(Token::Semicolon) = self.peek() {
            self.read();
        }

        Ok(Statement::ExpressionStatement { expr })
    }

    fn parse_prefix(&mut self, token: &Token) -> ParseResult<Expression> {
        match token {
            Token::Ident(ident) => Ok(Expression::IdentifierLiteral(ident.clone())),
            Token::Int(int) => {
                let int = int
                    .parse()
                    .map_err(|e| format!("failed to parse integer: {}", e))?;
                Ok(Expression::IntegerLiteral(int))
            }
            Token::Op(op) if is_prefix(token) => {
                let next_token = self.read().ok_or_else(|| {
                    format!(
                        "expected an expression following prefix operator {}, found nothing",
                        <Operator as Into<&str>>::into(*op)
                    )
                })?;
                let right = Box::new(self.parse_expression(&next_token, Precedence::Prefix)?);
                Ok(Expression::PrefixExpression { op: *op, right })
            }
            Token::OpenParen => {
                let next_token = self.read().ok_or("expected an expression following (")?;
                let expr = self.parse_expression(&next_token, Precedence::Lowest)?;
                self.expect_token(Token::CloseParen)?;
                Ok(expr)
            }
            Token::If => {
                let token = self.read().ok_or("expected an expression following if")?;
                let condition = Box::new(self.parse_expression(&token, Precedence::Lowest)?);

                let consequence = Box::new(
                    self.try_parse_statement()
                        .ok_or("expected statement following if condition")??,
                );

                if self.peek() == Some(&Token::Else) {
                    self.read();
                    let alternative = Box::new(
                        self.try_parse_statement()
                            .ok_or("expected statement following else")??,
                    );
                    Ok(Expression::IfExpression {
                        condition,
                        consequence: consequence,
                        alternative: Some(alternative),
                    })
                } else {
                    Ok(Expression::IfExpression {
                        condition,
                        consequence: consequence,
                        alternative: None,
                    })
                }
            }
            _ => Err(format!("unexpected token {:?}, expected a prefix", token)),
        }
    }

    fn parse_infix(&mut self, left: Expression) -> ParseResult<Expression> {
        let token = self
            .read()
            .ok_or("expected an infix operator, found nothing")?;

        let op = match token {
            Token::Op(op) if is_infix(&token) => op,
            _ => {
                return Err(format!(
                    "unexpected token {:?}, expected an infix operator",
                    token
                ))
            }
        };

        let next_token = self.read().ok_or(format!(
            "expected an expression following infix operator {}, found nothing",
            <Operator as Into<&str>>::into(op)
        ))?;

        let right = Box::new(self.parse_expression(&next_token, Precedence::of(&token))?);
        Ok(Expression::InfixExpression {
            left: Box::new(left),
            op,
            right,
        })
    }

    fn parse_expression(
        &mut self,
        token: &Token,
        precedence: Precedence,
    ) -> ParseResult<Expression> {
        let mut expr = self.parse_prefix(token)?;

        if let Some(mut next) = self.peek() {
            while next != &Token::Semicolon && precedence < Precedence::of(next) && is_infix(next) {
                expr = self.parse_infix(expr)?;
                next = match self.peek() {
                    Some(token) => token,
                    None => break,
                };
            }
        }

        Ok(expr)
    }
}

impl<T: Iterator<Item = Token>> Iterator for Parser<T> {
    type Item = Result<Statement, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.try_parse_statement()
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
    fn it_parses_block_statement() -> TestResult {
        // { let x = 5; let y = 10; }
        let input = vec![
            Token::OpenBrace,
            Token::Let,
            Token::Ident("x".to_string()),
            Token::Op(Operator::Assign),
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Let,
            Token::Ident("y".to_string()),
            Token::Op(Operator::Assign),
            Token::Int("10".to_string()),
            Token::Semicolon,
            Token::CloseBrace,
        ];
        let mut parser = Parser::new(input.into_iter());
        let program = parser.parse_program()?;
        let statements = program.statements;
        assert_eq!(1, statements.len());

        let block_statement = assert_matches!(
            &statements[0],
            Statement::BlockStatement { statements } => statements
        );
        assert_eq!(2, block_statement.len());

        assert_eq!(
            block_statement[0],
            Box::new(Statement::LetStatement {
                ident: "x".to_string(),
                expr: Expression::IntegerLiteral(5),
            })
        );

        assert_eq!(
            block_statement[1],
            Box::new(Statement::LetStatement {
                ident: "y".to_string(),
                expr: Expression::IntegerLiteral(10),
            })
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
        helpers::assert_integer_expr(*prefix.1, 5);

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
        helpers::assert_integer_expr(*prefix.1, 5);

        Ok(())
    }

    #[test]
    fn it_parses_infix_expr() -> TestResult {
        // 5 + 5;
        // 5 - 5;
        // 5 * 5;
        // 5 / 5;
        // 5 > 5;
        // 5 >= 5;
        // 5 < 5;
        // 5 <= 5;
        // 5 == 5;
        // 5 != 5;

        let input = vec![
            Token::Int("5".to_string()),
            Token::Op(Operator::Plus),
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Int("5".to_string()),
            Token::Op(Operator::Minus),
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Int("5".to_string()),
            Token::Op(Operator::Splat),
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Int("5".to_string()),
            Token::Op(Operator::Slash),
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Int("5".to_string()),
            Token::Op(Operator::Gt),
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Int("5".to_string()),
            Token::Op(Operator::Gte),
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Int("5".to_string()),
            Token::Op(Operator::Lt),
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Int("5".to_string()),
            Token::Op(Operator::Lte),
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Int("5".to_string()),
            Token::Op(Operator::Eq),
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Int("5".to_string()),
            Token::Op(Operator::Neq),
            Token::Int("5".to_string()),
            Token::Semicolon,
        ];
        let mut parser = Parser::new(input.into_iter());
        let program = parser.parse_program()?;

        helpers::assert_infix_expr(
            program,
            vec![
                (
                    Expression::IntegerLiteral(5),
                    Operator::Plus,
                    Expression::IntegerLiteral(5),
                ),
                (
                    Expression::IntegerLiteral(5),
                    Operator::Minus,
                    Expression::IntegerLiteral(5),
                ),
                (
                    Expression::IntegerLiteral(5),
                    Operator::Splat,
                    Expression::IntegerLiteral(5),
                ),
                (
                    Expression::IntegerLiteral(5),
                    Operator::Slash,
                    Expression::IntegerLiteral(5),
                ),
                (
                    Expression::IntegerLiteral(5),
                    Operator::Gt,
                    Expression::IntegerLiteral(5),
                ),
                (
                    Expression::IntegerLiteral(5),
                    Operator::Gte,
                    Expression::IntegerLiteral(5),
                ),
                (
                    Expression::IntegerLiteral(5),
                    Operator::Lt,
                    Expression::IntegerLiteral(5),
                ),
                (
                    Expression::IntegerLiteral(5),
                    Operator::Lte,
                    Expression::IntegerLiteral(5),
                ),
                (
                    Expression::IntegerLiteral(5),
                    Operator::Eq,
                    Expression::IntegerLiteral(5),
                ),
                (
                    Expression::IntegerLiteral(5),
                    Operator::Neq,
                    Expression::IntegerLiteral(5),
                ),
            ],
        );
        Ok(())
    }

    #[test]
    fn it_parses_by_precedence() -> TestResult {
        // 4 * 5 + 3;
        // ((4 * 5) + 3)
        let input = vec![
            Token::Int("4".to_string()),
            Token::Op(Operator::Splat),
            Token::Int("5".to_string()),
            Token::Op(Operator::Plus),
            Token::Int("3".to_string()),
            Token::Semicolon,
        ];

        let mut parser = Parser::new(input.into_iter());
        let program = parser.parse_program()?;
        assert_eq!(1, program.statements.len());
        let statement = &program.statements[0];
        assert_matches!(
            statement,
            Statement::ExpressionStatement {
                expr: Expression::InfixExpression {
                    // 4 * 5
                    left: box Expression::InfixExpression {
                        left: box Expression::IntegerLiteral(4),
                        op: Operator::Splat,
                        right: box Expression::IntegerLiteral(5),
                    },
                    op: Operator::Plus,
                    right: box Expression::IntegerLiteral(3),
                }
            }
        );

        // 4 * (5 + 3);
        let input = vec![
            Token::Int("4".to_string()),
            Token::Op(Operator::Splat),
            Token::OpenParen,
            Token::Int("5".to_string()),
            Token::Op(Operator::Plus),
            Token::Int("3".to_string()),
            Token::CloseParen,
            Token::Semicolon,
        ];
        let mut parser = Parser::new(input.into_iter());
        let program = parser.parse_program()?;
        assert_eq!(1, program.statements.len());
        let statement = &program.statements[0];

        assert_matches!(
            statement,
            Statement::ExpressionStatement {
                expr: Expression::InfixExpression {
                    left: box Expression::IntegerLiteral(4),
                    op: Operator::Splat,
                    right: box Expression::InfixExpression {
                        left: box Expression::IntegerLiteral(5),
                        op: Operator::Plus,
                        right: box Expression::IntegerLiteral(3),
                    },
                }
            }
        );

        // TODO: Consider a macro for this
        // "3 + 4 * 5 == 3 * 1 + 4 * 5";

        let input = vec![
            Token::Int("3".to_string()),
            Token::Op(Operator::Plus),
            Token::Int("4".to_string()),
            Token::Op(Operator::Splat),
            Token::Int("5".to_string()),
            Token::Op(Operator::Eq),
            Token::Int("3".to_string()),
            Token::Op(Operator::Splat),
            Token::Int("1".to_string()),
            Token::Op(Operator::Plus),
            Token::Int("4".to_string()),
            Token::Op(Operator::Splat),
            Token::Int("5".to_string()),
        ];
        let mut parser = Parser::new(input.into_iter());
        let program = parser.parse_program()?;
        let statement = &program.statements[0];

        assert_matches!(
            statement,
            Statement::ExpressionStatement {
                expr: Expression::InfixExpression {
                    // 3 + (4 * 5)
                    left: box Expression::InfixExpression {
                        left: box Expression::IntegerLiteral(3),
                        op: Operator::Plus,
                        right: box Expression::InfixExpression {
                            left: box Expression::IntegerLiteral(4),
                            op: Operator::Splat,
                            right: box Expression::IntegerLiteral(5),
                        },
                    },
                    op: Operator::Eq,
                    // (3 * 1) + (4 * 5)
                    right: box Expression::InfixExpression {
                        left: box Expression::InfixExpression {
                            left: box Expression::IntegerLiteral(3),
                            op: Operator::Splat,
                            right: box Expression::IntegerLiteral(1),
                        },
                        op: Operator::Plus,
                        right: box Expression::InfixExpression {
                            left: box Expression::IntegerLiteral(4),
                            op: Operator::Splat,
                            right: box Expression::IntegerLiteral(5),
                        },
                    }
                }
             }
        );

        Ok(())
    }

    #[test]
    fn it_parses_grouped_expr() -> TestResult {
        // (5 + 5) * 5;
        let input = vec![
            Token::OpenParen,
            Token::Int("5".to_string()),
            Token::Op(Operator::Plus),
            Token::Int("5".to_string()),
            Token::CloseParen,
            Token::Op(Operator::Splat),
            Token::Int("5".to_string()),
            Token::Semicolon,
        ];
        let mut parser = Parser::new(input.into_iter());
        let program = parser.parse_program()?;
        assert_eq!(1, program.statements.len());
        let statement = &program.statements[0];
        assert_matches!(
            statement,
            Statement::ExpressionStatement {
                expr: Expression::InfixExpression {
                    left: box Expression::InfixExpression {
                        // (5 + 5)
                        left: box Expression::IntegerLiteral(5),
                        op: Operator::Plus,
                        right: box Expression::IntegerLiteral(5),
                    },
                    // * 5
                    op: Operator::Splat,
                    right: box Expression::IntegerLiteral(5),
                }
            }
        );

        Ok(())
    }

    #[test]
    fn it_parses_if_expr() -> TestResult {
        // if x == 5 return x; else let x = 5;
        let input = vec![
            Token::If,
            Token::OpenParen,
            Token::Ident("x".to_string()),
            Token::Op(Operator::Eq),
            Token::Int("5".to_string()),
            Token::CloseParen,
            Token::Return,
            Token::Ident("x".to_string()),
            Token::Semicolon,
            Token::Else,
            Token::Let,
            Token::Ident("x".to_string()),
            Token::Op(Operator::Assign),
            Token::Int("5".to_string()),
            Token::Semicolon,
        ];
        let mut parser = Parser::new(input.into_iter());
        let program = parser.parse_program()?;
        assert_eq!(1, program.statements.len());
        let statement = &program.statements[0];

        let (condition, consequence, alternative) = assert_matches!(
            statement,
            Statement::ExpressionStatement {
                expr: Expression::IfExpression {
                    condition,
                    consequence,
                    alternative,
                }
            } => (*condition.clone(), *consequence.clone(), alternative.clone())
        );

        assert_eq!(
            Expression::InfixExpression {
                left: Box::new(Expression::IdentifierLiteral("x".to_string())),
                op: Operator::Eq,
                right: Box::new(Expression::IntegerLiteral(5)),
            },
            condition
        );

        assert_eq!(
            Statement::ReturnStatement {
                expr: Expression::IdentifierLiteral("x".to_string()),
            },
            consequence
        );

        assert_eq!(
            Some(Box::new(Statement::LetStatement {
                ident: "x".to_string(),
                expr: Expression::IntegerLiteral(5),
            })),
            alternative
        );

        Ok(())
    }

    #[test]
    fn it_parses_if_expr_with_block_statements() -> TestResult {
        // if (x < y) { x } else { y }
        let input = vec![
            Token::If,
            Token::OpenParen,
            Token::Ident("x".to_string()),
            Token::Op(Operator::Lt),
            Token::Ident("y".to_string()),
            Token::CloseParen,
            Token::OpenBrace,
            Token::Ident("x".to_string()),
            Token::CloseBrace,
            Token::Else,
            Token::OpenBrace,
            Token::Ident("y".to_string()),
            Token::CloseBrace,
        ];
        let mut parser = Parser::new(input.into_iter());
        let program = parser.parse_program()?;
        assert_eq!(1, program.statements.len());
        let statement = &program.statements[0];

        let (condition, consequence, alternative) = assert_matches!(
            statement,
            Statement::ExpressionStatement {
                expr: Expression::IfExpression {
                    condition,
                    consequence,
                    alternative,
                }
            } => (*condition.clone(), *consequence.clone(), alternative.clone())
        );

        let (ident_lhs, ident_rhs) = assert_matches!(
            condition,
            Expression::InfixExpression {
                left: box Expression::IdentifierLiteral(ident_lhs),
                op: Operator::Lt,
                right: box Expression::IdentifierLiteral(ident_rhs),
            } => (ident_lhs, ident_rhs)
        );
        assert_eq!("x".to_string(), ident_lhs);
        assert_eq!("y".to_string(), ident_rhs);

        assert_eq!(
            Statement::BlockStatement {
                statements: vec![Box::new(Statement::ExpressionStatement {
                    expr: Expression::IdentifierLiteral("x".to_string()),
                })],
            },
            consequence
        );

        assert_eq!(
            Some(Box::new(Statement::BlockStatement {
                statements: vec![Box::new(Statement::ExpressionStatement {
                    expr: Expression::IdentifierLiteral("y".to_string()),
                })],
            })),
            alternative
        );

        Ok(())
    }

    mod helpers {
        use super::*;

        pub fn assert_integer_expr(expr: Expression, expected: crate::StopInteger) {
            assert_eq!(Expression::IntegerLiteral(expected), expr)
        }

        pub fn assert_infix_expr(
            program: Program,
            expected: Vec<(Expression, Operator, Expression)>,
        ) {
            let statements = program.statements;
            assert_eq!(expected.len(), statements.len());
            for (statement, (expected_left, expected_op, expected_right)) in
                statements.iter().zip(expected.iter())
            {
                let (left, op, right) = assert_matches!(
                    statement,
                    Statement::ExpressionStatement {
                        expr: Expression::InfixExpression { left, op, right },
                    } => (left.clone(), *op, right.clone())
                );
                assert_eq!(*expected_left, *left);
                assert_eq!(*expected_op, op);
                assert_eq!(*expected_right, *right);
            }
        }
    }
}
