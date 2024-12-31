use std::fmt::Write;

use crate::{lex::Operator, StopRawLiteral, StopInteger};

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub(crate) fn new(statements: Vec<Statement>) -> Self {
        Self {
            statements,
        }
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.statements.is_empty() {
            f.write_str(self.statements[0].to_string().as_str())?;
            for statement in self.statements.iter().skip(1) {
                f.write_char('\n')?;
                f.write_str(statement.to_string().as_str())?;
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    LetStatement {
        ident: StopRawLiteral,
        expr: Expression,
    },
    ReturnStatement {
        expr: Expression,
    },
    ExpressionStatement {
        expr: Expression,
    }
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::LetStatement { ident: name, expr: value } => {
                f.write_str("let ")?;
                f.write_str(name)?;
                f.write_str(" = ")?;
                f.write_str(value.to_string().as_str())?;
            },
            Statement::ReturnStatement { expr: value } => {
                f.write_str("return ")?;
                f.write_str(value.to_string().as_str())?;
            },
            Statement::ExpressionStatement { expr: expression } => {
                f.write_str(expression.to_string().as_str())?;
            }
        }
        f.write_char(';')?;
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    IdentifierLiteral(StopRawLiteral),
    IntegerLiteral(StopInteger),
    PrefixExpression {
        op: Operator,
        right: Box<Expression>,
    },
    InfixExpression {
        left: Box<Expression>,
        op: Operator,
        right: Box<Expression>,
    },
    CallExpression {
        ident: StopRawLiteral,
        args: Vec<Expression>,
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::IdentifierLiteral(ident) => f.write_str(ident)?,
            Expression::IntegerLiteral(int) => f.write_str(int.to_string().as_str())?,
            Expression::PrefixExpression { op, right } => {
                f.write_char('(')?;
                f.write_str(<Operator as Into<&str>>::into(*op))?;
                f.write_str(right.to_string().as_str())?;
                f.write_char(')')?;
            },
            Expression::InfixExpression { left, op, right } => {
                f.write_char('(')?;
                f.write_str(left.to_string().as_str())?;
                f.write_char(' ')?;
                f.write_str(<Operator as Into<&str>>::into(*op))?;
                f.write_char(' ')?;
                f.write_str(right.to_string().as_str())?;
                f.write_char(')')?;
            },
            Expression::CallExpression { ident, args } => {
                f.write_str(ident)?;
                f.write_char('(')?;
                if !args.is_empty() {
                    f.write_str(args[0].to_string().as_str())?;
                    for arg in args.iter().skip(1) {
                        f.write_str(", ")?;
                        f.write_str(arg.to_string().as_str())?;
                    }
                }
                f.write_char(')')?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_displays_let_statement_ast() {
        // let myVar = add(1, 2);
        let let_statement = Statement::LetStatement {
            ident: StopRawLiteral::from("myVar"),
            expr: Expression::CallExpression {
                ident: StopRawLiteral::from("add"),
                args: vec![
                    Expression::IntegerLiteral(1),
                    Expression::IntegerLiteral(2),
                ],
            },
        };

        assert_eq!(
            "let myVar = add(1, 2);",
            let_statement.to_string()
        );
    }

    #[test]
    fn it_displays_return_statement_ast() {
        // return add(5, 5) + 5;
        let return_statement = Statement::ReturnStatement {
            expr: Expression::InfixExpression {
                left: Box::new(Expression::CallExpression {
                    ident: StopRawLiteral::from("add"),
                    args: vec![
                        Expression::IntegerLiteral(5),
                        Expression::IntegerLiteral(5),
                    ],
                }),
                op: Operator::Plus,
                right: Box::new(Expression::IntegerLiteral(5)),
            },
        };

        assert_eq!(
            "return (add(5, 5) + 5);",
            return_statement.to_string(),
        );
    }

    #[test]
    fn it_displays_expression_statement_ast() {
        // add(1, 2);
        let expr_statement = Statement::ExpressionStatement {
            expr: Expression::CallExpression {
                ident: StopRawLiteral::from("add"),
                args: vec![
                    Expression::IntegerLiteral(1),
                    Expression::IntegerLiteral(2),
                ],
            },
        };

        assert_eq!(
            "add(1, 2);",
            expr_statement.to_string(),
        );
    }

    #[test]
    fn it_displays_program_ast() {
        // let x = 5;
        // let y = add(x, 10);
        // x;
        // y;

        let program = Program::new(vec![
            Statement::LetStatement {
                ident: StopRawLiteral::from("x"),
                expr: Expression::IntegerLiteral(5),
            },
            Statement::LetStatement {
                ident: StopRawLiteral::from("y"),
                expr: Expression::CallExpression {
                    ident: StopRawLiteral::from("add"),
                    args: vec![
                        Expression::IdentifierLiteral(StopRawLiteral::from("x")),
                        Expression::IntegerLiteral(10),
                    ],
                }
            },
            Statement::ExpressionStatement {
                expr: Expression::IdentifierLiteral(StopRawLiteral::from("x")),
            },
            Statement::ExpressionStatement {
                expr: Expression::IdentifierLiteral(StopRawLiteral::from("y")),
            },
        ]);

        assert_eq!(
            concat!(
                "let x = 5;\n",
                "let y = add(x, 10);\n",
                "x;\n",
                "y;",
            ),
            program.to_string(),
        );
    }

    #[test]
    fn it_displays_expressions() {
        let expected = "(5 + (10 * 2))";

        test_expression(
            Expression::InfixExpression {
                left: Box::new(Expression::IntegerLiteral(5)),
                op: Operator::Plus,
                right: Box::new(Expression::InfixExpression {
                    left: Box::new(Expression::IntegerLiteral(10)),
                    op: Operator::Splat,
                    right: Box::new(Expression::IntegerLiteral(2)),
                }),
            },
            expected);
    }

    fn test_expression(input: Expression, expected: &str)
    {
        assert_eq!(expected, input.to_string());
    }
}