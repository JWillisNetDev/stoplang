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
        if self.statements.is_empty() {
            return Ok(());
        }
        f.write_str(self.statements[0].to_string().as_str())?;
        for statement in self.statements.iter().skip(1) {
            f.write_char('\n')?;
            f.write_str(statement.to_string().as_str())?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    LetStatement {
        name: StopRawLiteral,
        value: Expression,
    },
    ReturnStatement {
        value: Expression,
    },
    ExpressionStatement {
        expression: Expression,
    }
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::LetStatement { name, value } => {
                f.write_str("let ")?;
                f.write_str(name)?;
                f.write_str(" = ")?;
                f.write_str(value.to_string().as_str())?;
            },
            Statement::ReturnStatement { value } => {
                f.write_str("return ")?;
                f.write_str(value.to_string().as_str())?;
            },
            Statement::ExpressionStatement { expression } => {
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
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::IdentifierLiteral(ident) => f.write_str(ident)?,
            Expression::IntegerLiteral(int) => f.write_str(int.to_string().as_str())?,
            Expression::PrefixExpression { op, right } => {
                f.write_str("(")?;
                f.write_str(<Operator as Into<&str>>::into(*op))?;
                f.write_str(right.to_string().as_str())?;
                f.write_str(")")?;
            }
        }
        Ok(())
    }
}