use crate::{lex::Token, StopIdentifier, StopInteger};

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

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    LetStatement {
        name: StopIdentifier,
        value: Expression,
    },
    ReturnStatement {
        value: Expression,
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    IdentifierLiteral(StopIdentifier),
    IntegerLiteral(StopInteger),
    PrefixExpression {
        operator: Token,
        right: Box<Expression>,
    },
}
