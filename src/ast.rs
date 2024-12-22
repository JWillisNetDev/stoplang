use crate::{StopIdentifier, StopInteger};

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    LetStatement { name: StopIdentifier, value: Expression },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    IdentifierLiteral(StopIdentifier),
    IntegerLiteral(StopInteger),
}

