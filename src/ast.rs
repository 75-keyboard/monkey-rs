use crate::token::Token;
use crate::lexer::Lexer;

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    LetStatement { token: Token, name: Expression, value: Expression },
    ReturnStatement { token: Token, value: Expression }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Identifier(Token)
}

pub type Program = Vec<Statement>;
