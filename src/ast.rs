use crate::token::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    LetStatement { token: Token, name: Expression, value: Expression },
    ReturnStatement { token: Token, value: Expression },
    ExpressionStatement { token: Token, expr: Expression }
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", match self {
           Statement::LetStatement{ token, name, value: _ } => format!("{} {} = {};", token, if let Expression::Identifier(x) = name { x } else { &Token::Illegal }, Token::Illegal),
           Statement::ReturnStatement{ token, value: _ } => format!("{} {};", token, Token::Illegal),
           _ => "".to_string()
        })
    }
}


#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Identifier(Token)
}

pub type Program = Vec<Statement>;

#[cfg(test)]
mod tests {
    use crate::token::Token;
    use crate::ast;
    
    #[test]
    fn test_string() {
        let program: ast::Program = vec![
            ast::Statement::LetStatement{ token: Token::Let, name: ast::Expression::Identifier(Token::Ident("myVar".to_string())), value: ast::Expression::Identifier(Token::Illegal) },
            ast::Statement::ReturnStatement{ token: Token::Return, value: ast::Expression::Identifier(Token::Illegal) },
        ];

        let tests = vec![
            "let myVar = ILLEGAL;",
            "return ILLEGAL;"
        ];

        for (i, tt) in tests.iter().enumerate() {
            assert_eq!(**tt, format!("{}", program[i]));
        }
    }
}
