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
           Statement::LetStatement{ token, name, value } => format!("{} {} = {};", token, name, value),
           Statement::ReturnStatement{ token, value } => format!("{} {};", token, value),
           Statement::ExpressionStatement{ token: _, expr } => format!("{}", expr),
        })
    }
}


#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Identifier(Token),
    IntegerLiteral(Token),
    PrefixExpression{ token: Token, opr: String, right: Box<Expression> },
    InfixExpression{ token: Token, left: Box<Expression>, opr: String, right: Box<Expression> },
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", match self {
            Expression::Identifier(x) => format!("{}", x),
            Expression::IntegerLiteral(x) => format!("{}", x),
            Expression::PrefixExpression{ token: _, opr, right } => format!("({}{})", opr, right),
            Expression::InfixExpression{ token: _, left, opr, right } => format!("({} {} {})", left, opr, right),
        })
    }
}

pub struct Program {
    pub statements: Vec<Statement>
}

impl Program {
    pub fn new() -> Program {
        Program { statements: Vec::new() }
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut s: String = "".to_string();
        for i in self.statements.clone() {
            s = format!("{}{}", s, i);
        }

        write!(f, "{}", s)
    }
}

#[cfg(test)]
mod tests {
    use crate::token::Token;
    use crate::ast;
    
    #[test]
    fn test_string() {
        let mut program: ast::Program = ast::Program::new();
        program.statements = vec![
            ast::Statement::LetStatement{ token: Token::Let, name: ast::Expression::Identifier(Token::Ident("myVar".to_string())), value: ast::Expression::Identifier(Token::Int(15)) },
            ast::Statement::ReturnStatement{ token: Token::Return, value: ast::Expression::Identifier(Token::Ident("aaa".to_string())) },
        ];

        let tests = vec![
            "let myVar = 15;",
            "return aaa;"
        ];

        for (i, tt) in tests.iter().enumerate() {
            assert_eq!(**tt, format!("{}", program.statements[i]));
        }
    }
}
