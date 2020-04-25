use crate::token::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    LetStatement { name: Expression, value: Expression },
    ReturnStatement { value: Expression },
    ExpressionStatement { expr: Expression }
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", match self {
           Statement::LetStatement{ name, value } => format!("let {} = {};", name, value),
           Statement::ReturnStatement{ value } => format!("return {};", value),
           Statement::ExpressionStatement{ expr } => format!("{}", expr),
        })
    }
}


#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Identifier(Token),
    IntegerLiteral(Token),
    Boolean(Token),
    PrefixExpression{ opr: Token, right: Box<Expression> },
    InfixExpression{ left: Box<Expression>, opr: Token, right: Box<Expression> },
    IfExpression{ condition: Box<Expression>, conseqence: Program, alternative: Option<Program> },
    FnLiteral{ parameters: Vec<Expression>, body: Program },
    CallExpression{ function: Box<Expression>, arguments: Vec<Expression> },
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", match self {
            Expression::Identifier(x) => format!("{}", x),
            Expression::IntegerLiteral(x) => format!("{}", x),
            Expression::Boolean(x) => format!("{}", x),
            Expression::PrefixExpression{ opr, right } => format!("({}{})", opr, right),
            Expression::InfixExpression{ left, opr, right } => format!("({} {} {})", left, opr, right),
            Expression::IfExpression{ condition, conseqence, alternative } => {
                if let Some(alt) = alternative {
                    format!("if {} {} else {}", condition, conseqence, alt)
                } else {
                    format!("if {} {}", condition, conseqence)
                }
            },
            Expression::FnLiteral{ parameters, body } => {
                format!("fn({}) {{ {} }}", 
                    parameters.iter().enumerate().fold(
                        String::new(), |s, (i, p)| {
                            println!("{:?} {:?}", i, p);
                            if i != parameters.len()-1 {
                                format!("{}{}, ", s, p)
                            } else {
                                format!("{}{}", s, p)
                            }}
                        )
                    , body)
            },
            Expression::CallExpression{ function, arguments } => {
                format!("{}({})", 
                    function,
                    arguments.iter().enumerate().fold(
                        String::new(), |s, (i, p)| {
                            println!("{:?} {:?}", i, p);
                            if i != arguments.len()-1 {
                                format!("{}{}, ", s, p)
                            } else {
                                format!("{}{}", s, p)
                            }}
                    ))
            }
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Program (Vec<Statement>);

impl std::ops::Deref for Program {
    type Target = Vec<Statement>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for Program {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Program {
    pub fn new() -> Program {
        Program(Vec::new())
    }
    
    pub fn new_with_vec(v: Vec<Statement>) -> Program {
        Program(v)
    }
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut s: String = "".to_string();
        for i in &*self.clone() {
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
        let program = ast::Program::new_with_vec(vec![
            ast::Statement::LetStatement{ name: ast::Expression::Identifier(Token::Ident("myVar".to_string())), value: ast::Expression::Identifier(Token::Int(15)) },
            ast::Statement::ReturnStatement{ value: ast::Expression::Identifier(Token::Ident("aaa".to_string())) },
            ast::Statement::ExpressionStatement{ expr: ast::Expression::FnLiteral{
                parameters: vec![ast::Expression::Identifier(Token::Ident("x".to_string())), ast::Expression::Identifier(Token::Ident("y".to_string()))],
                body: ast::Program(vec![ ast::Statement::ReturnStatement{ value: ast::Expression::InfixExpression{ left: Box::new(ast::Expression::Identifier(Token::Ident("x".to_string()))) , opr: Token::Gt, right: Box::new(ast::Expression::Identifier(Token::Ident("y".to_string()))) } } ])
            } }
        ]);

        let tests = vec![
            "let myVar = 15;",
            "return aaa;",
            "fn(x, y) { return (x > y); }"
        ];

        for (i, tt) in tests.iter().enumerate() {
            assert_eq!(**tt, format!("{}", program[i]));
        }
    }
}
