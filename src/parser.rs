use crate::ast;
use crate::token::Token;
use crate::lexer::Lexer;

pub enum Precedence {
    Lowest,
    Equals,
    Lessgreater,
    Sum,
    Product,
    Prefix,
    Call

}

#[derive(Debug, Clone)]
pub struct ParseError {
    msg: String
}

pub struct Parser<'a> {
    l: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(mut l: Lexer<'a>) -> Parser {
        let tok = (l.next_token(), l.next_token());
        Parser {
            l: l,
            cur_token: tok.0,
            peek_token: tok.1,
            errors: Vec::new()
        }
    }

    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    pub fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program::new();
        while !self.cur_token_is(Token::Eof) {
            if let Some(x) = self.parse_statement() {
                program.push(x);
            }
            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.cur_token {
            Token::Let => return self.parse_let_statement(),
            Token::Return => return self.parse_return_statement(),
            _ => return self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<ast::Statement> {
        let ct = self.cur_token.clone();
        self.next_token();

        let n = match self.cur_token.clone() {
            Token::Ident(x) => {
                ast::Expression::Identifier(Token::Ident(x))
            },
            _ => { 
                self.cur_error(Token::Ident("SOMETHING".to_string())); 
                return None;
            }
            
        };
        
        if !self.expect_peek(Token::Assign) {
            return None;
        }

        while !self.cur_token_is(Token::Semicolon) {
            self.next_token();
        }
        
        Some(ast::Statement::LetStatement{ token: ct, name: n.clone(), value: n})
    }
    
    fn parse_return_statement(&mut self) -> Option<ast::Statement> {
        let ct = self.cur_token.clone();
        self.next_token();

        while !self.cur_token_is(Token::Semicolon) {
            self.next_token();
        }

        Some(ast::Statement::ReturnStatement{ token: ct, value: ast::Expression::Identifier(Token::Semicolon)})
    }

    fn parse_expression_statement(&mut self) -> Option<ast::Statement> {
        let ct = self.cur_token.clone();
        let expr = self.parse_expression(Precedence::Lowest);

        if !self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }

        if let Some(x) = expr {
            Some(ast::Statement::ExpressionStatement{ token: ct, expr: x })
        } else {
            None
        }
    }

    fn parse_expression(&self, prc: Precedence) -> Option<ast::Expression> {
        let left_expr = self.prefix_parse_fns(prc);
        return left_expr;
    }

    fn prefix_parse_fns(&self, _prc: Precedence) -> Option<ast::Expression> {
        match self.cur_token.clone() {
            t @ Token::Ident(_) => Some(self.parse_idetifier(t)),
            _ => None
        }
    }

    fn parse_idetifier(&self, t: Token) -> ast::Expression {
        ast::Expression::Identifier(t)
    }

    fn cur_token_is(&self, t: Token) -> bool {
        self.cur_token == t
    }

    fn peek_token_is(&self, t: Token) -> bool {
        self.peek_token == t
    }

    fn expect_peek(&mut self, t: Token) -> bool {
        if self.peek_token_is(t.clone()) {
            self.next_token(); true
        } else {
            self.peek_error(t); false
        }
    }

    pub fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn cur_error(&mut self, t: Token) {
        self.errors.push(format!("expected next token to be {:?}, got {:?} instead", t, self.cur_token));
    }

    fn peek_error(&mut self, t: Token) {
        self.errors.push(format!("expected next token to be {:?}, got {:?} instead", t, self.peek_token));
    }
}

#[cfg(test)]
mod tests {
    use crate::token::Token;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::ast;

    fn check_parser_errors(p: &mut Parser) {
        let errors = p.errors();
        if errors.len() == 0 {
            return;
        }

        println!("parser has {} errors", errors.len());
        for msg in errors {
            println!("parser error: {}", msg);
        }
        panic!("paniced");
    }

    #[test]
    fn test_let_statements() {
        let input = r#"
let x = 5;
let y = 10;
let foobar = 838383;
        "#;
        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&mut p);

        assert_eq!(program.len(), 3);
        
        let tests = vec![
            "x",
            "y",
            "foobar"
        ];

        for (i, tt) in tests.iter().enumerate() {
            match &program[i] {
                ast::Statement::LetStatement{ token: t, name: n, value: _v } => {
                    assert_eq!(*t, Token::Let);
                    assert_eq!(*n, ast::Expression::Identifier(Token::Ident(tt.to_string())));
                },
                _ => panic!("It isn't LetStatement!")
            };
        }
    }

    #[test]
    fn test_return_statements() {
        let input = r#"
return 5;
return 10;
return 993322;
        "#;
        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&mut p);

        assert_eq!(program.len(), 3);
        
        for stmt in program {
            match stmt {
                ast::Statement::ReturnStatement{ token: t, .. } =>
                    assert_eq!(t, Token::Return),
                _ => panic!("It isn't LetStatement!")
            };
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = r#"
            foobar;
        "#;
        
        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&mut p);

        assert_eq!(program.len(), 1);

        let stmt = program[0].clone();
        if let ast::Statement::ExpressionStatement{ expr: e, .. } = stmt {
            if let ast::Expression::Identifier(x) = e {
                if let Token::Ident(id) = x {
                    assert_eq!(id, "foobar".to_string());
                } else { assert!(false); }
            } else { assert!(false); }
        } else { assert!(false); }

    }
}
