use crate::ast;
use crate::token::Token;
use crate::lexer::Lexer;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,
    Lessgreater,
    Sum,
    Product,
    Prefix,
    Call
}

fn to_precedence(t: &Token) -> Precedence {
    match t {
        Token::Equal | Token::NotEqual => Precedence::Equals,
        Token::Lt | Token:: Gt => Precedence::Lessgreater,
        Token::Plus | Token::Minus => Precedence::Sum,
        Token::Slash | Token:: Asterisk => Precedence::Product,
        Token::Lparen => Precedence::Call,
        _ => Precedence::Lowest
    }
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
        self.next_token();

        let val = if let Some(v) = self.parse_expression(Precedence::Lowest) {
            v } else { return None };

        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }
        
        Some(ast::Statement::LetStatement{ name: n.clone(), value: val })
    }
    
    fn parse_return_statement(&mut self) -> Option<ast::Statement> {
        self.next_token();

        let val = if let Some(v) = self.parse_expression(Precedence::Lowest) {
            v } else { return None };

        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }

        Some(ast::Statement::ReturnStatement{ value: val})
    }

    fn parse_expression_statement(&mut self) -> Option<ast::Statement> {
        let expr = self.parse_expression(Precedence::Lowest);

        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }

        if let Some(x) = expr {
            Some(ast::Statement::ExpressionStatement{ expr: x })
        } else {
            None
        }
    }

    fn no_prefix_parse_fn_error(&mut self, t: Token) {
        self.errors.push(format!("no prefix parse function for {} found", t))
    }

    fn parse_expression(&mut self, prc: Precedence) -> Option<ast::Expression> {
        let mut left_expr = match self.cur_token {
            Token::Ident(_) => self.parse_ident(),
            Token::Int(_) => self.parse_int_literal(),
            Token::True | Token::False => self.parse_boolean(),
            Token::Bang | Token::Minus => self.parse_prefix_expression(),
            Token::Lparen => self.parse_grouped_expression(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_fn_literal(),
            _ => {
                self.no_prefix_parse_fn_error(self.cur_token.clone());
                return None;
            }
        };

        while prc < self.peek_precedence()  {
            match self.peek_token {
                Token::Plus | Token::Minus | Token::Slash | Token::Asterisk | Token::Equal | Token::NotEqual | Token::Lt | Token::Gt => {
                    self.next_token();
                    left_expr = self.parse_infix_expression(left_expr.unwrap());
                },
                Token::Lparen => {
                    self.next_token();
                    left_expr = self.parse_call_expression(left_expr.unwrap());
                }
                _ => break
            }
        }
        left_expr
    }

    fn parse_ident(&self) -> Option<ast::Expression> {
        Some(ast::Expression::Identifier(self.cur_token.clone()))
    }

    fn parse_int_literal(&self) -> Option<ast::Expression> {
        Some(ast::Expression::IntegerLiteral(self.cur_token.clone()))
    }

    fn parse_boolean(&self) -> Option<ast::Expression> {
        Some(ast::Expression::Boolean(self.cur_token.clone()))
    }

    fn parse_grouped_expression(&mut self) -> Option<ast::Expression> {
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest);
        if !self.expect_peek(Token::Rparen) {
            return None;
        }

        expr
    }

    fn parse_block_statement(&mut self) -> ast::Program {
        let mut program = ast::Program::new();
        self.next_token();

        while !self.cur_token_is(Token::Rbrace) && !self.cur_token_is(Token::Eof) {
            if let Some(stmt) = self.parse_statement() {
                program.push(stmt);
            }
            self.next_token()
        }
        
        program
    }

    fn parse_if_expression(&mut self) -> Option<ast::Expression> {
        if !self.expect_peek(Token::Lparen) { return None }
        self.next_token();

        let cd = self.parse_expression(Precedence::Lowest);

        if cd != None && !self.expect_peek(Token::Rparen) { return None }
        if !self.expect_peek(Token::Lbrace) { return None }

        let cns = self.parse_block_statement();

        if self.peek_token_is(Token::Else) {
            self.next_token();
            if !self.expect_peek(Token::Lbrace) { return None }
        }
        let alt = self.parse_block_statement();

        Some(ast::Expression::IfExpression { condition: Box::new(cd.unwrap()), conseqence: cns, alternative: if alt.len() != 0 {Some(alt)} else {None} })
    }

    fn parse_fn_literal(&mut self) -> Option<ast::Expression> {
        if !self.expect_peek(Token::Lparen) { return None }

        let p = self.parse_fn_parameters();
        if p == None || !self.expect_peek(Token::Lbrace) { return None }
        
        Some(ast::Expression::FnLiteral{ parameters: p.unwrap(), body: self.parse_block_statement()})
    }

    fn parse_fn_parameters(&mut self) -> Option<Vec<ast::Expression>> {
        let mut idents = Vec::new();
        if self.peek_token_is(Token::Rparen) { self.next_token(); return Some(idents) }

        self.next_token();
        idents.push(ast::Expression::Identifier(self.cur_token.clone()));

        while self.peek_token_is(Token::Comma) {
            self.next_token(); self.next_token();
            idents.push(ast::Expression::Identifier(self.cur_token.clone()));
        }

        if !self.expect_peek(Token::Rparen) { return None }
        Some(idents)
    }

    fn parse_call_expression(&mut self, function: ast::Expression) -> Option<ast::Expression> {
        if let Some(args) = self.parse_call_arguments() {
            Some(ast::Expression::CallExpression{ function: Box::new(function), arguments: args })
        } else { None }
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<ast::Expression>> {
        let mut args = Vec::new();

        if self.peek_token_is(Token::Rparen) { self.next_token(); return Some(args) }
        self.next_token();
        if let Some(arg) = self.parse_expression(Precedence::Lowest) {
            args.push(arg);
        }

        while self.peek_token_is(Token::Comma) {
            self.next_token(); self.next_token();
            if let Some(arg) = self.parse_expression(Precedence::Lowest) {
                args.push(arg);
            }
        }

        if !self.expect_peek(Token::Rparen) { return None }

        Some(args)
    }

    fn parse_prefix_expression(&mut self) -> Option<ast::Expression> {
        let opr = self.cur_token.clone();
        self.next_token();
        Some(ast::Expression::PrefixExpression{ opr: opr, right: Box::new(self.parse_expression(Precedence::Prefix).unwrap())})
    }

    fn parse_infix_expression(&mut self, left: ast::Expression) -> Option<ast::Expression> {
        let opr = self.cur_token.clone();
        let prc = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(prc).unwrap();
        Some(ast::Expression::InfixExpression{ left: Box::new(left), opr: opr, right: Box::new(right)})
    }

    fn cur_precedence(&self) -> Precedence {
        to_precedence(&self.cur_token)
    }

    fn peek_precedence(&self) -> Precedence {
        to_precedence(&self.peek_token)
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

    fn test_integer_literal(il: ast::Expression, num: i64) {
        if let ast::Expression::IntegerLiteral(x) = il {
            if let Token::Int(n) = x {
                assert_eq!(n, num);
            } else { assert!(false); }
        } else { assert!(false); }
    }

    fn test_boolean(bl: ast::Expression, b: bool) {
        if let ast::Expression::Boolean(x) = bl {
            match x {
                Token::True => assert_eq!(b, true),
                Token::False => assert_eq!(b, false),
                _ => assert!(false)
            }
        } else { assert!(false); }
    }

    fn test_identifier(expr: ast::Expression, value: &str) {
        if let ast::Expression::Identifier(x) = expr {
            if let Token::Ident(id) = x {
                assert_eq!(id, value.to_string());
            } else { assert!(false); }
        } else { assert!(false); }
    }

    fn test_expression(expr: ast::Expression, expected: ast::Expression) {
        match expected {
            ast::Expression::IntegerLiteral(x) => {
                if let Token::Int(n) = x {
                    test_integer_literal(expr, n);
                } else { assert!(false); }
            },
            ast::Expression::Identifier(x) => {
                if let Token::Ident(n) = x {
                    test_identifier(expr, &*n);
                } else { assert!(false); }
            },
            ast::Expression::InfixExpression{ left, opr, right } => {
                test_infix_expression(expr, *left, opr, *right);
            }
            _ => assert!(false)
        }
    }

    fn test_infix_expression(expr: ast::Expression, l: ast::Expression, o: Token, r: ast::Expression) {
        match expr {
            ast::Expression::InfixExpression{ left, opr, right, .. } => {
                assert_eq!(l, *left);
                assert_eq!(o, opr);
                assert_eq!(r, *right);
            },
            _ => assert!(false)
        }
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
                ast::Statement::LetStatement{ name: n, value: _v } => {
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
        
        for stmt in &*program {
            match stmt {
                ast::Statement::ReturnStatement{ .. } =>
                    assert!(true),
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
            test_identifier(e, "foobar");
        } else { assert!(false); }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = r#"5;"#;

        let l= Lexer::new(input);
        let mut p = Parser::new(l);
        
        let program = p.parse_program();
        check_parser_errors(&mut p);

        assert_eq!(program.len(), 1);

        let stmt = program[0].clone();
        if let ast::Statement::ExpressionStatement{ expr: e, .. } = stmt {
            test_integer_literal(e, 5);
        } else { assert!(false); }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let tests = vec![
            ("!5;", Token::Bang, 5),
            ("-15;", Token::Minus, 15),
        ];

        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&mut p);

            assert_eq!(program.len(), 1);

            let stmt = program[0].clone();

            if let ast::Statement::ExpressionStatement{ expr: e, .. } = stmt {
                if let ast::Expression::PrefixExpression{ opr, right } = e {
                    assert_eq!(opr, tt.1);
                    test_integer_literal(*right, tt.2);
                } else { assert!(false); }
            } else { assert!(false); }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let tests = vec![
            ("5 + 6;", 5, Token::Plus, 6),
            ("5 - 5;", 5, Token::Minus, 5),
            ("5 * 5;", 5, Token::Asterisk, 5),
            ("5 / 5;", 5, Token::Slash, 5),
            ("5 > 5;", 5, Token::Gt, 5),
            ("5 < 5;", 5, Token::Lt, 5),
            ("5 == 5;", 5, Token::Equal, 5),
            ("5 != 5;", 5, Token::NotEqual, 5),
        ];

        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&mut p);

            assert_eq!(program.len(), 1);

            let stmt = program[0].clone();

            if let ast::Statement::ExpressionStatement{ expr: e, .. } = stmt {
                if let ast::Expression::InfixExpression{ left, opr, right } = e {
                    assert_eq!(opr, tt.2);
                    test_integer_literal(*left, tt.1);
                    test_integer_literal(*right, tt.3);
                } else { assert!(false); }
            } else { assert!(false); }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            (r#"3 > 5 == false"#, r#"((3 > 5) == false)"#),
            (r#"3 < 5 == true"#, r#"((3 < 5) == true)"#),
            (r#"-a * b"#, r#"((-a) * b)"#),
            (r#"!-a"#, r#"(!(-a))"#),
            (r#"a + b + c"#, r#"((a + b) + c)"#),
            (r#"a + b - c"#, r#"((a + b) - c)"#),
            (r#"a * b * c"#, r#"((a * b) * c)"#),
            (r#"a * b / c"#, r#"((a * b) / c)"#),
            (r#"a + b / c"#, r#"(a + (b / c))"#),
            (r#"a + b * c + d / e - f"#, r#"(((a + (b * c)) + (d / e)) - f)"#),
            (r#"3 + 4; -5 * 5"#, r#"(3 + 4)((-5) * 5)"#),
            (r#"5 > 4 == 3 < 4"#, r#"((5 > 4) == (3 < 4))"#),
            (r#"5 > 4 != 3 < 4"#, r#"((5 > 4) != (3 < 4))"#),
            (r#"3 + 4 * 5 == 3 * 1 + 4 * 5"#, r#"((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"#),
            (r#"1 + (2 + 3) + 4"#, r#"((1 + (2 + 3)) + 4)"#),
            (r#"(5 + 5) * 2"#, r#"((5 + 5) * 2)"#),
            (r#"2 / (5 + 5)"#, r#"(2 / (5 + 5))"#),
            (r#"-(5 + 5)"#, r#"(-(5 + 5))"#),
            (r#"!(true == true)"#, r#"(!(true == true))"#),
        ];

        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&mut p);
            assert_eq!(format!("{}", program), tt.1);
        }
    }

    #[test]
    fn test_boolean_expression() {
        let tests = vec![
            ("true", true),
            ("false", false),
        ];
        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&mut p);
            assert_eq!(program.len(), 1);
            if let ast::Statement::ExpressionStatement{ expr: e, .. } = program[0].clone() {
                test_boolean(e, tt.1);
            } else { assert!(false); }
        }
    }

    #[test]
    fn test_if_expression() {
        let input = r#"if (x < y) { x }"#;
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&mut p);

        assert_eq!(1, program.len());

        let stmt = program[0].clone();

        match stmt {
            ast::Statement::ExpressionStatement{ expr } => {
                match expr {
                    ast::Expression::IfExpression{ condition, conseqence, alternative } => {
                        test_infix_expression(*condition, ast::Expression::Identifier(Token::Ident("x".to_string())), Token::Lt, ast::Expression::Identifier(Token::Ident("y".to_string())));
                        assert_eq!(conseqence.len(), 1);
                        assert_eq!(alternative, None);
                    },
                    _ => assert!(false)
                }
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = r#"if (x < y) { x } else { y }"#;
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&mut p);

        assert_eq!(1, program.len());

        let stmt = program[0].clone();

        match stmt {
            ast::Statement::ExpressionStatement{ expr } => {
                match expr {
                    ast::Expression::IfExpression{ condition, conseqence, alternative } => {
                        test_infix_expression(*condition, ast::Expression::Identifier(Token::Ident("x".to_string())), Token::Lt, ast::Expression::Identifier(Token::Ident("y".to_string())));
                        assert_eq!(conseqence.len(), 1);
                        assert_ne!(alternative, None);
                        assert_eq!(alternative.unwrap().len(), 1);
                    },
                    _ => assert!(false)
                }
            },
            _ => assert!(false)
        }
    }

    #[test]
    fn test_fn_literal_expression() {
        let input = r#"fn(x, y) { x + y; }"#;
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&mut p);

        assert_eq!(1, program.len());

        let stmt = program[0].clone();

        match stmt {
            ast::Statement::ExpressionStatement{ expr } => {
                match expr {
                    ast::Expression::FnLiteral{ parameters, body } => {
                        assert_eq!(parameters.len(), 2);
                        assert_eq!(parameters[0], ast::Expression::Identifier(Token::Ident("x".to_string())));
                        assert_eq!(parameters[1], ast::Expression::Identifier(Token::Ident("y".to_string())));

                        assert_eq!(body.len(), 1);
                        match body[0].clone() {
                            ast::Statement::ExpressionStatement{ expr } => {
                                test_infix_expression(expr, ast::Expression::Identifier(Token::Ident("x".to_string())), Token::Plus, ast::Expression::Identifier(Token::Ident("y".to_string())))
                            },
                            _ => panic!("It is not ExpressionStatement.")
                        }
                    },
                    _ => panic!("It is not FnLiteral.")
                }
            },
            _ => panic!("It is not ExpressionStatement.")
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = r#"add(1, 2 * 3, 4 + 5)"#;
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&mut p);

        assert_eq!(1, program.len());

        let stmt = program[0].clone();

        match stmt {
            ast::Statement::ExpressionStatement{ expr } => {
                match expr {
                    ast::Expression::CallExpression{ function, arguments } => {
                        assert_eq!(arguments.len(), 3);
                        assert_eq!(*function, ast::Expression::Identifier(Token::Ident("add".to_string())));

                        test_integer_literal(arguments[0].clone(), 1);
                        test_infix_expression(arguments[1].clone(), ast::Expression::IntegerLiteral(Token::Int(2)), Token::Asterisk, ast::Expression::IntegerLiteral(Token::Int(3)));
                        test_infix_expression(arguments[2].clone(), ast::Expression::IntegerLiteral(Token::Int(4)), Token::Plus, ast::Expression::IntegerLiteral(Token::Int(5)));
                    },
                    _ => panic!("It is not FnLiteral.")
                }
            },
            _ => panic!("It is not ExpressionStatement.")
        }
    }
}
