use crate::ast;
use crate::token::Token;
use crate::lexer::Lexer;

pub struct Parser<'a> {
    l: Lexer<'a>,
    cur_token: Token,
    peek_token: Token
}

impl<'a> Parser<'a> {
    pub fn new(mut l: Lexer<'a>) -> Parser {
        let tok = (l.next_token(), l.next_token());
        Parser {
            l: l,
            cur_token: tok.0,
            peek_token: tok.1
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
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<ast::Statement> {
        let ct = self.cur_token.clone();
        self.next_token();

        let n = match self.cur_token.clone() {
            Token::Ident(x) => {
                ast::Expression::Identifier(Token::Ident(x))
            },
            _ => return None
            
        };
        
        if !self.expect_peek(Token::Assign) {
            return None;
        }

        while !self.cur_token_is(Token::Semicolon) {
            self.next_token();
        }
        
        Some(ast::Statement::LetStatement{ token: ct, name: n.clone(), value: n})
    }

    fn cur_token_is(&self, t: Token) -> bool {
        self.cur_token == t
    }

    fn peek_token_is(&self, t: Token) -> bool {
        self.peek_token == t
    }

    fn expect_peek(&mut self, t: Token) -> bool {
        if self.peek_token_is(t) {
            self.next_token(); true
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::token::Token;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::ast;

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

        assert_eq!(program.len(), 3);
        
        let tests = vec![
            "x",
            "y",
            "foobar"
        ];

        for (i, tt) in tests.iter().enumerate() {
            match &program[i] {
                ast::Statement::LetStatement{ token: t, name: n, value: v } => {
                    assert_eq!(*t, Token::Let);
                    assert_eq!(*n, ast::Expression::Identifier(Token::Ident(tt.to_string())));
                },
                _ => panic!("It isn't LetStatement!")
            };
        }
    }
}
