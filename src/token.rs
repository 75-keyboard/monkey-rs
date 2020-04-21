#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Illegal,
    Eof,

    Ident(String),
    Int(i64),

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Lt,
    Gt,

    Comma,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    Function,
    Let,
    If,
    Return,
    Else,
    True,
    False,

    Equal,
    NotEqual
}

impl Token {
    pub fn token_literal(token: Token) -> String {
        match token {
            Token::Illegal => "ILLEGAL".to_string(),
            Token::Eof => "EOF".to_string(),

            Token::Ident(x) => x.clone(),
            Token::Int(x) => x.to_string(),

            Token::Assign => "=".to_string(),
            Token::Plus => "+".to_string(),
            Token::Minus => "-".to_string(),
            Token::Bang => "!".to_string(),
            Token::Asterisk => "*".to_string(),
            Token::Slash => "/".to_string(),

            Token::Lt => "<".to_string(),
            Token::Gt => ">".to_string(),

            Token::Comma => ".to_string(),".to_string(),
            Token::Semicolon => ";".to_string(),

            Token::Lparen => "(".to_string(),
            Token::Rparen => ")".to_string(),
            Token::Lbrace => "{".to_string(),
            Token::Rbrace => "}".to_string(),

            Token::Function => "FUNCTION".to_string(),
            Token::Let => "LET".to_string(),
            Token::If => "IF".to_string(),
            Token::Return => "RETURN".to_string(),
            Token::Else => "ELSE".to_string(),
            Token::True => "TRUE".to_string(),
            Token::False => "FALSE".to_string(),

            Token::Equal => "==".to_string(),
            Token::NotEqual => "!=".to_string()
        }
    }
}
