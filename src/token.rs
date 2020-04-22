#[derive(Clone, Debug, PartialEq)]
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

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", 
            match self {
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

                Token::Function => "fn".to_string(),
                Token::Let => "let".to_string(),
                Token::If => "if".to_string(),
                Token::Return => "return".to_string(),
                Token::Else => "else".to_string(),
                Token::True => "true".to_string(),
                Token::False => "false".to_string(),

                Token::Equal => "==".to_string(),
                Token::NotEqual => "!=".to_string()
            })
    }
}
