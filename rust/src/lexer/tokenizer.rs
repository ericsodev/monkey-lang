#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Illegal,
    EOF,

    Ident,
    Int,

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Gt,
    Lt,
    Eq,
    Neq,

    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    Fn,
    Let,
    Return,
    True,
    False,
    If,
    Else,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
    pub line_num: usize,
    pub column_num: usize,
}

impl Default for Token {
    fn default() -> Self {
        Token {
            line_num: 0,
            column_num: 0,
            literal: String::from("unknown"),
            token_type: TokenType::Illegal,
        }
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.token_type == other.token_type && self.literal == other.literal
    }
}

impl Token {
    fn eq_value(&self, other: &Self) -> bool {
        self.token_type == other.token_type
            && self.literal == other.literal
            && self.column_num == other.column_num
            && self.line_num == other.line_num
    }

    pub fn new(token_type: TokenType, literal: &str, line: usize, col: usize) -> Self {
        Token {
            line_num: line,
            column_num: col,
            token_type,
            literal: literal.to_string(),
        }
    }
}

pub struct Tokenizer {
    input: String,
    position: usize,
    current_line: usize,
    current_column: usize,
    is_eof: bool,
    tokens: Vec<Token>,
}

impl Default for Tokenizer {
    fn default() -> Tokenizer {
        Tokenizer {
            current_column: 0,
            current_line: 0,
            is_eof: false,
            position: 0,
            tokens: vec![],
            input: String::new(),
        }
    }
}

impl Tokenizer {
    pub fn new(input: &str) -> Self {
        Tokenizer {
            input: input.to_string(),
            ..Default::default()
        }
    }
    pub fn get_tokens(&mut self) -> Vec<Token> {
        if self.is_eof {
            return self.tokens.to_vec();
        }

        self.tokenize();
        return self.tokens.to_vec();
    }

    fn tokenize(&mut self) -> () {
        if self.is_eof {
            return;
        }

        let token = self.next_token();
        self.tokens.push(token);
        self.tokenize();
    }

    pub fn eof(&self) -> bool {
        self.is_eof
    }

    fn consume_char(&mut self) -> char {
        self.position += 1;
        self.current_column += 1;
        match self.input.chars().nth(self.position - 1) {
            Some(c) => {
                if c == '\n' {
                    self.current_line += 1;
                    self.current_column = 0;
                }
                c
            }
            None => '\0',
        }
    }

    fn peek_char(&self) -> char {
        match self.input.chars().nth(self.position) {
            Some(c) => c,
            None => '\0',
        }
    }

    fn skip_whitespace(&mut self) -> () {
        match self.peek_char() {
            ' ' | '\n' | '\t' | '\r' => {
                self.consume_char();
                self.skip_whitespace();
            }
            _ => return,
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        // Position of current token
        let line = self.current_line;
        let col = self.current_column;

        let c = self.consume_char();
        match c {
            '=' => {
                if self.peek_char() == '=' {
                    self.consume_char();
                    return Token::new(TokenType::Eq, "==", line, col);
                }
                Token::new(TokenType::Assign, &c.to_string(), line, col)
            }
            '+' => Token::new(TokenType::Plus, &c.to_string(), line, col),
            '-' => Token::new(TokenType::Minus, &c.to_string(), line, col),
            '*' => Token::new(TokenType::Asterisk, &c.to_string(), line, col),
            '/' => Token::new(TokenType::Slash, &c.to_string(), line, col),
            '!' => {
                if self.peek_char() == '=' {
                    self.consume_char();
                    return Token::new(TokenType::Neq, "!=", line, col);
                }
                Token::new(TokenType::Bang, &c.to_string(), line, col)
            }
            '<' => Token::new(TokenType::Lt, &c.to_string(), line, col),
            '>' => Token::new(TokenType::Gt, &c.to_string(), line, col),
            ',' => Token::new(TokenType::Comma, &c.to_string(), line, col),
            ';' => Token::new(TokenType::Semicolon, &c.to_string(), line, col),
            '(' => Token::new(TokenType::LParen, &c.to_string(), line, col),
            ')' => Token::new(TokenType::RParen, &c.to_string(), line, col),
            '{' => Token::new(TokenType::LBrace, &c.to_string(), line, col),
            '}' => Token::new(TokenType::RBrace, &c.to_string(), line, col),
            '\0' => {
                self.is_eof = true;
                Token::new(TokenType::EOF, &c.to_string(), line, col)
            }
            // Integers
            c if char_is_num(&c) => {
                let mut literal: Vec<char> = vec![c];

                while char_is_num(&self.peek_char()) {
                    literal.push(self.peek_char());
                    self.consume_char();
                }

                let literal_str = literal.into_iter().collect::<String>();
                Token::new(TokenType::Int, &literal_str, line, col)
            }
            // Keywords and identifiers
            c if char_is_alpha(&c) || c == '_' => {
                let mut literal: Vec<char> = vec![c];

                while char_is_alpha(&self.peek_char()) || self.peek_char() == '_' {
                    literal.push(self.peek_char());
                    self.consume_char();
                }

                let literal_str = literal.into_iter().collect::<String>();

                match match_keyword(&literal_str, line, col) {
                    Some(keyword) => keyword,
                    None => Token::new(TokenType::Ident, &literal_str, line, col),
                }
            }
            c => Token {
                token_type: TokenType::Illegal,
                literal: c.to_string(),
                line_num: line,
                column_num: col,
            },
        }
    }
}

fn match_keyword(literal: &str, line: usize, col: usize) -> Option<Token> {
    match literal {
        "let" => Some(Token::new(TokenType::Let, literal, line, col)),
        "fn" => Some(Token::new(TokenType::Fn, literal, line, col)),
        "return" => Some(Token::new(TokenType::Return, literal, line, col)),
        "if" => Some(Token::new(TokenType::If, literal, line, col)),
        "else" => Some(Token::new(TokenType::Else, literal, line, col)),
        "true" => Some(Token::new(TokenType::True, literal, line, col)),
        "false" => Some(Token::new(TokenType::False, literal, line, col)),
        _ => None,
    }
}

fn char_is_alpha(c: &char) -> bool {
    'a' <= *c && *c <= 'z' || 'A' <= *c && *c <= 'Z'
}

fn char_is_num(c: &char) -> bool {
    '0' <= *c && *c <= '9'
}

pub fn build_tokenizer(input: String) -> Tokenizer {
    Tokenizer {
        input,
        ..Default::default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input1: String = String::from("(){}=+,;-*/!<>");
        let mut tokenizer = build_tokenizer(input1);
        let expected_tokens1: Vec<Token> = vec![
            Token::new(TokenType::LParen, "(", 0, 0),
            Token::new(TokenType::RParen, ")", 0, 0),
            Token::new(TokenType::LBrace, "{", 0, 0),
            Token::new(TokenType::RBrace, "}", 0, 0),
            Token::new(TokenType::Assign, "=", 0, 0),
            Token::new(TokenType::Plus, "+", 0, 0),
            Token::new(TokenType::Comma, ",", 0, 0),
            Token::new(TokenType::Semicolon, ";", 0, 0),
            Token::new(TokenType::Minus, "-", 0, 0),
            Token::new(TokenType::Asterisk, "*", 0, 0),
            Token::new(TokenType::Slash, "/", 0, 0),
            Token::new(TokenType::Bang, "!", 0, 0),
            Token::new(TokenType::Lt, "<", 0, 0),
            Token::new(TokenType::Gt, ">", 0, 0),
            Token::new(TokenType::EOF, "\0", 0, 0),
        ];

        for expected in expected_tokens1 {
            let actual = tokenizer.next_token();
            assert_eq!(actual, expected)
        }

        let input2: String = String::from("foo bar foo_bar 123  093  let  fn return end");
        tokenizer = build_tokenizer(input2);
        let expected_tokens2: Vec<Token> = vec![
            Token::new(TokenType::Ident, "foo", 0, 0),
            Token::new(TokenType::Ident, "bar", 0, 0),
            Token::new(TokenType::Ident, "foo_bar", 0, 0),
            Token::new(TokenType::Int, "123", 0, 0),
            Token::new(TokenType::Int, "093", 0, 0),
            Token::new(TokenType::Let, "let", 0, 0),
            Token::new(TokenType::Fn, "fn", 0, 0),
            Token::new(TokenType::Return, "return", 0, 0),
            Token::new(TokenType::Ident, "end", 0, 0),
            Token::new(TokenType::EOF, "\0", 0, 0),
        ];

        for expected in expected_tokens2 {
            let actual = tokenizer.next_token();
            assert_eq!(actual, expected)
        }

        let input3: String = String::from("fn let return\t if true else false   fn");
        tokenizer = build_tokenizer(input3);
        let expected_tokens3: Vec<Token> = vec![
            Token::new(TokenType::Fn, "fn", 0, 0),
            Token::new(TokenType::Let, "let", 0, 0),
            Token::new(TokenType::Return, "return", 0, 0),
            Token::new(TokenType::If, "if", 0, 0),
            Token::new(TokenType::True, "true", 0, 0),
            Token::new(TokenType::Else, "else", 0, 0),
            Token::new(TokenType::False, "false", 0, 0),
            Token::new(TokenType::Fn, "fn", 0, 0),
            Token::new(TokenType::EOF, "\0", 0, 0),
        ];

        for expected in expected_tokens3 {
            let actual = tokenizer.next_token();
            assert_eq!(actual, expected)
        }
    }
    #[test]
    fn test_eq_neq() {
        let input = "let ten = 10 != 8;
                     let five = 5 == 10;";

        let mut tokenizer = build_tokenizer(input.to_string());
        let expected_tokens: Vec<Token> = vec![
            Token::new(TokenType::Let, "let", 0, 0),
            Token::new(TokenType::Ident, "ten", 0, 0),
            Token::new(TokenType::Assign, "=", 0, 0),
            Token::new(TokenType::Int, "10", 0, 0),
            Token::new(TokenType::Neq, "!=", 0, 0),
            Token::new(TokenType::Int, "8", 0, 0),
            Token::new(TokenType::Semicolon, ";", 0, 0),
            Token::new(TokenType::Let, "let", 0, 0),
            Token::new(TokenType::Ident, "five", 0, 0),
            Token::new(TokenType::Assign, "=", 0, 0),
            Token::new(TokenType::Int, "5", 0, 0),
            Token::new(TokenType::Eq, "==", 0, 0),
            Token::new(TokenType::Int, "10", 0, 0),
            Token::new(TokenType::Semicolon, ";", 0, 0),
        ];

        for expected in expected_tokens {
            let actual = tokenizer.next_token();
            assert_eq!(actual, expected)
        }
    }

    #[test]
    fn test_token_position() {
        let input = "let ten = 10 != 8;  \nlet five = 5 == 10;  ";

        let mut tokenizer = build_tokenizer(input.to_string());
        let expected_tokens: Vec<Token> = vec![
            Token::new(TokenType::Let, "let", 0, 0),
            Token::new(TokenType::Ident, "ten", 0, 4),
            Token::new(TokenType::Assign, "=", 0, 8),
            Token::new(TokenType::Int, "10", 0, 10),
            Token::new(TokenType::Neq, "!=", 0, 13),
            Token::new(TokenType::Int, "8", 0, 16),
            Token::new(TokenType::Semicolon, ";", 0, 17),
            Token::new(TokenType::Let, "let", 1, 0),
            Token::new(TokenType::Ident, "five", 1, 4),
            Token::new(TokenType::Assign, "=", 1, 9),
            Token::new(TokenType::Int, "5", 1, 11),
            Token::new(TokenType::Eq, "==", 1, 13),
            Token::new(TokenType::Int, "10", 1, 16),
            Token::new(TokenType::Semicolon, ";", 1, 18),
        ];

        for expected in expected_tokens {
            let actual = tokenizer.next_token();
            assert!(expected.eq_value(&actual))
        }
    }

    #[test]
    fn test_full_code() {
        let input = "let five = 5;
                    let ten = 10;
                    let add = fn(x, y) {
                    x + y;
                    };
                    let result = add(five, ten);
                    !-/*5;
                    5 < 10 > 5;
                    if (5 < 10) {
                    return true;
                    } else {
                    return false;
                    }";

        let mut tokenizer = build_tokenizer(input.to_string());
        let expected_tokens: Vec<Token> = vec![
            Token::new(TokenType::Let, "let", 0, 0),
            Token::new(TokenType::Ident, "five", 0, 0),
            Token::new(TokenType::Assign, "=", 0, 0),
            Token::new(TokenType::Int, "5", 0, 0),
            Token::new(TokenType::Semicolon, ";", 0, 0),
            Token::new(TokenType::Let, "let", 0, 0),
            Token::new(TokenType::Ident, "ten", 0, 0),
            Token::new(TokenType::Assign, "=", 0, 0),
            Token::new(TokenType::Int, "10", 0, 0),
            Token::new(TokenType::Semicolon, ";", 0, 0),
            Token::new(TokenType::Let, "let", 0, 0),
            Token::new(TokenType::Ident, "add", 0, 0),
            Token::new(TokenType::Assign, "=", 0, 0),
            Token::new(TokenType::Fn, "fn", 0, 0),
            Token::new(TokenType::LParen, "(", 0, 0),
            Token::new(TokenType::Ident, "x", 0, 0),
            Token::new(TokenType::Comma, ",", 0, 0),
            Token::new(TokenType::Ident, "y", 0, 0),
            Token::new(TokenType::RParen, ")", 0, 0),
            Token::new(TokenType::LBrace, "{", 0, 0),
            Token::new(TokenType::Ident, "x", 0, 0),
            Token::new(TokenType::Plus, "+", 0, 0),
            Token::new(TokenType::Ident, "y", 0, 0),
            Token::new(TokenType::Semicolon, ";", 0, 0),
            Token::new(TokenType::RBrace, "}", 0, 0),
            Token::new(TokenType::Semicolon, ";", 0, 0),
            Token::new(TokenType::Let, "let", 0, 0),
            Token::new(TokenType::Ident, "result", 0, 0),
            Token::new(TokenType::Assign, "=", 0, 0),
            Token::new(TokenType::Ident, "add", 0, 0),
            Token::new(TokenType::LParen, "(", 0, 0),
            Token::new(TokenType::Ident, "five", 0, 0),
            Token::new(TokenType::Comma, ",", 0, 0),
            Token::new(TokenType::Ident, "ten", 0, 0),
            Token::new(TokenType::RParen, ")", 0, 0),
            Token::new(TokenType::Semicolon, ";", 0, 0),
            Token::new(TokenType::Bang, "!", 0, 0),
            Token::new(TokenType::Minus, "-", 0, 0),
            Token::new(TokenType::Slash, "/", 0, 0),
            Token::new(TokenType::Asterisk, "*", 0, 0),
            Token::new(TokenType::Int, "5", 0, 0),
            Token::new(TokenType::Semicolon, ";", 0, 0),
            Token::new(TokenType::Int, "5", 0, 0),
            Token::new(TokenType::Lt, "<", 0, 0),
            Token::new(TokenType::Int, "10", 0, 0),
            Token::new(TokenType::Gt, ">", 0, 0),
            Token::new(TokenType::Int, "5", 0, 0),
            Token::new(TokenType::Semicolon, ";", 0, 0),
            Token::new(TokenType::If, "if", 0, 0),
            Token::new(TokenType::LParen, "(", 0, 0),
            Token::new(TokenType::Int, "5", 0, 0),
            Token::new(TokenType::Lt, "<", 0, 0),
            Token::new(TokenType::Int, "10", 0, 0),
            Token::new(TokenType::RParen, ")", 0, 0),
            Token::new(TokenType::LBrace, "{", 0, 0),
            Token::new(TokenType::Return, "return", 0, 0),
            Token::new(TokenType::True, "true", 0, 0),
            Token::new(TokenType::Semicolon, ";", 0, 0),
            Token::new(TokenType::RBrace, "}", 0, 0),
            Token::new(TokenType::Else, "else", 0, 0),
            Token::new(TokenType::LBrace, "{", 0, 0),
            Token::new(TokenType::Return, "return", 0, 0),
            Token::new(TokenType::False, "false", 0, 0),
            Token::new(TokenType::Semicolon, ";", 0, 0),
            Token::new(TokenType::RBrace, "}", 0, 0),
        ];

        for expected in expected_tokens {
            let actual = tokenizer.next_token();
            assert_eq!(actual, expected);
        }
    }
}
