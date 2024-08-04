use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TextPos {
    pub pos: usize,
    pub line: usize,
    pub col: usize,
}

impl Display for TextPos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

impl TextPos {
    pub fn add_length(&self, length: usize) -> Self {
        Self {
            pos: self.pos + length,
            line: self.line,
            col: self.col + length,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind {
    LParen,
    RParen,
    LCurly,
    RCurly,
    Arrow,

    And,
    Or,
    Eq,
    Add,
    Sub,
    Mul,
    Div,

    Comma,
    SemiColon,
    Colon,

    Return,
    Fn,
    Var,

    Identifier,
    Int,
    Unknown
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub content: &'a str,
    pub start: TextPos,
    pub end: TextPos,
}


impl<'a> Default for TextPos {
    fn default() -> Self {
        Self { pos: 0, line: 1, col: 1 }
    }
}

impl<'a> Default for Token<'a> {
    fn default() -> Self {
        Self {
            kind: TokenKind::Unknown,
            content: "",
            start: TextPos::default(),
            end: TextPos::default(),
        }
    }
}

impl<'a> Token<'a> {
    pub fn from_str(content: &'a str, kind: TokenKind, start: TextPos) -> Self {
        Self {
            kind,
            content,
            start,
            end: start.add_length(content.len()),
        }
    }
}

fn match_chars(c: u8, start: TextPos) -> Option<Token<'static>> {
    match c {
        b'(' => Some(Token::from_str("(", TokenKind::LParen, start)),
        b')' => Some(Token::from_str(")", TokenKind::RParen, start)),
        b'{' => Some(Token::from_str("{", TokenKind::LCurly, start)),
        b'}' => Some(Token::from_str("}", TokenKind::RCurly, start)),


        b'=' => Some(Token::from_str("=", TokenKind::Eq, start)),
        b'+' => Some(Token::from_str("+", TokenKind::Add, start)),
        b'-' => Some(Token::from_str("-", TokenKind::Sub, start)),
        b'*' => Some(Token::from_str("*", TokenKind::Mul, start)),
        b'/' => Some(Token::from_str("/", TokenKind::Div, start)),

        b',' => Some(Token::from_str(",", TokenKind::Comma, start)),
        b';' => Some(Token::from_str(";", TokenKind::SemiColon, start)),
        b':' => Some(Token::from_str(":", TokenKind::Colon, start)),
        _ => None,
    }
}

fn match_identifier(input: &str, start: TextPos) -> Option<Token> {
    if !input.as_bytes()[0].is_ascii_alphabetic() && input.as_bytes()[0] != b'_' {
        None
    } else {
        let length = input.as_bytes().iter().take_while(|c| c.is_ascii_alphanumeric() || **c == b'_').count();
        Some(Token::from_str(&input[..length], TokenKind::Identifier, start))
    }
}


fn match_str<'a>(input: &str, content: &'a str, kind: TokenKind, start: TextPos) -> Option<Token<'a>> {
    if input.starts_with(content) {
        Some(Token::from_str(content, kind, start))
    } else {
        None
    }
}

fn match_keyword<'a>(input: &str, content: &'a str, kind: TokenKind, start: TextPos) -> Option<Token<'a>> {
    return match_str(input, content, kind, start).filter(|_| input.as_bytes()[content.len()].is_ascii_whitespace());
}

fn match_int(input: &str, start: TextPos) -> Option<Token> {
    if !input.as_bytes()[0].is_ascii_digit() {
        None
    } else {
        let length = input.as_bytes().iter().take_while(|c| c.is_ascii_digit() || **c == b'_').count();
        Some(Token::from_str(&input[..length], TokenKind::Int, start))
    }
}

macro_rules! return_match {
    ($current: expr, $match_expr: expr) => {
        if let Some(token) = $match_expr {
            $current = token;
            return &$current;
        }
    };
}

pub struct Lexer<'a> {
    input: &'a str,
    pub current: Token<'a>,
}
impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            current: Token::default(),
        }
    }

    pub fn consume(&mut self) -> &Token {
        if self.current.end.pos >= self.input.len() {
            self.current = Token::default();
            return &self.current;
        }

        while self.input.as_bytes()[self.current.end.pos].is_ascii_whitespace() {
            if self.current.end.pos == self.input.len() - 1 {
                self.current.end.pos += 1;
                self.current = Token::default();
                return &self.current;
            }

            if self.input.as_bytes()[self.current.end.pos] == b'\n' {
                self.current.end.line += 1;
                self.current.end.col = 1;
            } else {
                self.current.end.col += 1;
            }

            self.current.end.pos += 1;
        }

        let trimmed_input = &self.input[self.current.end.pos..];

        return_match!(self.current, match_str(trimmed_input, "->", TokenKind::Arrow, self.current.end));

        return_match!(self.current, match_chars(trimmed_input.as_bytes()[0], self.current.end));


        return_match!(self.current, match_keyword(trimmed_input, "return", TokenKind::Return, self.current.end));
        return_match!(self.current, match_keyword(trimmed_input, "fn", TokenKind::Fn, self.current.end));
        return_match!(self.current, match_keyword(trimmed_input, "var", TokenKind::Var, self.current.end));


        return_match!(self.current, match_identifier(trimmed_input, self.current.end));
        return_match!(self.current, match_int(trimmed_input, self.current.end));

        self.current = Token::default();
        return &self.current;
    }

    pub fn consume_if(&mut self, kind: TokenKind) -> bool {
        if self.current.kind == kind {
            self.consume();
            true
        } else {
            false
        }
    }
}

mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let mut lexer = Lexer::new("
(
    241
    return allo
)
");
        assert_eq!(*lexer.consume(), Token::from_str("(", TokenKind::LParen, TextPos { pos: 1, line: 2, col: 1 }));
        assert_eq!(*lexer.consume(), Token::from_str("241", TokenKind::Int, TextPos { pos: 7, line: 3, col: 5 }));
        assert_eq!(*lexer.consume(), Token::from_str("return", TokenKind::Return, TextPos { pos: 15, line: 4, col: 5 }));
        assert_eq!(*lexer.consume(), Token::from_str("allo", TokenKind::Identifier, TextPos { pos: 22, line: 4, col: 12 }));
        assert_eq!(*lexer.consume(), Token::from_str(")", TokenKind::RParen, TextPos { pos: 27, line: 5, col: 1 }));
    }
}