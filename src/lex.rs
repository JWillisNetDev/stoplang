use crate::StopIdentifier;
use std::iter::Peekable;

static KEYWORDS: phf::Map<&'static str, Token> = phf::phf_map! {
    "let" => Token::Let,
    "if" => Token::If,
    "else" => Token::Else,
    "fn" => Token::Fn,
    "return" => Token::Return,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    // Keywords
    Let,
    If,
    Else,
    Fn,
    Return,

    // Literals
    Ident(StopIdentifier), // x
    Int(StopIdentifier),   // 123

    // Operators
    Assign,        // =
    Eq,            // ==
    Neq,           // !=
    Plus,          // +
    Minus,         // -
    Bang,          // !
    Splat,         // *
    Slash,         // /
    LessThan,      // <
    LessThanEq,    // <=
    GreaterThan,   // >
    GreaterThanEq, // >=

    // Punctuation
    Semicolon,  // ;
    Comma,      // ,
    OpenParen,  // (
    CloseParen, // )
    OpenBrace,  // {
    CloseBrace, // }

    // Special
    Error(String),
}

pub struct Lexer<T: Iterator<Item = char>> {
    inner: Peekable<T>,
    pos: i32,
}

impl<T: Iterator<Item = char>> Lexer<T> {
    pub fn new(inner: T) -> Self {
        Lexer {
            inner: inner.peekable(),
            pos: 0,
        }
    }

    fn read(&mut self) -> Option<char> {
        let c = self.inner.next();
        if c.is_some() {
            self.pos += 1;
        }
        c
    }

    fn peek(&mut self) -> Option<&char> {
        self.inner.peek()
    }

    fn eat_whitespace(&mut self) {
        while let Some(&c) = self.peek() {
            if c.is_whitespace() {
                self.read();
            } else {
                break;
            }
        }
    }

    fn get_identifier(&mut self) -> String {
        let mut identifier = String::from(self.read().unwrap());
        while let Some(&c) = self.peek() {
            if is_identifier_char(c) {
                identifier.push(c);
                self.read();
            } else {
                break;
            }
        }
        return identifier;
    }

    fn get_number(&mut self) -> String {
        let mut number = String::from(self.read().unwrap());
        while let Some(&c) = self.peek() {
            if is_number_char(c) {
                number.push(c);
                self.read();
            } else {
                break;
            }
        }
        number
    }

    fn get_symbol(&mut self) -> Token {
        match self.read() {
            Some('=') => {
                let c = self.peek();
                if c == Some(&'=') {
                    self.read();
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            Some('+') => Token::Plus,
            Some('-') => Token::Minus,
            Some('!') => {
                let c = self.peek();
                if c == Some(&'=') {
                    self.read();
                    Token::Neq
                } else {
                    Token::Bang
                }
            }
            Some('*') => Token::Splat,
            Some('/') => Token::Slash,
            Some('<') => {
                let c = self.peek();
                if let Some('=') = c {
                    self.read();
                    Token::LessThanEq
                } else {
                    Token::LessThan
                }
            }
            Some('>') => {
                let c = self.peek();
                if let Some('=') = c {
                    self.read();
                    Token::GreaterThanEq
                } else {
                    Token::GreaterThan
                }
            }
            Some(';') => Token::Semicolon,
            Some(',') => Token::Comma,
            Some('(') => Token::OpenParen,
            Some(')') => Token::CloseParen,
            Some('{') => Token::OpenBrace,
            Some('}') => Token::CloseBrace,
            Some(c) => Token::Error(format!(
                "unexpected character: {} at position {}",
                c, self.pos
            )),
            None => Token::Error(format!("unexpected end of input at position {}", self.pos)),
        }
    }
}

fn is_identifier_char(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

fn is_number_char(c: char) -> bool {
    c >= '0' && c <= '9'
}

impl<T: Iterator<Item = char>> Iterator for Lexer<T> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.eat_whitespace();

        let next = match self.peek() {
            Some(&c) => c,
            None => return None,
        };

        if is_identifier_char(next) {
            let identifier = self.get_identifier();
            match KEYWORDS.get(identifier.as_str()) {
                Some(token) => Some(token.clone()),
                None => Some(Token::Ident(identifier)),
            }
        } else if is_number_char(next) {
            let number = self.get_number();
            Some(Token::Int(number))
        } else {
            Some(self.get_symbol())
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn it_wraps_iterator() {
        let input = "abc".chars();
        let lexer = Lexer::new(input);

        assert_eq!(lexer.inner.collect::<String>(), "abc");
    }

    #[test]
    fn it_reads_and_peeks() {
        let mut lexer = Lexer::new("abc".chars());

        let c = lexer.peek();
        assert_eq!(c, Some(&'a'));
        assert_eq!(lexer.pos, 0);

        let c = lexer.read();
        assert_eq!(c, Some('a'));
        assert_eq!(lexer.pos, 1);

        let c = lexer.peek();
        assert_eq!(c, Some(&'b'));
        assert_eq!(lexer.pos, 1);

        let c = lexer.read();
        assert_eq!(c, Some('b'));
        assert_eq!(lexer.pos, 2);

        let c = lexer.peek();
        assert_eq!(c, Some(&'c'));
        assert_eq!(lexer.pos, 2);

        let c = lexer.read();
        assert_eq!(c, Some('c'));
        assert_eq!(lexer.pos, 3);

        let c = lexer.peek();
        assert_eq!(c, None);
        assert_eq!(lexer.pos, 3);

        let c = lexer.read();
        assert_eq!(c, None);
        assert_eq!(lexer.pos, 3);
    }

    #[test]
    fn it_eats_whitespace() {
        let mut lexer = Lexer::new("   a\t\t\tb\n\n\nc".chars());

        lexer.eat_whitespace();
        assert_eq!(lexer.pos, 3);

        let c = lexer.read();
        assert_eq!(c, Some('a'));

        lexer.eat_whitespace();
        assert_eq!(lexer.pos, 7);

        let c = lexer.read();
        assert_eq!(c, Some('b'));

        lexer.eat_whitespace();
        assert_eq!(lexer.pos, 11);

        let c = lexer.read();
        assert_eq!(c, Some('c'));

        lexer.eat_whitespace();
        assert_eq!(lexer.pos, 12);
    }

    #[test]
    fn it_parses_tokens() {
        let input = r#"let x = 5;"#;
        let mut lexer = Lexer::new(input.chars());

        use Token::*;
        test_tokens(
            vec![
                (Let, 3),
                (Ident("x".to_string()), 5),
                (Assign, 7),
                (Int("5".to_string()), 9),
                (Semicolon, 10),
            ],
            &mut lexer,
        );

        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn it_parses_token_if_else() {
        let input = r#"let x = if a == b { 1 } else { 2 };"#;
        let mut lexer = Lexer::new(input.chars());

        use Token::*;
        test_tokens(
            vec![
                (Let, 3),
                (Ident("x".to_string()), 5),
                (Assign, 7),
                (If, 10),
                (Ident("a".to_string()), 12),
                (Eq, 15),
                (Ident("b".to_string()), 17),
                (OpenBrace, 19),
                (Int("1".to_string()), 21),
                (CloseBrace, 23),
                (Else, 28),
                (OpenBrace, 30),
                (Int("2".to_string()), 32),
                (CloseBrace, 34),
                (Semicolon, 35),
            ],
            &mut lexer,
        );

        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn it_parses_token_fn() {
        let input = r#"fn add(a, b) { a + b }"#;
        let mut lexer = Lexer::new(input.chars());

        use Token::*;
        test_tokens(
            vec![
                (Fn, 2),
                (Ident("add".to_string()), 6),
                (OpenParen, 7),
                (Ident("a".to_string()), 8),
                (Comma, 9),
                (Ident("b".to_string()), 11),
                (CloseParen, 12),
                (OpenBrace, 14),
                (Ident("a".to_string()), 16),
                (Plus, 18),
                (Ident("b".to_string()), 20),
                (CloseBrace, 22),
            ],
            &mut lexer,
        );

        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn it_parses_identifiers() {
        let input = r#"abc _def GHI"#;
        let mut lexer = Lexer::new(input.chars());

        use Token::*;
        test_tokens(
            vec![
                (Ident("abc".to_string()), 3),
                (Ident("_def".to_string()), 8),
                (Ident("GHI".to_string()), 12),
            ],
            &mut lexer,
        );

        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn it_parses_numbers() {
        let input = r#"123 456 789"#;
        let mut lexer = Lexer::new(input.chars());

        use Token::*;
        test_tokens(
            vec![
                (Int("123".to_string()), 3),
                (Int("456".to_string()), 7),
                (Int("789".to_string()), 11),
            ],
            &mut lexer,
        );

        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn it_parses_keywords() {
        let input = r#"let if else fn return"#;
        let mut lexer = Lexer::new(input.chars());

        use Token::*;
        test_tokens(
            vec![(Let, 3), (If, 6), (Else, 11), (Fn, 14), (Return, 21)],
            &mut lexer,
        );

        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn it_parses_symbols() {
        let input = r#"= == + - ! * / ; , ( ) { } != > >= < <="#;
        let mut lexer = Lexer::new(input.chars());

        use Token::*;
        test_tokens(
            vec![
                (Assign, 1),
                (Eq, 4),
                (Plus, 6),
                (Minus, 8),
                (Bang, 10),
                (Splat, 12),
                (Slash, 14),
                (Semicolon, 16),
                (Comma, 18),
                (OpenParen, 20),
                (CloseParen, 22),
                (OpenBrace, 24),
                (CloseBrace, 26),
                (Neq, 29),
                (LessThan, 31),
                (LessThanEq, 34),
                (GreaterThan, 36),
                (GreaterThanEq, 39),
            ],
            &mut lexer,
        );

        assert_eq!(lexer.next(), None);
    }

    // TODO: This would be better as a macro.
    fn test_tokens(expected: Vec<(Token, i32)>, lexer: &mut Lexer<impl Iterator<Item = char>>) {
        for (expected_token, expected_pos) in expected {
            let token = lexer.next();
            if let Some(Token::Error(err)) = token {
                panic!(
                    "Lexing encountered the following error: `{}`, expected: {:?}",
                    err, expected_token
                );
            }
            assert_eq!(
                token,
                Some(expected_token.clone()),
                "expected token {:?}, got {:?}",
                expected_token,
                token
            );
            assert_eq!(
                lexer.pos, expected_pos,
                "expected pos {}, got {}",
                expected_pos, lexer.pos
            );
        }
    }
}
