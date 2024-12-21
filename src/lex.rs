use crate::StopInteger;
use std::iter::Peekable;

static KEYWORDS: phf::Map<&'static str, Token> = phf::phf_map! {
    "let" => Token::Let,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    // Keywords
    Let, // let

    // Literals
    Ident(String),    // x
    Int(StopInteger), // 123

    // Operators
    Assign, // =

    // Punctuation
    Semicolon, // ;

    // Special
    Error(String),
}

impl Token {
    fn token_from_symbol(c: char) -> Token {
        match c {
            '=' => Token::Assign,
            ';' => Token::Semicolon,
            _ => Token::Error(format!("unexpected character: {}", c)),
        }
    }
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

    fn get_number(&mut self) -> StopInteger {
        let mut number = String::from(self.read().unwrap());
        while let Some(&c) = self.peek() {
            if is_number_char(c) {
                number.push(c);
                self.read();
            } else {
                break;
            }
        }
        number.parse::<StopInteger>().unwrap()
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
            self.read();
            Some(Token::token_from_symbol(next))
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
                (Int(5), 9),
                (Semicolon, 10),
            ],
            &mut lexer,
        );

        assert_eq!(lexer.next(), None);
    }

    fn test_tokens(expected: Vec<(Token, i32)>, lexer: &mut Lexer<impl Iterator<Item = char>>) {
        for (expected_token, expected_pos) in expected {
            let token = lexer.next();
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
