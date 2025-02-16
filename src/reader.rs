use core::str;
use std::num::ParseIntError;

use pcre2::bytes::RegexBuilder;

use crate::Value;

type Result<T> = std::result::Result<T, Error>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("unexpected eof")]
    UnexpectedEOF,
    #[error(transparent)]
    PCRE(#[from] pcre2::Error),
    #[error(transparent)]
    NumberParseError(#[from] ParseIntError),
}

pub struct Reader<'a> {
    tokens: Vec<&'a [u8]>,
    position: usize,
}

impl Reader<'_> {
    pub fn new(tokens: Vec<&[u8]>) -> Reader<'_> {
        Reader {
            tokens,
            position: 0,
        }
    }
}

impl Reader<'_> {
    pub fn next(&mut self) -> Option<&'_ [u8]> {
        if self.position + 1 >= self.tokens.len() {
            return None;
        }
        self.position += 1;
        Some(self.peek())
    }

    pub fn peek(&self) -> &'_ [u8] {
        self.tokens[self.position]
    }
}

impl Reader<'_> {
    pub fn read_form(&mut self) -> Result<Value> {
        match self.peek() {
            b"(" => self.read_list(),
            _ => self.read_atom(),
        }
    }

    pub fn read_list(&mut self) -> Result<Value> {
        let mut list = Vec::new();
        loop {
            let Some(token) = self.next() else {
                return Err(Error::UnexpectedEOF)
            };
            if token == b")" {
                return Ok(Value::List(list));
            }
            list.push(self.read_form()?);
        }
    }

    pub fn read_atom(&self) -> Result<Value> {
        let token = self.peek();
        let str = str::from_utf8(self.peek()).unwrap();
        
        if token.is_empty() {
            return Err(Error::UnexpectedEOF);
        }
        
        if token[0].is_ascii_digit() || token[0] == b'-' {
            return match str.parse() {
                Ok(num) => Ok(Value::Number(num)),
                Err(e) => return Err(Error::NumberParseError(e)),
            };
        }

        return Ok(Value::Symbol(str.to_string()))
    }
}

pub fn tokenize(str: &str) -> Result<Vec<&[u8]>> {
    let regex = RegexBuilder::new().build(r#"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"#)?;

    let mut vec = vec![];
    for found in regex.find_iter(str.as_bytes()) {
        let found = found?;
        vec.push(found.as_bytes().trim_ascii());
    }

    Ok(vec)
}

pub fn read_str(str: &str) -> Result<Value> {
    let mut reader = Reader::new(tokenize(str)?);
    Ok(reader.read_form()?)
}

#[cfg(test)]
mod test {
    use core::str;

    use crate::Value;

    use super::{read_str, tokenize};

    #[test]
    fn test_tokenize() {
        let tokens = tokenize("(+ 1 (+ 1 3))").expect("tokenize regex should not fail");
        let mut tokens_str = vec![];

        for token in tokens {
            let str = str::from_utf8(token).expect("token should not contain invalid utf8");
            tokens_str.push(str)
        }

        assert_eq!(tokens_str, vec!["(", "+", "1", "(", "+", "1", "3", ")", ")"]);
    }

    #[test]
    fn test_read_str() {
        let value = read_str("  ( +  1  (+ 1 3) ) ").expect("read_str should not fail with the given input");
        assert_eq!(value, Value::List(vec![
            Value::Symbol("+".into()),
            Value::Number(1),
            Value::List(vec![
                Value::Symbol("+".into()),
                Value::Number(1),
                Value::Number(3),
            ]),
        ]));

        let value = read_str("  123  ").expect("read_str should not fail with the given input");
        assert_eq!(value, Value::Number(123));

        let value = read_str("  (123  456  789) ").expect("read_str should not fail with the given input");
        assert_eq!(value, Value::List(vec![
            Value::Number(123),
            Value::Number(456),
            Value::Number(789),
        ]));

        let value = read_str("  abc  ").expect("read_str should not fail with the given input");
        assert_eq!(value, Value::Symbol("abc".into()));

        let value = read_str("(abc)").expect("read_str should not fail with the given input");
        assert_eq!(value, Value::List(vec![
            Value::Symbol("abc".into()),
        ]));
    }
}