use core::str;
use std::{num::ParseIntError, str::Utf8Error};
use lazy_static::lazy_static;

use pcre2::bytes::Regex;

use crate::Value::{self, List, Int, Symbol, Nil, Bool, Str, Keyword};

type Result<T> = std::result::Result<T, Error>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("unmatched left parenthesis")]
    UnmatchedLeftParenthesis,
    #[error("unmatched right parenthesis")]
    UnmatchedRightParenthesis,
    #[error(transparent)]
    PCRE(#[from] pcre2::Error),
    #[error(transparent)]
    NumberParseError(#[from] ParseIntError),
    #[error(transparent)]
    InvalidUtf8(#[from] Utf8Error),
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
        let val = match self.peek() {
            b"(" => self.read_seq(),
            _ => self.read_atom(),
        }?;
        Ok(val)
    }

    pub fn read_seq(&mut self) -> Result<Value> {
        let mut list = Vec::new();
        loop {
            let Some(token) = self.next() else {
                return Err(Error::UnmatchedLeftParenthesis)
            };
            if token == b")" {
                return Ok(List(list));
            }
            list.push(self.read_form()?);
        }
    }

    pub fn read_atom(&self) -> Result<Value> {
        let token = self.peek();

        lazy_static! {
            static ref INT_RE: Regex = Regex::new(r#"^-?[0-9]+$"#)
                .expect("regex should be valid");
            static ref STR_RE: Regex = Regex::new(r#""(?:\\.|[^\\"])*""#)
                .expect("regex should be valid");
            static ref KEYWORD_RE: Regex = Regex::new(r#"^:[\S]*$"#)
                .expect("regex should be valid");
        }

        match token {
            b"nil" => Ok(Nil),
            b"true" => Ok(Bool(true)),
            b"false" => Ok(Bool(false)),
            token => {
                let str = str::from_utf8(token)?;
                if INT_RE.is_match(token)? {
                    Ok(Int(str.parse()?))
                } else if STR_RE.is_match(token)? {
                    Ok(Str(unescape_str(str)))
                } else if KEYWORD_RE.is_match(token)? {
                    Ok(Keyword(str[1..].to_string()))
                } else {
                    Ok(Symbol(str.to_string()))
                }
            }
        }
    }
}

fn unescape_str(str: &str) -> String {
    str.trim_matches('\"')
        .replace("\\\\", "\\")
        .replace("\\n", "\n")
        .replace("\\\"", "\"")
}

pub fn tokenize(str: &str) -> Result<Vec<&[u8]>> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r#"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"#)
            .expect("regex should be valid");
    }

    let mut vec = vec![];
    for found in RE.find_iter(str.as_bytes()) {
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
        let value = read_str("  ( +  1  (+ \"my \\\\ cool \\\" string\\n\" 3) ) ").expect("read_str should not fail with the given input");
        assert_eq!(value, Value::List(vec![
            Value::Symbol("+".into()),
            Value::Int(1),
            Value::List(vec![
                Value::Symbol("+".into()),
                Value::Str("my \\ cool \" string\n".into()),
                Value::Int(3),
            ]),
        ]));

        let value = read_str("  123  ").expect("read_str should not fail with the given input");
        assert_eq!(value, Value::Int(123));

        let value = read_str("  (true  nil  false) ").expect("read_str should not fail with the given input");
        assert_eq!(value, Value::List(vec![
            Value::Bool(true),
            Value::Nil,
            Value::Bool(false),
        ]));

        let value = read_str("  abc  ").expect("read_str should not fail with the given input");
        assert_eq!(value, Value::Symbol("abc".into()));

        let value = read_str("(:abc)").expect("read_str should not fail with the given input");
        assert_eq!(value, Value::List(vec![
            Value::Keyword("abc".into()),
        ]));
    }
}