use core::str;
use lazy_static::lazy_static;
use std::{collections::HashMap, num::ParseIntError, str::Utf8Error};

use pcre2::bytes::Regex;

use crate::{
    types::MapKey,
    Value::{self, Bool, Int, Keyword, List, Map, Nil, Str, Symbol, Vector},
};

type Result<T> = std::result::Result<T, Error>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("unmatched left parenthesis")]
    UnmatchedLeftParenthesis,
    #[error("unmatched right parenthesis")]
    UnmatchedRightParenthesis,
    #[error("invalid map key type, map keys can only be string type")]
    InvalidMapKeyType,
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
            b"(" => Ok(List(SeqReader::new(self, b")").to_vec()?)),
            b"[" => Ok(Vector(SeqReader::new(self, b"]").to_vec()?)),
            b"{" => Ok(Map(SeqReader::new(self, b"}").to_map()?)),
            _ => self.read_atom(),
        }?;
        Ok(val)
    }

    pub fn read_atom(&self) -> Result<Value> {
        let token = self.peek();

        lazy_static! {
            static ref INT_RE: Regex = Regex::new(r#"^-?[0-9]+$"#).expect("regex should be valid");
            static ref STR_RE: Regex =
                Regex::new(r#""(?:\\.|[^\\"])*""#).expect("regex should be valid");
            static ref KEYWORD_RE: Regex =
                Regex::new(r#"^:[\S]*$"#).expect("regex should be valid");
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

pub struct SeqReader<'a, 'b> {
    reader: &'a mut Reader<'b>,
    end: &'a [u8],
}

impl<'a, 'b> SeqReader<'a, 'b> {
    pub fn new(reader: &'a mut Reader<'b>, end: &'a [u8]) -> Self {
        Self { reader, end }
    }
}

impl SeqReader<'_, '_> {
    pub fn to_vec(self) -> Result<Vec<Value>> {
        let mut vec = vec![];
        for value in self {
            vec.push(value?);
        }
        Ok(vec)
    }

    pub fn to_map(self) -> Result<HashMap<MapKey, Value>> {
        let mut map = HashMap::new();
        let mut key = None;
        for value in self {
            match key.take() {
                Some(key) => {
                    map.insert(key, value?);
                }
                None => match value? {
                    Str(str) => key = Some(MapKey::Str(str)),
                    Keyword(str) => key = Some(MapKey::Keyword(str)),
                    _ => return Err(Error::InvalidMapKeyType),
                },
            }
        }
        Ok(map)
    }
}

impl Iterator for SeqReader<'_, '_> {
    type Item = Result<Value>;

    fn next(&mut self) -> Option<Self::Item> {
        let Some(token) = self.reader.next() else {
            return Some(Err(Error::UnmatchedLeftParenthesis));
        };
        if token == self.end {
            return None;
        }
        Some(self.reader.read_form())
    }
}

pub fn tokenize(str: &str) -> Result<Vec<&[u8]>> {
    lazy_static! {
        static ref RE: Regex =
            Regex::new(r#"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"#)
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

    use crate::{types::MapKey, Value};

    use super::{read_str, tokenize};

    #[test]
    fn tokenizing() {
        let tokens = tokenize("(+ 1 (+ 1 3))").expect("tokenize regex should not fail");
        let mut tokens_str = vec![];

        for token in tokens {
            let str = str::from_utf8(token).expect("token should not contain invalid utf8");
            tokens_str.push(str)
        }

        assert_eq!(
            tokens_str,
            vec!["(", "+", "1", "(", "+", "1", "3", ")", ")"]
        );
    }

    #[test]
    fn reading_str() {
        let value = try_read_str("  ( +  1  (+ \"my \\\\ cool \\\" string\\n\" 3) ) ");
        assert_eq!(
            value,
            Value::List(vec![
                Value::Symbol("+".into()),
                Value::Int(1),
                Value::List(vec![
                    Value::Symbol("+".into()),
                    Value::Str("my \\ cool \" string\n".into()),
                    Value::Int(3),
                ]),
            ])
        );

        let value = try_read_str("  123  ");
        assert_eq!(value, Value::Int(123));

        let value = try_read_str("  [true  nil  false] ");
        assert_eq!(
            value,
            Value::Vector(vec![Value::Bool(true), Value::Nil, Value::Bool(false),])
        );

        let value = try_read_str("  abc  ");
        assert_eq!(value, Value::Symbol("abc".into()));

        let value = try_read_str("(:abc)");
        assert_eq!(value, Value::List(vec![Value::Keyword("abc".into()),]));

        let value = try_read_str("{\"a\" 1 :b 2 \"c\" 3}");
        assert_eq!(
            value,
            Value::Map(
                [
                    (MapKey::Str("a".into()), Value::Int(1)),
                    (MapKey::Keyword("b".into()), Value::Int(2)),
                    (MapKey::Str("c".into()), Value::Int(3)),
                ]
                .into()
            )
        );

        read_str(" {1  2} ").expect_err("only string keys should be allowed in map");
    }

    fn try_read_str(str: &str) -> Value {
        super::read_str(str).expect("read_str should not fail with the given input")
    }
}
