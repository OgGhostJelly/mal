use core::str;
use lazy_static::lazy_static;
use std::{collections::HashMap, num::ParseIntError, rc::Rc, str::Utf8Error};

use pcre2::bytes::Regex;

use crate::{list, sym, types::MapKey, MalVal};

type Result<T> = std::result::Result<T, Error>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("unmatched left parenthesis")]
    UnmatchedLeftParenthesis,
    #[error("unmatched right parenthesis")]
    UnmatchedRightParenthesis,
    #[error("invalid map key type, map keys can only be string or keyword type")]
    InvalidMapKeyType,
    #[error("key does not have a corresponding value")]
    MismatchedKey,
    #[error(transparent)]
    Pcre(#[from] pcre2::Error),
    #[error(transparent)]
    NumberParse(#[from] ParseIntError),
    #[error(transparent)]
    InvalidUtf8(#[from] Utf8Error),
    #[error("unexpected eof")]
    UnexpectedEof,
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
    pub fn read_form(&mut self) -> Result<MalVal> {
        let val = match self.peek() {
            b"(" => Ok(MalVal::List(SeqReader::new(self, b")").into_vec()?)),
            b"[" => Ok(MalVal::Vector(SeqReader::new(self, b"]").into_vec()?)),
            b"{" => Ok(MalVal::Map(SeqReader::new(self, b"}").into_map()?)),
            b"@" => Ok(list!(sym!("deref"), self.next_form()?)),
            b"'" => Ok(list!(sym!("quote"), self.next_form()?)),
            b"`" => Ok(list!(sym!("quasiquote"), self.next_form()?)),
            b"~" => Ok(list!(sym!("unquote"), self.next_form()?)),
            b"~@" => Ok(list!(sym!("splice-unquote"), self.next_form()?)),
            _ => self.read_atom(),
        }?;
        Ok(val)
    }

    fn next_form(&mut self) -> Result<MalVal> {
        self.next();
        self.read_form()
    }

    pub fn read_atom(&self) -> Result<MalVal> {
        let token = self.peek();

        lazy_static! {
            static ref INT_RE: Regex = Regex::new(r#"^-?[0-9]+$"#).expect("regex should be valid");
            static ref STR_RE: Regex =
                Regex::new(r#""(?:\\.|[^\\"])*""#).expect("regex should be valid");
            static ref KEYWORD_RE: Regex =
                Regex::new(r#"^:[\S]*$"#).expect("regex should be valid");
        }

        match token {
            b"nil" => Ok(MalVal::Nil),
            b"true" => Ok(MalVal::Bool(true)),
            b"false" => Ok(MalVal::Bool(false)),
            token => {
                let str = str::from_utf8(token)?;
                if INT_RE.is_match(token)? {
                    Ok(MalVal::Int(str.parse()?))
                } else if STR_RE.is_match(token)? {
                    Ok(MalVal::Str(unescape_str(str)))
                } else if KEYWORD_RE.is_match(token)? {
                    Ok(MalVal::Kwd(str[1..].to_string()))
                } else {
                    Ok(MalVal::Sym(str.to_string()))
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
    pub fn into_vec(self) -> Result<Rc<Vec<MalVal>>> {
        let mut vec = vec![];
        for value in self {
            vec.push(value?);
        }
        Ok(Rc::new(vec))
    }

    pub fn into_map(self) -> Result<Rc<HashMap<MapKey, MalVal>>> {
        let mut map = HashMap::new();
        let mut key = None;
        for value in self {
            match key.take() {
                Some(key) => {
                    map.insert(key, value?);
                }
                None => match value? {
                    MalVal::Str(str) => key = Some(MapKey::Str(str)),
                    MalVal::Kwd(str) => key = Some(MapKey::Keyword(str)),
                    _ => return Err(Error::InvalidMapKeyType),
                },
            }
        }
        if key.is_some() {
            return Err(Error::MismatchedKey);
        }
        Ok(Rc::new(map))
    }
}

impl Iterator for SeqReader<'_, '_> {
    type Item = Result<MalVal>;

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

pub fn read_str(str: &str) -> Result<MalVal> {
    let mut reader = Reader::new(tokenize(str)?);
    let val = reader.read_form()?;

    if reader.position + 1 != count_tokens_trim_end(&reader) {
        return Err(Error::UnexpectedEof);
    }

    Ok(val)
}

/// Count the amount of tokens in the reader excluding empty tokens at the end.
fn count_tokens_trim_end(reader: &Reader<'_>) -> usize {
    let mut count = reader.tokens.len();
    for value in reader.tokens.iter().rev() {
        if value.is_empty() {
            count -= 1;
        } else {
            break;
        }
    }
    count
}

#[cfg(test)]
mod test {
    use core::str;
    use std::rc::Rc;

    use crate::{types::MapKey, MalVal};

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
            MalVal::List(
                vec![
                    MalVal::Sym("+".into()),
                    MalVal::Int(1),
                    MalVal::List(
                        vec![
                            MalVal::Sym("+".into()),
                            MalVal::Str("my \\ cool \" string\n".into()),
                            MalVal::Int(3),
                        ]
                        .into()
                    ),
                ]
                .into()
            )
        );

        let value = try_read_str("  123  ");
        assert_eq!(value, MalVal::Int(123));

        let value = try_read_str("  [true  nil  false] ");
        assert_eq!(
            value,
            MalVal::Vector(vec![MalVal::Bool(true), MalVal::Nil, MalVal::Bool(false),].into())
        );

        let value = try_read_str("  abc  ");
        assert_eq!(value, MalVal::Sym("abc".into()));

        let value = try_read_str("(:abc)");
        assert_eq!(value, MalVal::List(vec![MalVal::Kwd("abc".into()),].into()));

        let value = try_read_str("{\"a\" 1 :b 2 \"c\" 3}");
        assert_eq!(
            value,
            MalVal::Map(Rc::new(
                [
                    (MapKey::Str("a".into()), MalVal::Int(1)),
                    (MapKey::Keyword("b".into()), MalVal::Int(2)),
                    (MapKey::Str("c".into()), MalVal::Int(3)),
                ]
                .into()
            ),)
        );

        read_str(" {1  2} ").expect_err("only string keys should be allowed in map");
        read_str(" {:a} ").expect_err("mismatched keys in map should fail");
    }

    fn try_read_str(str: &str) -> MalVal {
        super::read_str(str).expect("read_str should not fail with the given input")
    }
}
