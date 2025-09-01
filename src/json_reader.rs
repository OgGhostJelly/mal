use std::{collections::HashMap, rc::Rc};

use json::JsonValue;

use crate::{types::MapKey, MalVal};

pub fn read_str(s: &str) -> Result<Option<MalVal>> {
    let value = json::parse(s)?;
    Ok(json_to_mal(value))
}

fn json_to_mal(value: JsonValue) -> Option<MalVal> {
    match value {
        JsonValue::Null => todo!(),
        JsonValue::Short(short) => json_str_to_mal(short.as_str()),
        JsonValue::String(string) => json_str_to_mal(string),
        JsonValue::Number(number) => match number.try_into() {
            Ok(value) => Some(MalVal::Int(value)),
            Err(_) => todo!("float is not yet supported"),
        },
        JsonValue::Boolean(value) => Some(MalVal::Bool(value)),
        JsonValue::Object(mut object) => {
            let keys: Vec<String> = object.iter().map(|(key, _)| key.to_string()).collect();
            let mut map = HashMap::new();
            for key in keys {
                let value = object.remove(&key).expect("key should be in object");
                if let Some(value) = json_to_mal(value) {
                    _ = map.insert(MapKey::Str(key), value);
                }
            }
            Some(MalVal::Map(Rc::new(map)))
        }
        JsonValue::Array(array) => {
            let mut ls = Vec::new();
            for value in array {
                if let Some(value) = json_to_mal(value) {
                    ls.push(value);
                }
            }
            Some(MalVal::List(Rc::new(ls)))
        }
    }
}

fn json_str_to_mal<T: AsRef<str> + Into<String>>(value: T) -> Option<MalVal> {
    Some(match value.as_ref() {
        input if input.starts_with('\"') && input.ends_with('\"') => MalVal::Str(value.into()),
        input if input.starts_with(':') => MalVal::Kwd(value.into()),
        input if input.starts_with(";") => return None,
        _ => MalVal::Sym(value.into()),
    })
}

pub type Result<T> = std::result::Result<T, Error>;
pub type Error = json::JsonError;
