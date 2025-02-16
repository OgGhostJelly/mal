use std::fmt::{self, Write};

use crate::Value;

pub fn write_value(o: &mut impl Write, value: &Value) -> fmt::Result {
    match value {
        Value::List(values) => {
            o.write_char('(')?;
            for (index, value) in values.iter().enumerate() {
                if index != 0 {
                    o.write_char(' ')?;
                }
                write_value(o, value)?;
            }
            o.write_char(')')?;
            Ok(())
        },
        Value::Symbol(sym) => o.write_str(sym),
        Value::Int(num) => o.write_str(&num.to_string()),
        Value::Bool(true) => o.write_str("true"),
        Value::Bool(false) => o.write_str("false"),
        Value::Nil => o.write_str("nil"),
        Value::Str(str) => write!(o, "\"{}\"", escape_str(str)),
        Value::Keyword(str) => write!(o, ":{str}"),
    }
}

fn escape_str(str: &str) -> String {
    str.replace("\\", "\\\\")
        .replace("\n", "\\n")
        .replace("\"", "\\\"")
}