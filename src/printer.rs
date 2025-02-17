use std::fmt::{self, Display, Write};

use crate::Value;

pub fn write_value(o: &mut impl Write, value: &Value) -> fmt::Result {
    match value {
        Value::List(values) => write_seq(o, values.into_iter(), '(', ')'),
        Value::Symbol(sym) => o.write_str(sym),
        Value::Int(num) => o.write_str(&num.to_string()),
        Value::Bool(true) => o.write_str("true"),
        Value::Bool(false) => o.write_str("false"),
        Value::Nil => o.write_str("nil"),
        Value::Str(str) => write!(o, "\"{}\"", escape_str(str)),
        Value::Keyword(str) => write!(o, ":{str}"),
        Value::Vector(values) => write_seq(o, values.into_iter(), '[', ']'),
        Value::Map(map) => write_seq(
            o,
            map.into_iter().flat_map(|(a, b)| {
                let val: [Box<dyn Display>; 2] = [Box::new(a), Box::new(b)];
                val
            }),
            '{',
            '}',
        ),
    }
}

fn write_seq<'a>(
    o: &mut impl Write,
    iter: impl Iterator<Item = impl Display>,
    begin: char,
    end: char,
) -> fmt::Result {
    o.write_char(begin)?;
    for (index, value) in iter.enumerate() {
        if index != 0 {
            o.write_char(' ')?;
        }
        write!(o, "{value}")?;
    }
    o.write_char(end)?;
    Ok(())
}

fn escape_str(str: &str) -> String {
    str.replace("\\", "\\\\")
        .replace("\n", "\\n")
        .replace("\"", "\\\"")
}

#[cfg(test)]
mod test {}
