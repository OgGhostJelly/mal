use std::fmt::{self, Display, Write};

use crate::Value;

pub fn write_value(o: &mut impl Write, value: &Value) -> fmt::Result {
    match value {
        Value::List(values) => write_seq(o, values.iter(), '(', ')'),
        Value::Symbol(sym) => o.write_str(sym),
        Value::Int(num) => o.write_str(&num.to_string()),
        Value::Bool(true) => o.write_str("true"),
        Value::Bool(false) => o.write_str("false"),
        Value::Nil => o.write_str("nil"),
        Value::Str(str) => write!(o, "\"{}\"", escape_str(str)),
        Value::Keyword(str) => write!(o, ":{str}"),
        Value::Vector(values) => write_seq(o, values.iter(), '[', ']'),
        Value::Map(map) => write_seq(
            o,
            map.iter().flat_map(|(a, b)| [a.to_string(), b.to_string()]),
            '{',
            '}',
        ),
        Value::Func(name, _) => write!(o, "<function:{name}>"),
    }
}

fn write_seq(
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
mod test {
    use crate::types::{MapKey, Value};

    #[test]
    fn printing() {
        assert_eq!(
            Value::Str("my \"cool\" string".into()).to_string(),
            r#""my \"cool\" string""#,
        );

        assert_eq!(
            Value::List(
                [
                    Value::Symbol("+".into()),
                    Value::Int(123),
                    Value::Vector(vec![Value::Bool(true), Value::Bool(false)]),
                    Value::Nil,
                ]
                .into()
            )
            .to_string(),
            "(+ 123 [true false] nil)",
        );

        let str = Value::Map(
            [
                (MapKey::Keyword("a".into()), Value::Nil),
                (MapKey::Keyword("b".into()), Value::Int(2)),
            ]
            .into(),
        )
        .to_string();
        assert!(str == "{:a nil :b 2}" || str == "{:b 2 :a nil}");
    }
}
