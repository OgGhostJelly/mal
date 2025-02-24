use std::fmt::{self, Display, Write};

use crate::MalVal;

pub fn write_value(o: &mut impl Write, value: &MalVal, print_readably: bool) -> fmt::Result {
    match value {
        MalVal::List(values) => write_seq(o, values.iter(), print_readably, '(', ')'),
        MalVal::Sym(sym) => o.write_str(sym),
        MalVal::Int(num) => o.write_str(&num.to_string()),
        MalVal::Bool(true) => o.write_str("true"),
        MalVal::Bool(false) => o.write_str("false"),
        MalVal::Nil => o.write_str("nil"),
        MalVal::Str(str) => {
            if print_readably {
                write!(o, "\"{}\"", escape_str(str))
            } else {
                write!(o, "{str}")
            }
        }
        MalVal::Kwd(str) => write!(o, ":{str}"),
        MalVal::Vector(values) => write_seq(o, values.iter(), print_readably, '[', ']'),
        MalVal::Map(map) => write_seq(
            o,
            map.iter().flat_map(|(a, b)| [a.to_string(), b.to_string()]),
            print_readably,
            '{',
            '}',
        ),
        MalVal::Func(name, _) => match name {
            Some(name) => write!(o, "#<function:{name}>"),
            None => write!(o, "#<function>"),
        },
        MalVal::MalFunc { name, is_macro, .. } => {
            if *is_macro {
                match name {
                    Some(name) => write!(o, "#<macro:{name}>"),
                    None => write!(o, "#<macro>"),
                }
            } else {
                match name {
                    Some(name) => write!(o, "#<function:{name}>"),
                    None => write!(o, "#<function>"),
                }
            }
        }
        MalVal::Atom(atom) => {
            if print_readably {
                write!(o, "(atom {:#})", atom.borrow())
            } else {
                write!(o, "(atom {})", atom.borrow())
            }
        }
    }
}

fn write_seq(
    o: &mut impl Write,
    iter: impl Iterator<Item = impl Display>,
    print_readably: bool,
    begin: char,
    end: char,
) -> fmt::Result {
    o.write_char(begin)?;
    for (index, value) in iter.enumerate() {
        if index != 0 {
            o.write_char(' ')?;
        }
        if print_readably {
            write!(o, "{value:#}")?;
        } else {
            write!(o, "{value}")?;
        }
    }
    o.write_char(end)?;
    Ok(())
}

fn escape_str(str: &str) -> String {
    str.replace('\\', "\\\\")
        .replace('\n', "\\n")
        .replace('\"', "\\\"")
}

#[cfg(test)]
mod test {
    use std::rc::Rc;

    use crate::types::{MalVal, MapKey};

    #[test]
    fn printing() {
        assert_eq!(
            format!("{:#}", MalVal::Str("my \"cool\" string".into())),
            r#""my \"cool\" string""#,
        );

        assert_eq!(
            format!("{}", MalVal::Str("my \"cool\" string".into())),
            r#"my "cool" string"#,
        );

        assert_eq!(
            MalVal::List(
                vec![
                    MalVal::Sym("+".into()),
                    MalVal::Int(123),
                    MalVal::Vector(vec![MalVal::Bool(true), MalVal::Bool(false)].into()),
                    MalVal::Nil,
                ]
                .into()
            )
            .to_string(),
            "(+ 123 [true false] nil)",
        );

        let str = MalVal::Map(Rc::new(
            [
                (MapKey::Kwd("a".into()), MalVal::Nil),
                (MapKey::Kwd("b".into()), MalVal::Int(2)),
            ]
            .into(),
        ))
        .to_string();
        assert!(str == "{:a nil :b 2}" || str == "{:b 2 :a nil}");
    }
}
