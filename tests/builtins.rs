use abuff::ir::{ControlFlow, Value};
use smol_str::SmolStr;

use crate::common::run_typed;

mod common;

#[test]
fn test_len() {
    let src = r#"
        let string = "Hello";
        let array = [1,2,3];
        string.len() + array.len()
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Int(i) => assert_eq!(i, 8),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn test_some() {
    let src = r#"
        Some(1)
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::EnumVariant { enum_name, variant, value } => {
                    assert_eq!(enum_name, "Option".to_string());
                    assert_eq!(variant, "Some".to_string());
                    assert_eq!(*value, Value::Int(1));
                },
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn test_option() {
    let src = r#"
        let some = Some(1);
        let none = None<Int>();
        fun f(a: Option<Int>, b: Option<Int>): Option<Int> {
            a
        }
        f(some, none)
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::EnumVariant { enum_name, variant, value } => {
                    assert_eq!(enum_name, "Option".to_string());
                    assert_eq!(variant, "Some".to_string());
                    assert_eq!(*value, Value::Int(1));
                },
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn methods() {
    let src = r#"
        let some = Some(1);
        some.intoResult().isOk()
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Bool(b) => assert_eq!(b, true),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn sort() {
    let src = r#"
        let array = [2, 1];
        array.sort()
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Array(a) => assert_eq!(a, vec![Value::Int(1), Value::Int(2)]),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn array_reverse() {
    let src = r#"
        let array = [1, 2, 3];
        array.reverse()
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Array(a) => assert_eq!(a, vec![Value::Int(3), Value::Int(2), Value::Int(1)]),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn array_is_empty() {
    let src = r#"
        let array = [];
        array.isEmpty()
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Bool(a) => assert_eq!(a, true),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn string_is_empty() {
    let src = r#"
        let string = "";
        string.isEmpty()
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Bool(a) => assert_eq!(a, true),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn array_get_success() {
    let src = r#"
        let arr = [1,2,3];
        arr.get(0)
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::EnumVariant { enum_name, variant, value } => {
                    assert_eq!(enum_name, "Option".to_string());
                    assert_eq!(variant, "Some".to_string());
                    assert_eq!(*value, Value::Int(1));
                },
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn array_get_failure() {
    let src = r#"
        let arr = [];
        arr.get(0)
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::EnumVariant { enum_name, variant, value } => {
                    assert_eq!(enum_name, "Option".to_string());
                    assert_eq!(variant, "None".to_string());
                    assert_eq!(*value, Value::Void);
                },
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn string_get_success() {
    let src = r#"
        let str = "hello";
        str.get(0)
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::EnumVariant { enum_name, variant, value } => {
                    assert_eq!(enum_name, "Option".to_string());
                    assert_eq!(variant, "Some".to_string());
                    assert_eq!(*value, Value::Char('h'));
                },
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn string_get_failure() {
    let src = r#"
        let str = "hello";
        str.get(8)
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::EnumVariant { enum_name, variant, value } => {
                    assert_eq!(enum_name, "Option".to_string());
                    assert_eq!(variant, "None".to_string());
                    assert_eq!(*value, Value::Void);
                },
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn string_reverse() {
    let src = r#"
        let string = "asd";
        string.reverse()
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::String(a) => assert_eq!(a, SmolStr::new("dsa")),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn starts_with_success() {
    let src = r#"
        let string = "asda";
        string.startsWith("as")
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Bool(a) => assert_eq!(a, true),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn starts_with_fail() {
    let src = r#"
        let string = "asda";
        string.startsWith("ad")
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Bool(a) => assert_eq!(a, false),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn ends_with_success() {
    let src = r#"
        let string = "asda";
        string.endsWith("da")
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Bool(a) => assert_eq!(a, true),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn ends_with_fail() {
    let src = r#"
        let string = "asda";
        string.endsWith("das")
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Bool(a) => assert_eq!(a, false),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn string_case_manipulation() {
    let src = r#"
        let string = "Hello";
        let string = string.toUpperCase();
        assert(string == "HELLO", "toUpperCase is broken!");
        string.toLowerCase()
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::String(s) => assert_eq!(s, "hello"),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn string_trim() {
    let src = r#"
        let string = "\n\t   hello \t   \n\n";
        string.trim()
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::String(s) => assert_eq!(s, "hello"),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn char_is_checks() {
    let src = r#"
        assert('a'.isAlphabetic() && !'2'.isAlphabetic(), "isAlphabetic is broken!");
        assert('b'.isAlphanumeric() && '2'.isAlphanumeric() && !'!'.isAlphanumeric(), "isAlphanumeric is broken!");
        assert('9'.isNumeric() && !'c'.isNumeric(), "isNumeric is broken!");
        assert('z'.isLowerCase() && !'G'.isLowerCase(), "isLowerCase is broken!");
        assert('L'.isUpperCase() && !'a'.isUpperCase(), "isUpperCase is broken!");
        assert(!'c'.isWhitespace(), "isWhitespace is broken!");
        ' '.isWhitespace()
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Bool(a) => assert_eq!(a, true),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn char_to_string() {
    let src = r#"
        'h'.toString()
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::String(s) => assert_eq!(s, "h"),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}