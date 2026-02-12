use abuff::ir::{ControlFlow, Value};

use crate::common::run_typed;

mod common;

#[test]
fn test_len() {
    let src = r#"
        let string = "Hello";
        let array = [1,2,3];
        len<String>(string) + len<Int[]>(array)
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