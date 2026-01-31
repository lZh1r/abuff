use abuff::ir::{ControlFlow, Value};

use crate::common::run_typed;

mod common;

#[test]
fn test_simle_generic() {
    let src = r#"
        fun f<T>(x: T): T {
            x
        }
        f(1)
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Int(i) => assert_eq!(i, 1),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn test_array_generic() {
    let src = r#"
        fun f<T>(x: T[]): Int {
            len(x)
        }
        f([3,5,6]) + f("Hello")
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
fn test_nested_array_generic() {
    let src = r#"
        fun f<T>(x: T[][]): Int {
            len(x)
        }
        f([[3,5,6], [1]])
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Int(i) => assert_eq!(i, 2),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}