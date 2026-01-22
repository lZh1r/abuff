use abuff::ir::{ControlFlow, Value};

use crate::common::run_typed;

mod common;

#[test]
fn test_import() {
    let src = r#"
        import {a} from "tests/stubs/test";
        a;
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Int(i) => assert_eq!(i, 5),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn test_type_import() {
    let src = r#"
        import {type A} from "tests/stubs/test";
        let a: A = 6;
        a + 4;
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Int(i) => assert_eq!(i, 10),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn test_multiple_imports() {
    let src = r#"
        import {type A, a} from "tests/stubs/test";
        let b: A = 6;
        a + b;
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Int(i) => assert_eq!(i, 11),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn test_chain_imports() {
    let src = r#"
        import {b} from "tests/stubs/hello";
        b(5);
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Int(i) => assert_eq!(i, 10),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn test_intersecting_imports() {
    let src = r#"
        import {b} from "tests/stubs/hello";
        import {a} from "tests/stubs/test";
        b(a);
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Int(i) => assert_eq!(i, 10),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}