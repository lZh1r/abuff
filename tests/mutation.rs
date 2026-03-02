mod common;
use abuff::ir::{ControlFlow, Value};

use crate::common::run_typed;

#[test]
fn variable_mut() {
    let src = r#"
        let mut a = 10;
        a -= 9;
        a
    "#;
    
    match run_typed(src.to_string()).unwrap() {
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
fn variable_immut() {
    let src = r#"
        let a = 10;
        a -= 9;
    "#;
    
    match run_typed(src.to_string()) {
        Err(e) => {
            assert!(e.inner.contains("mutable"))
        }
        _ => panic!()
    };
}

#[test]
fn array_mut() {
    let src = r#"
        let mut a = ["a", "b", "c"];
        a[0] = "z";
        a[0]
    "#;
    
    match run_typed(src.to_string()).unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::String(s) => assert_eq!(s, "z"),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn array_immut() {
    let src = r#"
        let a = ["a", "b", "c"];
        a[0] = "z";
    "#;
    
    match run_typed(src.to_string()) {
        Err(e) => {
            assert!(e.inner.contains("mutable"))
        }
        _ => panic!()
    };
}

#[test]
fn record_mut() {
    let src = r#"
        let mut a = {
            a: 'a',
            b: 'b',
            c: 'c'
        };
        a.a = 'z';
        a.a
    "#;
    
    match run_typed(src.to_string()).unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Char(c) => assert_eq!(c, 'z'),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn record_immut() {
    let src = r#"
        let a = {
            a: 'a',
            b: 'b',
            c: 'c'
        };
        a.a = 'z';
    "#;
    
    match run_typed(src.to_string()) {
        Err(e) => {
            assert!(e.inner.contains("mutable"))
        }
        _ => panic!()
    };
}

//should not pass
#[test]
fn shared_reference_mutation() {
    let src = r#"
        let mut a = [1];
        let b = a;
        a[0] = 2;
        b[0]
    "#;
    
    match run_typed(src.to_string()).unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Int(i) => assert_eq!(i, 2),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}