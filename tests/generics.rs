use abuff::ir::{ControlFlow, Value};

use crate::common::run_typed;

mod common;

#[test]
fn test_simle_generic_implicit() {
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
fn test_simle_generic() {
    let src = r#"
        fun f<T>(x: T): T {
            x
        }
        f<Int>(1)
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
fn test_array_generic_implicit() {
    let src = r#"
        fun f<T>(x: T[]): Int {
            len(x)
        }
        f([3,5,6]) + f(["Hello"])
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Int(i) => assert_eq!(i, 4),
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
        f<Int>([3,5,6]) + f<String>(["Hello"])
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Int(i) => assert_eq!(i, 4),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn test_nested_array_generic_implicit() {
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

#[test]
fn test_nested_array_generic() {
    let src = r#"
        fun f<T>(x: T[][]): Int {
            len(x)
        }
        f<Int>([[3,5,6], [1]])
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

#[test]
fn test_enum_generic_implicit() {
    let src = r#"
        enum A<T> {
            B: T
        }
        
        A.B(1)
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::EnumVariant { enum_name, variant, value } => {
                    assert_eq!(enum_name, "A".to_string());
                    assert_eq!(variant, "B".to_string());
                    assert_eq!(*value, Value::Int(1));
                },
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn test_enum_generic() {
    let src = r#"
        enum A<T> {
            B: T
        }
        
        A.B<Int>(1)
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::EnumVariant { enum_name, variant, value } => {
                    assert_eq!(enum_name, "A".to_string());
                    assert_eq!(variant, "B".to_string());
                    assert_eq!(*value, Value::Int(1));
                },
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn test_generic_types() {
    let src = r#"
        type A<T> = {a: T};
        fun f(a: A<Int>): Int {
            a.a
        }
        let a: A<Int> = {a: 1};
        f(a)
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