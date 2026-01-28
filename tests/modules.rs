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

#[test]
fn test_struct_with_custom_type_imports() {
    let src = r#"
        import {type S} from "tests/stubs/struct";
        let s: S = {
            a: 2,
            b: 3
        };
        s.a * s.b;
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Int(i) => assert_eq!(i, 6),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn test_aliased_imports() {
    let src = r#"
        import {b as c} from "tests/stubs/hello";
        c(5);
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
fn test_builtins_imports() {
    let src = r#"
        print(1);
        7;
    "#;
    
    let res = run_typed(src.to_string());
    
    match res.unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Int(i) => assert_eq!(i, 7),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn test_import_simple_enum() {
    let src = r#"
        import {Test} from "tests/stubs/enum";
        fun test(x: Test): Test {
            x
        }
        test(Test.A(2));
    "#;
    
    match run_typed(src.to_string()).unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::EnumVariant { enum_name, variant, value } => {
                    assert_eq!(enum_name, "Test".to_string());
                    assert_eq!(variant, "A".to_string());
                    assert_eq!(*value, Value::Int(2));
                },
                _ => panic!()
            }
        }
        _ => panic!()
    };
}