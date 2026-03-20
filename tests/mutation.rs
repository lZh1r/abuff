mod common;
use abuff::ast::clean::{ControlFlow, Value};

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

#[test]
fn nested_array_mutation() {
    let src = r#"
        let mut a = [[[[1]]]];
        a[0][0][0][0] = 2;
        a[0][0][0][0]
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

#[test]
fn nested_record_mutation() {
    let src = r#"
        let mut a = {
            b: {
                c: {
                    d: 1
                }
            }
        };
        a.b.c.d = 4;
        a.b.c.d
    "#;
    
    match run_typed(src.to_string()).unwrap() {
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
fn mixed_nested_mutation() {
    let src = r#"
        let mut a = {
            b: {
                c: [{
                    d: 1
                }]
            }
        };
        a.b.c[0].d = 5;
        a.b.c[0].d
    "#;
    
    match run_typed(src.to_string()).unwrap() {
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

#[test]
fn array_push_pop() {
    let src = r#"
        let mut a = [0];
        a.push(1);
        a.pop().unwrap()
    "#;
    
    match run_typed(src.to_string()).unwrap() {
        ControlFlow::Value(v) => match v {
            Value::Int(i) => assert_eq!(i, 1),
            _ => panic!()
        },
        _ => panic!()
    };
}

#[test]
fn tuple_mut() {
    let src = r#"
        let mut a = (1,2,3);
        a[0] = 3;
        a[1] = 3;
        a[0] + a[1] + a[2]
    "#;
    
    match run_typed(src.to_string()).unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Int(i) => assert_eq!(i, 9),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn mut_fun_param_fail() {
    let src = r#"
        fun f(mut a: Int[]) {
            a.push(4)
        }
        
        let a = [1,2,3];
        f(a)
        
        print(a)
    "#;
    
    match run_typed(src.to_string()) {
        Err(e) => {
            if !(e.inner == "Cannot pass an immutable reference as a mutable argument") {
                panic!()
            }
        },
        _ => panic!()
    };
}

#[test]
fn mut_fun_param() {
    let src = r#"
        fun f(mut a: Int[]) {
            a.push(4)
        }
        
        let mut a = [1,2,3];
        f(a)
        
        a[3]
    "#;
    
    match run_typed(src.to_string()).unwrap() {
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
fn mut_variadic_fun_param() {
    let src = r#"
        fun f(mut ...a: Int[][]) {
            a[0].push(4)
        }
        
        let mut a = [1,2,3];
        let mut b = [1,2,3];
        f(a, b)
        
        a[3] + b.len()
    "#;
    
    match run_typed(src.to_string()).unwrap() {
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
fn mut_variadic_fun_param_fail() {
    let src = r#"
        fun f(mut ...a: Int[][]) {
            a[0].push(4)
        }
        
        let mut a = [1,2,3];
        f(a, [1,2,3])
    "#;
    
    match run_typed(src.to_string()) {
        Err(e) => {
            if !(e.inner == "Cannot pass a literal expression as a mutable argument") {
                panic!()
            }
        },
        _ => panic!()
    };
}
