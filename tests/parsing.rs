use abuff::ir::{ControlFlow, Value};

use crate::common::{run_typed};

mod common;

#[test]
fn test_math_precedence() {
    let src = r#"
        2 + 3 * 6 / 3 - 4 * (9 - 5)
    "#;
    
    match run_typed(src.to_string()).unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Int(i) => assert_eq!(i, -8),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn test_block_and_anon_abuse() {
    let src = r#"
        let a = 2;
        a + {
            let b = {
                (fun(x: Int): Int {
                    x + 5
                })({ d: 3 }.d)
            };
            b - 4
        } + (fun(x: Int): Int {x * x})(4)
    "#;
    
    match run_typed(src.to_string()).unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Int(i) => assert_eq!(i, 22),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn test_nested_records() {
    let src = r#"
        let obj = {
            a: {
                b: {
                    c: {
                        d: {
                            e: 4
                        }
                    }
                }
            }
        };
        obj.a.b.c.d.e
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
fn test_nested_fun_calls() {
    let src = r#"
        let f = fun(x: Int): Int {x+1};
        f(f(f(f(f(1)))))
    "#;
    
    match run_typed(src.to_string()).unwrap() {
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
fn test_fun_shadowing() {
    let src = r#"
        let a = 2;
        let f = fun(x: Int): Int {
            let a = 0;
            x+a
        };
        let a = 3;
        f(1)
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
fn test_whitespace_max() {
    let src = r#"
        let a = 12 ; 
        fun mega_function ( x : Int ) : Int {
            a + x
        }
        let b = { bb : 2 } ;
        mega_function ( b . bb )
    "#;
    
    match run_typed(src.to_string()).unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Int(i) => assert_eq!(i, 14),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn test_whitespace_min() {
    let src = r#"let a=12;let mega_function=fun(x:Int):Int{a+x};let b={bb:2};mega_function(b.bb)"#;
    
    match run_typed(src.to_string()).unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Int(i) => assert_eq!(i, 14),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn test_if_else() {
    let src = r#"
        if (4 < 3) 3
        else if (2 == 2) 2
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
fn test_anon_fun() {
    let src = r#"
        (fun (a: Int): Int {a*2})(2)
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
fn test_fun_in_record() {
    let src = r#"
        let a = {
            b: fun (x: Int): Int x*2
        };
        a.b(2)
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