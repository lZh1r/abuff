use abuff::ir::{ControlFlow, Value};

use crate::common::run_typed;

mod common;

#[test]
fn test_simle_generic() {
    let src = r#"
        fun f<T>(x: T): T {
            x
        }
        f<Int>(1);
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