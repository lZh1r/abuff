use abuff::ir::{ControlFlow, Value};

use crate::common::run_typed;

mod common;

#[test]
fn test_len() {
    let src = r#"
        let string = "Hello";
        let array = [1,2,3];
        len(string) + len(array);
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