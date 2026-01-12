use gigalang::ast::Value;

use crate::common::{parse_code, run};

mod common;

#[test]
fn test_precedence_math() {
    let ast = parse_code(r#"
        2 + 3 * 6 / 3 - 4 * (9 - 5);
    "#);
    
    match run(&ast).unwrap() {
        Value::Int(i) => assert_eq!(i, -8),
        _ => panic!()
    };
}

#[test]
fn test_block_and_anon_abuse() {
    let ast = parse_code(r#"
        let a = 2;
        a + {
            let b = {
                (fun(x) {
                    x + 5
                })({ d: 3 }.d)
            };
            b - 4
        } + (fun(x) {x * x})(4);
    "#);
    
    match run(&ast).unwrap() {
        Value::Int(i) => assert_eq!(i, 22),
        _ => panic!()
    };
}