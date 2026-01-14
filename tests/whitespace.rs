use gigalang::ir::Value;

use crate::common::{parse_code, run};

mod common;

#[test]
fn test_whitespace_max() {
    let ast = parse_code(r#"
        let a = 12 ; 
        let mega_function = fun ( x ) {
            a + x
        } ;
        let b = { bb : 2 } ;
        mega_function ( b . bb ) ;
    "#);
    
    match run(&ast).unwrap() {
        Value::Int(i) => assert_eq!(i, 14),
        _ => panic!()
    };
}

#[test]
fn test_whitespace_min() {
    let ast = parse_code(r#"let a=12;let mega_function=fun(x){a+x};let b={bb:2};mega_function(b.bb);"#);
    
    match run(&ast).unwrap() {
        Value::Int(i) => assert_eq!(i, 14),
        _ => panic!()
    };
}