use abuff::ast::clean::{ControlFlow, Value};

use crate::common::{run_typed};

mod common;

#[test]
fn simple_int_match() {
    let src = r#"
        let a = 12;
        match a {
            1 -> 2,
            12 -> 10,
            _ -> 1
        }
    "#;
    
    match run_typed(src.to_string()).unwrap() {
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
fn conditional_int_match() {
    let src = r#"
        let a = 12;
        match a {
            1 -> 2,
            i if i % 2 == 0 -> 10,
            _ -> 1
        }
    "#;
    
    match run_typed(src.to_string()).unwrap() {
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
fn enum_match() {
    let src = r#"
        enum A {
            B: Int,
            C: Int
        };
        let a = A.B(1);
        match a {
            A.B(v) -> v,
            A.C(_) -> 123
        }
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
fn enum_match_shorthand() {
    let src = r#"
        enum A {
            B: Int,
            C: Int
        };
        let a = A.B(1);
        match a {
            .B(v) -> v,
            .C(_) -> 123
        }
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
fn builtin_enum_match() {
    let src = r#"
        let a = Some(1);
        match a {
            Option.Some(v) -> v,
            Option.None(_) -> 123
        }
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
fn builtin_enum_match_shorthand() {
    let src = r#"
        let a = Some(1);
        match a {
            .Some(v) -> v,
            .None(_) -> 123
        }
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
fn simple_tuple_match() {
    let src = r#"
        let a = (1,2);
        match a {
            (1,2) -> 1,
            _ -> 2
        }
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
fn conditional_tuple_match() {
    let src = r#"
        let a = (1,2);
        match a {
            (1,1) -> 3,
            (x if x % 2 == 1, y if y % 2 == 0) -> x,
            _ -> 2
        }
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
fn enum_tuple_match() {
    let src = r#"
        let a = (Some(4), Some(3));
        match a {
            (.None(_), .None(_)) -> 2,
            (.Some(a), .Some(b)) -> a - b,
            _ -> 0
        }
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
fn enum_default_tuple_match() {
    let src = r#"
        let a = (Some(4), 2);
        match a {
            (.None(_), 2) -> 2,
            (.Some(a), _) -> a,
            _ -> 0
        }
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
fn nested_mixed_tuple_match_default() {
    let src = r#"
        let a = ((Some(4), "123"), 2);
        match a {
            ((.None(_), "hello"), 2) -> 1,
            (_, a) -> a,
            _ -> 0
        }
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
fn nested_mixed_tuple_match() {
    let src = r#"
        let a = ((Some(4), "123"), 2);
        match a {
            ((.Some(a), x if x.len() == 3), 2) -> a + x.len() + 2,
            _ -> 0
        }
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
fn match_dead_branches() {
    let src = r#"
        let a = (Some(4), 2);
        match a {
            (.None(_), 2) -> 2,
            (a, b) -> 1,
            (.Some(a), _) -> a,
            (.Some(a), 2) -> a,
            _ -> 0
        }
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