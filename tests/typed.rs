use abuff::ir::{ControlFlow, Value};

use crate::common::{run_typed};

mod common;

#[test]
fn test_custom_type_spam() {
    let src = r#"
        type Coord = Int;
        type Vector2 = { x: Coord, y: Coord };
        type Entity = { pos: Vector2, id: Int };
        type Player = Entity;
        let get_player_x = fun(p: Player) -> Int {
            p.pos.x
        };
        let raw_data = {
            pos: { x: 100, y: 200 },
            id: 1
        };
        get_player_x(raw_data);
    "#;
    
    match run_typed(src.to_string()).unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Int(i) => assert_eq!(i, 100),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn test_logic_precedence() {
    let src = r#"
        !(!(4 < 5) && (3 % 3 == 0 && 5 % 2 == 1 || 8 - 3 == 5 && 3 % 3 == 1));
    "#;
    
    match run_typed(src.to_string()).unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Bool(i) => assert_eq!(i, true),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn test_factorial() {
    let src = r#"
        let n = 5;
        let acc = 1;
        while (n > 0) {
            acc = acc * n;
            n = n - 1;
        };
        acc;
    "#;
    
    match run_typed(src.to_string()).unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Int(i) => assert_eq!(i, 120),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn test_nested_function_return() {
    let src = r#"
        fun ff() -> (x: Int) -> Int {
            fun (x: Int) -> Int {
                if (x == 2) return 0;
                x
            }
        };
        ff()(2);
    "#;
    
    match run_typed(src.to_string()).unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Int(i) => assert_eq!(i, 0),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

