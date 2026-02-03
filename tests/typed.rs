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
        let get_player_x = fun(p: Player): Int {
            p.pos.x
        };
        let raw_data = {
            pos: { x: 100, y: 200 },
            id: 1
        };
        get_player_x(raw_data)
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
        !(!(4 < 5) && (3 % 3 == 0 && 5 % 2 == 1 || 8 - 3 == 5 && 3 % 3 == 1))
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
fn test_comment_handling() {
    let src = r#"
        // This is a single-line comment
        let x = 42; // Another single-line comment
        
        /* This is a
           multi-line comment */
        let y = 100;
        
        let z = x + y;
        
        /// This is a doc comment
        let result = z * 2;
        
        result
    "#;
    
    match run_typed(src.to_string()).unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Int(i) => assert_eq!(i, 284), // (42 + 100) * 2 = 284
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
        acc
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
        fun ff(): (x: Int) -> Int {
            fun (x: Int): Int {
                if (x == 2) return 0;
                x
            }
        }
        ff()(2)
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

#[test]
fn test_array_indexing() {
    let src = r#"
        let a = [ 1 , 2 , 3 ];
        a[2] + a[3 % 2]
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
fn test_array_mutation() {
    let src = r#"
        let a = [ 1 , 2 , 3 ];
        a[2] = 4;
        a[2]
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
fn test_spread_params() {
    let src = r#"
        fun f(...x: Int[]): Int[] {
            x
        }
        f(1,2,3)
    "#;
    
    match run_typed(src.to_string()).unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Array(i) => assert_eq!(i, vec![Value::Int(1), Value::Int(2), Value::Int(3)]),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn test_simpler_enum() {
    let src = r#"
        enum Color {
            Red,
            Green,
            Blue
        }
        fun giveColor(x: Color): Color {
            x
        }
        giveColor(Color.Red())
    "#;
    
    match run_typed(src.to_string()).unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::EnumVariant { enum_name, variant, value } => {
                    assert_eq!(enum_name, "Color".to_string());
                    assert_eq!(variant, "Red".to_string());
                    assert_eq!(*value, Value::Void);
                },
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn test_simple_enum() {
    let src = r#"
        enum Hello {
            A: Int,
            B: String
        }
        fun world(x: Hello.A): Hello {
            x
        }
        world(Hello.A(2))
    "#;
    
    match run_typed(src.to_string()).unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::EnumVariant { enum_name, variant, value } => {
                    assert_eq!(enum_name, "Hello".to_string());
                    assert_eq!(variant, "A".to_string());
                    assert_eq!(*value, Value::Int(2));
                },
                _ => panic!()
            }
        }
        _ => panic!()
    };
}

#[test]
fn test_hoist_type() {
    let src = r#"
        let a: T = {a: 5};
        type T = {a: Int};
        a.a
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
fn test_hoist_function() {
    let src = r#"
        let a: T = f(5);
        fun f(val: Int): Int {
            val*val
        }
        a.a
    "#;
    
    match run_typed(src.to_string()).unwrap() {
        ControlFlow::Value(v) => {
            match v {
                Value::Int(i) => assert_eq!(i, 25),
                _ => panic!()
            }
        }
        _ => panic!()
    };
}