use gigalang::ir::Value;

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
        Value::Int(i) => assert_eq!(i, 100),
        _ => panic!()
    };
}

#[test]
fn test_logic_precedence() {
    let src = r#"
        !(!(4 < 5) && (3 % 3 == 0 && 5 % 2 == 1 || 8 - 3 == 5 && 3 % 3 == 1));
    "#;
    
    match run_typed(src.to_string()).unwrap() {
        Value::Bool(b) => assert_eq!(b, true),
        _ => panic!()
    };
}