use std::{collections::HashMap, sync::{Mutex, OnceLock}};

use crate::{ast::Spanned, ir::{ControlFlow, Value}};
//Box<dyn Fn(Vec<Value>) -> Result<Value, String> + Send + Sync>

pub type NativeFun = fn(&[Value]) -> Result<ControlFlow, Spanned<String>>;

static NATIVE_FUN_REGISTRY: OnceLock<Mutex<HashMap<(String, String), NativeFun>>> = OnceLock::new();

pub fn register_fun(path: &str, name: &str, fun: NativeFun) {
    let mut registry = NATIVE_FUN_REGISTRY.get_or_init(|| Mutex::new(HashMap::new())).lock().unwrap();
    registry.insert((path.into(), name.into()), fun);
}

pub fn get_native_fun(path: &str, name: &str) -> Option<NativeFun> {
    let registry = NATIVE_FUN_REGISTRY.get_or_init(|| Mutex::new(HashMap::new())).lock().unwrap();
    registry.get(&(path.into(), name.into())).copied()
}