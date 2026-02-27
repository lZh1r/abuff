
use std::{collections::HashMap, sync::{Mutex, OnceLock}};
use smol_str::SmolStr;

use crate::{ast::{Spanned, TypeInfo}, ir::{ControlFlow, Value}};

pub type NativeFun = fn(&[Value]) -> Result<ControlFlow, Spanned<SmolStr>>;

static NATIVE_FUN_REGISTRY: OnceLock<Mutex<HashMap<(SmolStr, SmolStr), NativeFun>>> = OnceLock::new();
static NATIVE_TYPE_REGISTRY: OnceLock<Mutex<HashMap<(SmolStr, SmolStr), TypeInfo>>> = OnceLock::new();

pub fn register_fun(path: &str, name: &str, fun: NativeFun) {
    let mut registry = NATIVE_FUN_REGISTRY.get_or_init(|| Mutex::new(HashMap::new())).lock().unwrap();
    registry.insert((path.into(), name.into()), fun);
}

pub fn get_native_fun(path: &str, name: &str) -> Option<NativeFun> {
    let registry = NATIVE_FUN_REGISTRY.get_or_init(|| Mutex::new(HashMap::new())).lock().unwrap();
    registry.get(&(path.into(), name.into())).copied()
}

pub fn register_type(path: &str, name: &str, type_info: TypeInfo) {
    let mut registry = NATIVE_TYPE_REGISTRY.get_or_init(|| Mutex::new(HashMap::new())).lock().unwrap();
    registry.insert((path.into(), name.into()), type_info);
}

pub fn get_native_type(path: &str, name: &str) -> Option<TypeInfo> {
    let registry = NATIVE_TYPE_REGISTRY.get_or_init(|| Mutex::new(HashMap::new())).lock().unwrap();
    registry.get(&(path.into(), name.into())).cloned()
}