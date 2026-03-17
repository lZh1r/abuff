use crate::checker::flatten::flatten_type;
use smol_str::SmolStr;

use crate::{ast::typed::FunctionParam, env::TypeEnv, span::Spanned};

pub fn get_flat_params(
    params: &Vec<FunctionParam>,
    env: &mut TypeEnv,
    populate_vars: bool
) -> Result<Vec<FunctionParam>, Spanned<SmolStr>> {
    let mut flat_params = Vec::new();
    for param in params {
        let flattened_type = flatten_type(&param.type_info, env)?.into_owned();
        if populate_vars {
            env.add_var_type(param.name.clone(), flattened_type.clone(), param.is_mutable);
        }
        flat_params.push(FunctionParam {
            type_info: flattened_type,
            ..param.clone()
        } )
    }
    Ok(flat_params)
}