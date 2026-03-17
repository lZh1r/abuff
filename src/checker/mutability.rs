use smol_str::{SmolStr, format_smolstr};

use crate::{ast::{clean, typed::FunctionParam}, env::TypeEnv, span::{Spanned, spanned}};

pub fn check_variable_mutability(expr: &Spanned<clean::Expr>, env: &TypeEnv) -> Result<bool, Spanned<SmolStr>> {
    match &expr.inner {
        clean::Expr::Var(var) => {
            Ok(env.get_var_type(var).unwrap().1)
        },
        clean::Expr::Index(target, _) => {
            check_variable_mutability(&*target, env)
        },
        clean::Expr::Get(target, _) => {
            check_variable_mutability(&*target, env)
        },
        t => Err(Spanned {
            inner: format_smolstr!("{t:?} is not a variable"),
            span: expr.span
        })
    }
}

pub fn check_param_mutability(param: &FunctionParam, arg_expr: &Spanned<clean::Expr>, env: &TypeEnv) -> Result<(), Spanned<SmolStr>> {
    match check_variable_mutability(arg_expr, env) {
        Ok(is_mutable) => {
            if param.is_mutable && !is_mutable {
                return Err(
                    spanned(
                        "Cannot pass an immutable reference as a mutable argument".into(),
                        arg_expr.span
                    )
                )
            }
        },
        Err(_) => {
            if param.is_mutable {
                return Err(
                    spanned(
                        "Cannot pass a literal expression as a mutable argument".into(),
                        arg_expr.span
                    )
                )
            }
        },
    };
    Ok(())
}