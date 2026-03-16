use smol_str::{SmolStr, format_smolstr};

use crate::{ast::clean, env::TypeEnv, span::Spanned};

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