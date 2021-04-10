use super::{Input, Output};
use crate::{Env, Error, Expression, ExpressionMap, ExpressionValue};
use std::sync::Arc;

pub fn get_map<'a, 'b, E: Env<'a>>(map: ExpressionMap, rhs: Input, env: &'b mut E) -> Output {
    match &*Expression::eval_rc(rhs, env)? {
        ExpressionValue::String(x) => {
            if let Some(val) = map.get(x) {
                Expression::eval_rc(val, env)
            } else {
                Err(Error::new(format!("key '{}' is not found in map", x)))
            }
        }
        _ => Err(Error::new_static("second argument is not a valid key")),
    }
}

pub fn put_map<'a, 'b, E: Env<'a>>(
    mut map: ExpressionMap,
    mdl: Input,
    rhs: Input,
    env: &'b mut E,
) -> Output {
    if let ExpressionValue::String(ref string_key) = *Expression::eval_rc(mdl, env)? {
        map.insert_arc(string_key, Expression::eval_rc(rhs, env)?);
        Ok(Arc::new(map.into()))
    } else {
        Err(Error::new_static("second argument should be a string"))
    }
}

pub fn remove_map<'a, 'b, E: Env<'a>>(
    mut map: ExpressionMap,
    rhs: Input,
    env: &'b mut E,
) -> Output {
    match *Expression::eval_rc(rhs, env)? {
        ExpressionValue::String(ref key) => {
            map.remove(key);
            Ok(Arc::new(map.into()))
        }
        _ => Err(Error::new_static("second argument is not a valid key")),
    }
}
