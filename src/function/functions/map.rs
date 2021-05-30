use super::{Input, Output};
use crate::{Env, Error, Expression, ExpressionMap, ExpressionValue};

pub fn get_map<'a, 'b, E: Env<'a>>(map: ExpressionMap, rhs: Input, env: &'b mut E) -> Output {
    match Expression::eval(rhs, env)? {
        ExpressionValue::String(x) => {
            if let Some(val) = map.get(&x) {
                Expression::eval(val, env)
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
    if let ExpressionValue::String(ref string_key) = Expression::eval(mdl, env)? {
        map.insert(string_key, Expression::eval(rhs, env)?);
        Ok(map.into())
    } else {
        Err(Error::new_static("second argument should be a string"))
    }
}

pub fn remove_map<'a, 'b, E: Env<'a>>(
    map: ExpressionMap,
    rhs: Input,
    env: &'b mut E,
) -> Output {
    match Expression::eval(rhs, env)? {
        ExpressionValue::String(ref key) => {
            Ok(ExpressionMap::from_hashmap(map.0.without(key)).into())
        }
        _ => Err(Error::new_static("second argument is not a valid key")),
    }
}
