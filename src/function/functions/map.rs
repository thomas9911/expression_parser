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

pub fn put<'a, 'b, E: Env<'a>>(lhs: Input, mdl: Input, rhs: Input, env: &'b mut E) -> Output {
    if let ExpressionValue::String(ref string_key) = Expression::eval(mdl, env)? {
        let mut map = eval_to_map(lhs, env)?;
        map.insert(string_key, Expression::eval(rhs, env)?);
        Ok(map.into())
    } else {
        Err(Error::new_static("second argument should be a string"))
    }
}

pub fn remove_map<'a, 'b, E: Env<'a>>(
    mut map: ExpressionMap,
    rhs: Input,
    env: &'b mut E,
) -> Output {
    match Expression::eval(rhs, env)? {
        ExpressionValue::String(ref key) => {
            map.remove(key);
            Ok(map.into())
        }
        _ => Err(Error::new_static("second argument is not a valid key")),
    }
}

fn eval_to_map<'a, 'b, E: Env<'a>>(input: Input, env: &'b mut E) -> Result<ExpressionMap, Error> {
    Expression::eval(input, env)?
        .as_map()
        .ok_or(Error::new_static("first argument should be a map"))
}
