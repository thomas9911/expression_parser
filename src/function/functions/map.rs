use super::{Input, Output, VariableMap};
use crate::{Env, Error, Expression, ExpressionMap, ExpressionValue};

pub fn get_map<'a, 'b, Vars: Env<'a>>(map: ExpressionMap, rhs: Input, vars: &'b Vars) -> Output {
    match Expression::eval(rhs, vars)? {
        ExpressionValue::String(x) => {
            if let Some(val) = map.get(&x) {
                Expression::eval(val, vars)
            } else {
                Err(Error::new(format!("key '{}' is not found in map", x)))
            }
        }
        _ => Err(Error::new_static("second argument is not a valid key")),
    }
}

pub fn put<'a, 'b, Vars: Env<'a>>(lhs: Input, mdl: Input, rhs: Input, vars: &'b Vars) -> Output {
    if let ExpressionValue::String(ref string_key) = Expression::eval(mdl, vars)? {
        let mut map = eval_to_map(lhs, vars)?;
        map.insert(string_key, Expression::eval(rhs, vars)?);
        Ok(map.into())
    } else {
        Err(Error::new_static("second argument should be a string"))
    }
}

pub fn remove_map<'a, 'b, Vars: Env<'a>>(
    mut map: ExpressionMap,
    rhs: Input,
    vars: &'b Vars,
) -> Output {
    match Expression::eval(rhs, vars)? {
        ExpressionValue::String(ref key) => {
            map.remove(key);
            Ok(map.into())
        }
        _ => Err(Error::new_static("second argument is not a valid key")),
    }
}

fn eval_to_map<'a, 'b, Vars: Env<'a>>(
    input: Input,
    vars: &'b Vars,
) -> Result<ExpressionMap, Error> {
    Expression::eval(input, vars)?
        .as_map()
        .ok_or(Error::new_static("first argument should be a map"))
}
