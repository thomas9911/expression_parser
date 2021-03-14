use super::{as_string, ok_string};
use super::{Input, Output, VariableMap};
use crate::{Environment, Expression};

pub fn trim(lhs: Input, rhs: Input, env: &Environment) -> Output {
    let string = as_string(Expression::eval(lhs, env)?);
    let trim_with = as_string(Expression::eval(rhs, env)?);

    ok_string(
        string
            .trim_end_matches(&trim_with)
            .trim_start_matches(&trim_with)
            .to_string(),
    )
}

pub fn contains(lhs: Input, rhs: Input, env: &Environment) -> Output {
    let string = as_string(Expression::eval(lhs, env)?);
    let contains = as_string(Expression::eval(rhs, env)?);
    Ok(string.contains(&contains).into())
}

pub fn upper(lhs: Input, env: &Environment) -> Output {
    ok_string(as_string(Expression::eval(lhs, env)?).to_uppercase())
}

pub fn lower(lhs: Input, env: &Environment) -> Output {
    ok_string(as_string(Expression::eval(lhs, env)?).to_lowercase())
}
