use super::{as_string, ok_string};
use super::{Env, Input, Output};
use crate::Expression;

pub fn trim<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, vars: &'b mut E) -> Output {
    let string = as_string(Expression::eval(lhs, vars)?);
    let trim_with = as_string(Expression::eval(rhs, vars)?);

    ok_string(
        string
            .trim_end_matches(&trim_with)
            .trim_start_matches(&trim_with)
            .to_string(),
    )
}

pub fn contains<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, vars: &'b mut E) -> Output {
    let string = as_string(Expression::eval(lhs, vars)?);
    let contains = as_string(Expression::eval(rhs, vars)?);
    Ok(string.contains(&contains).into())
}

pub fn upper<'a, 'b, E: Env<'a>>(lhs: Input, vars: &'b mut E) -> Output {
    ok_string(as_string(Expression::eval(lhs, vars)?).to_uppercase())
}

pub fn lower<'a, 'b, E: Env<'a>>(lhs: Input, vars: &'b mut E) -> Output {
    ok_string(as_string(Expression::eval(lhs, vars)?).to_lowercase())
}
