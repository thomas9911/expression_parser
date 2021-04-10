use super::{as_string, ok_boolean, ok_string};
use super::{Env, Input, Output};
use crate::Expression;

pub fn trim<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    let string = as_string(Expression::eval_rc(lhs, env)?);
    let trim_with = as_string(Expression::eval_rc(rhs, env)?);

    ok_string(
        string
            .trim_end_matches(&trim_with)
            .trim_start_matches(&trim_with)
            .to_string(),
    )
}

pub fn contains<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    let string = as_string(Expression::eval_rc(lhs, env)?);
    let contains = as_string(Expression::eval_rc(rhs, env)?);
    ok_boolean(string.contains(&contains))
}

pub fn upper<'a, 'b, E: Env<'a>>(lhs: Input, env: &'b mut E) -> Output {
    ok_string(as_string(Expression::eval_rc(lhs, env)?).to_uppercase())
}

pub fn lower<'a, 'b, E: Env<'a>>(lhs: Input, env: &'b mut E) -> Output {
    ok_string(as_string(Expression::eval_rc(lhs, env)?).to_lowercase())
}
