use super::{ok_boolean, Env, Input, Output};
use crate::{Expression, ExpressionValue};

pub fn equal<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    let string = Expression::eval_rc(lhs, env)?;
    let other = Expression::eval_rc(rhs, env)?;
    ok_boolean(string.eq(&other))
}

pub fn not_equal<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    use std::ops::Not;

    let string = Expression::eval_rc(lhs, env)?;
    let other = Expression::eval_rc(rhs, env)?;
    ok_boolean(string.eq(&other).not())
}

pub fn and<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    let string = Expression::eval_rc(lhs, env)?;
    let other = Expression::eval_rc(rhs, env)?;
    Ok(string.and(other))
}

pub fn or<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    let string = Expression::eval_rc(lhs, env)?;
    let other = Expression::eval_rc(rhs, env)?;
    Ok(string.or(other))
}
