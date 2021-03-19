use super::{Env, Input, Output};
use crate::{Expression, ExpressionValue};

pub fn equal<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    let string = Expression::eval(lhs, env)?;
    let other = Expression::eval(rhs, env)?;
    Ok(string.eq(&other).into())
}

pub fn not_equal<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    use std::ops::Not;

    let string = Expression::eval(lhs, env)?;
    let other = Expression::eval(rhs, env)?;
    Ok(string.eq(&other).not().into())
}

pub fn and<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    let string: ExpressionValue = Expression::eval(lhs, env)?;
    let other: ExpressionValue = Expression::eval(rhs, env)?;
    Ok(string.and(other).into())
}

pub fn or<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    let string: ExpressionValue = Expression::eval(lhs, env)?;
    let other: ExpressionValue = Expression::eval(rhs, env)?;
    Ok(string.or(other).into())
}
