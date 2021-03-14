use super::{Input, Output, VariableMap};
use crate::{Environment, Expression, ExpressionValue};

pub fn equal(lhs: Input, rhs: Input, env: &Environment) -> Output {
    let string = Expression::eval(lhs, env)?;
    let other = Expression::eval(rhs, env)?;
    Ok(string.eq(&other).into())
}

pub fn not_equal(lhs: Input, rhs: Input, env: &Environment) -> Output {
    use std::ops::Not;

    let string = Expression::eval(lhs, env)?;
    let other = Expression::eval(rhs, env)?;
    Ok(string.eq(&other).not().into())
}

pub fn and(lhs: Input, rhs: Input, env: &Environment) -> Output {
    let string: ExpressionValue = Expression::eval(lhs, env)?;
    let other: ExpressionValue = Expression::eval(rhs, env)?;
    Ok(string.and(other).into())
}

pub fn or(lhs: Input, rhs: Input, env: &Environment) -> Output {
    let string: ExpressionValue = Expression::eval(lhs, env)?;
    let other: ExpressionValue = Expression::eval(rhs, env)?;
    Ok(string.or(other).into())
}
