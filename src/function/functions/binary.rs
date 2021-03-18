use super::{Env, Input, Output};
use crate::{Expression, ExpressionValue};

pub fn equal<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, vars: &'b mut E) -> Output {
    let string = Expression::eval(lhs, vars)?;
    let other = Expression::eval(rhs, vars)?;
    Ok(string.eq(&other).into())
}

pub fn not_equal<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, vars: &'b mut E) -> Output {
    use std::ops::Not;

    let string = Expression::eval(lhs, vars)?;
    let other = Expression::eval(rhs, vars)?;
    Ok(string.eq(&other).not().into())
}

pub fn and<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, vars: &'b mut E) -> Output {
    let string: ExpressionValue = Expression::eval(lhs, vars)?;
    let other: ExpressionValue = Expression::eval(rhs, vars)?;
    Ok(string.and(other).into())
}

pub fn or<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, vars: &'b mut E) -> Output {
    let string: ExpressionValue = Expression::eval(lhs, vars)?;
    let other: ExpressionValue = Expression::eval(rhs, vars)?;
    Ok(string.or(other).into())
}
