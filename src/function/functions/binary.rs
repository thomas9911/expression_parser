use super::{Env, Input, Output};
use crate::{Expression, ExpressionValue};

pub fn equal<'a, 'b, Vars: Env<'a>>(lhs: Input, rhs: Input, vars: &'b Vars) -> Output {
    let string = Expression::eval(lhs, vars)?;
    let other = Expression::eval(rhs, vars)?;
    Ok(string.eq(&other).into())
}

pub fn not_equal<'a, 'b, Vars: Env<'a>>(lhs: Input, rhs: Input, vars: &'b Vars) -> Output {
    use std::ops::Not;

    let string = Expression::eval(lhs, vars)?;
    let other = Expression::eval(rhs, vars)?;
    Ok(string.eq(&other).not().into())
}

pub fn and<'a, 'b, Vars: Env<'a>>(lhs: Input, rhs: Input, vars: &'b Vars) -> Output {
    let string: ExpressionValue = Expression::eval(lhs, vars)?;
    let other: ExpressionValue = Expression::eval(rhs, vars)?;
    Ok(string.and(other).into())
}

pub fn or<'a, 'b, Vars: Env<'a>>(lhs: Input, rhs: Input, vars: &'b Vars) -> Output {
    let string: ExpressionValue = Expression::eval(lhs, vars)?;
    let other: ExpressionValue = Expression::eval(rhs, vars)?;
    Ok(string.or(other).into())
}
