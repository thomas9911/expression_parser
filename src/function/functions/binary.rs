use super::{Input, Output, Vars};
use crate::{Expression, ExpressionValue};

pub fn equal(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let string = Expression::eval(lhs, vars)?;
    let other = Expression::eval(rhs, vars)?;
    Ok(string.eq(&other).into())
}

pub fn not_equal(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    use std::ops::Not;

    let string = Expression::eval(lhs, vars)?;
    let other = Expression::eval(rhs, vars)?;
    Ok(string.eq(&other).not().into())
}

pub fn and(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let string: ExpressionValue = Expression::eval(lhs, vars)?;
    let other: ExpressionValue = Expression::eval(rhs, vars)?;
    Ok(string.and(other).into())
}

pub fn or(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let string: ExpressionValue = Expression::eval(lhs, vars)?;
    let other: ExpressionValue = Expression::eval(rhs, vars)?;
    Ok(string.or(other).into())
}
