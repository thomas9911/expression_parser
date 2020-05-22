use super::{Input, Output, VariableMap};
use crate::{Expression, ExpressionValue};

pub fn equal<Vars: VariableMap>(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let string = Expression::eval(lhs, vars)?;
    let other = Expression::eval(rhs, vars)?;
    Ok(string.eq(&other).into())
}

pub fn not_equal<Vars: VariableMap>(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    use std::ops::Not;

    let string = Expression::eval(lhs, vars)?;
    let other = Expression::eval(rhs, vars)?;
    Ok(string.eq(&other).not().into())
}

pub fn and<Vars: VariableMap>(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let string: ExpressionValue = Expression::eval(lhs, vars)?;
    let other: ExpressionValue = Expression::eval(rhs, vars)?;
    Ok(string.and(other).into())
}

pub fn or<Vars: VariableMap>(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let string: ExpressionValue = Expression::eval(lhs, vars)?;
    let other: ExpressionValue = Expression::eval(rhs, vars)?;
    Ok(string.or(other).into())
}
