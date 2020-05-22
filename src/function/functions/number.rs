use super::{into_number, ok_number};
use super::{Input, Output};
use crate::VariableMap;

pub fn add(lhs: Input, rhs: Input, vars: &impl VariableMap) -> Output {
    ok_number(into_number(lhs, vars)? + into_number(rhs, vars)?)
}

pub fn sub(lhs: Input, rhs: Input, vars: &impl VariableMap) -> Output {
    ok_number(into_number(lhs, vars)? - into_number(rhs, vars)?)
}

pub fn mul(lhs: Input, rhs: Input, vars: &impl VariableMap) -> Output {
    ok_number(into_number(lhs, vars)? * into_number(rhs, vars)?)
}

pub fn div(lhs: Input, rhs: Input, vars: &impl VariableMap) -> Output {
    ok_number(into_number(lhs, vars)? / into_number(rhs, vars)?)
}

pub fn pow(lhs: Input, rhs: Input, vars: &impl VariableMap) -> Output {
    ok_number(into_number(lhs, vars)?.powf(into_number(rhs, vars)?))
}

pub fn cos(lhs: Input, vars: &impl VariableMap) -> Output {
    ok_number(into_number(lhs, vars)?.cos())
}

pub fn sin(lhs: Input, vars: &impl VariableMap) -> Output {
    ok_number(into_number(lhs, vars)?.sin())
}

pub fn tan(lhs: Input, vars: &impl VariableMap) -> Output {
    ok_number(into_number(lhs, vars)?.tan())
}
