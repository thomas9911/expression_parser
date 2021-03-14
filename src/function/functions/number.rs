use super::{into_number, ok_boolean, ok_number};
use super::{Input, Output, VariableMap};
use crate::Environment;

pub fn lesser(lhs: Input, rhs: Input, env: &Environment) -> Output {
    ok_boolean(into_number(lhs, env)? < into_number(rhs, env)?)
}

pub fn greater(lhs: Input, rhs: Input, env: &Environment) -> Output {
    ok_boolean(into_number(lhs, env)? > into_number(rhs, env)?)
}

pub fn add(lhs: Input, rhs: Input, env: &Environment) -> Output {
    ok_number(into_number(lhs, env)? + into_number(rhs, env)?)
}

pub fn sub(lhs: Input, rhs: Input, env: &Environment) -> Output {
    ok_number(into_number(lhs, env)? - into_number(rhs, env)?)
}

pub fn mul(lhs: Input, rhs: Input, env: &Environment) -> Output {
    ok_number(into_number(lhs, env)? * into_number(rhs, env)?)
}

pub fn div(lhs: Input, rhs: Input, env: &Environment) -> Output {
    ok_number(into_number(lhs, env)? / into_number(rhs, env)?)
}

pub fn pow(lhs: Input, rhs: Input, env: &Environment) -> Output {
    ok_number(into_number(lhs, env)?.powf(into_number(rhs, env)?))
}

pub fn cos(lhs: Input, env: &Environment) -> Output {
    ok_number(into_number(lhs, env)?.cos())
}

pub fn sin(lhs: Input, env: &Environment) -> Output {
    ok_number(into_number(lhs, env)?.sin())
}

pub fn tan(lhs: Input, env: &Environment) -> Output {
    ok_number(into_number(lhs, env)?.tan())
}
