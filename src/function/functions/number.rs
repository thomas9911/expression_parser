use super::{into_number, ok_boolean, ok_number};
use super::{Env, Input, Output};

pub fn lesser<'a, 'b, Vars: Env<'a>>(lhs: Input, rhs: Input, vars: &'b Vars) -> Output {
    ok_boolean(into_number(lhs, vars)? < into_number(rhs, vars)?)
}

pub fn greater<'a, 'b, Vars: Env<'a>>(lhs: Input, rhs: Input, vars: &'b Vars) -> Output {
    ok_boolean(into_number(lhs, vars)? > into_number(rhs, vars)?)
}

pub fn add<'a, 'b, Vars: Env<'a>>(lhs: Input, rhs: Input, vars: &'b Vars) -> Output {
    ok_number(into_number(lhs, vars)? + into_number(rhs, vars)?)
}

pub fn sub<'a, 'b, Vars: Env<'a>>(lhs: Input, rhs: Input, vars: &'b Vars) -> Output {
    ok_number(into_number(lhs, vars)? - into_number(rhs, vars)?)
}

pub fn mul<'a, 'b, Vars: Env<'a>>(lhs: Input, rhs: Input, vars: &'b Vars) -> Output {
    ok_number(into_number(lhs, vars)? * into_number(rhs, vars)?)
}

pub fn div<'a, 'b, Vars: Env<'a>>(lhs: Input, rhs: Input, vars: &'b Vars) -> Output {
    ok_number(into_number(lhs, vars)? / into_number(rhs, vars)?)
}

pub fn pow<'a, 'b, Vars: Env<'a>>(lhs: Input, rhs: Input, vars: &'b Vars) -> Output {
    ok_number(into_number(lhs, vars)?.powf(into_number(rhs, vars)?))
}

pub fn cos<'a, 'b, Vars: Env<'a>>(lhs: Input, vars: &'b Vars) -> Output {
    ok_number(into_number(lhs, vars)?.cos())
}

pub fn sin<'a, 'b, Vars: Env<'a>>(lhs: Input, vars: &'b Vars) -> Output {
    ok_number(into_number(lhs, vars)?.sin())
}

pub fn tan<'a, 'b, Vars: Env<'a>>(lhs: Input, vars: &'b Vars) -> Output {
    ok_number(into_number(lhs, vars)?.tan())
}
