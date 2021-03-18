use super::{into_number, ok_boolean, ok_number};
use super::{Env, Input, Output};

pub fn lesser<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, vars: &'b mut E) -> Output {
    ok_boolean(into_number(lhs, vars)? < into_number(rhs, vars)?)
}

pub fn greater<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, vars: &'b mut E) -> Output {
    ok_boolean(into_number(lhs, vars)? > into_number(rhs, vars)?)
}

pub fn add<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, vars: &'b mut E) -> Output {
    ok_number(into_number(lhs, vars)? + into_number(rhs, vars)?)
}

pub fn sub<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, vars: &'b mut E) -> Output {
    ok_number(into_number(lhs, vars)? - into_number(rhs, vars)?)
}

pub fn mul<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, vars: &'b mut E) -> Output {
    ok_number(into_number(lhs, vars)? * into_number(rhs, vars)?)
}

pub fn div<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, vars: &'b mut E) -> Output {
    ok_number(into_number(lhs, vars)? / into_number(rhs, vars)?)
}

pub fn pow<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, vars: &'b mut E) -> Output {
    ok_number(into_number(lhs, vars)?.powf(into_number(rhs, vars)?))
}

pub fn cos<'a, 'b, E: Env<'a>>(lhs: Input, vars: &'b mut E) -> Output {
    ok_number(into_number(lhs, vars)?.cos())
}

pub fn sin<'a, 'b, E: Env<'a>>(lhs: Input, vars: &'b mut E) -> Output {
    ok_number(into_number(lhs, vars)?.sin())
}

pub fn tan<'a, 'b, E: Env<'a>>(lhs: Input, vars: &'b mut E) -> Output {
    ok_number(into_number(lhs, vars)?.tan())
}
