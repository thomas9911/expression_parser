use super::{into_number, ok_boolean, ok_number};
use super::{Env, Input, Output};

pub fn lesser<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    ok_boolean(into_number(lhs, env)? < into_number(rhs, env)?)
}

pub fn greater<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    ok_boolean(into_number(lhs, env)? > into_number(rhs, env)?)
}

pub fn add<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    ok_number(into_number(lhs, env)? + into_number(rhs, env)?)
}

pub fn sub<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    ok_number(into_number(lhs, env)? - into_number(rhs, env)?)
}

pub fn mul<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    ok_number(into_number(lhs, env)? * into_number(rhs, env)?)
}

pub fn div<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    ok_number(into_number(lhs, env)? / into_number(rhs, env)?)
}

pub fn pow<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    ok_number(into_number(lhs, env)?.powf(into_number(rhs, env)?))
}

pub fn cos<'a, 'b, E: Env<'a>>(lhs: Input, env: &'b mut E) -> Output {
    ok_number(into_number(lhs, env)?.cos())
}

pub fn sin<'a, 'b, E: Env<'a>>(lhs: Input, env: &'b mut E) -> Output {
    ok_number(into_number(lhs, env)?.sin())
}

pub fn tan<'a, 'b, E: Env<'a>>(lhs: Input, env: &'b mut E) -> Output {
    ok_number(into_number(lhs, env)?.tan())
}
