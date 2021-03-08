use super::{into_number, ok_boolean, ok_number};
use super::{Input, Output, VariableMap};

pub fn lesser<Vars: VariableMap>(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    ok_boolean(into_number(lhs, vars)? < into_number(rhs, vars)?)
}

pub fn greater<Vars: VariableMap>(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    ok_boolean(into_number(lhs, vars)? > into_number(rhs, vars)?)
}

pub fn add<Vars: VariableMap>(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    ok_number(into_number(lhs, vars)? + into_number(rhs, vars)?)
}

pub fn sub<Vars: VariableMap>(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    ok_number(into_number(lhs, vars)? - into_number(rhs, vars)?)
}

pub fn mul<Vars: VariableMap>(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    ok_number(into_number(lhs, vars)? * into_number(rhs, vars)?)
}

pub fn div<Vars: VariableMap>(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    ok_number(into_number(lhs, vars)? / into_number(rhs, vars)?)
}

pub fn pow<Vars: VariableMap>(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    ok_number(into_number(lhs, vars)?.powf(into_number(rhs, vars)?))
}

pub fn cos<Vars: VariableMap>(lhs: Input, vars: &Vars) -> Output {
    ok_number(into_number(lhs, vars)?.cos())
}

pub fn sin<Vars: VariableMap>(lhs: Input, vars: &Vars) -> Output {
    ok_number(into_number(lhs, vars)?.sin())
}

pub fn tan<Vars: VariableMap>(lhs: Input, vars: &Vars) -> Output {
    ok_number(into_number(lhs, vars)?.tan())
}
