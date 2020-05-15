use super::{as_string, ok_string};
use super::{Input, Output, Vars};
use crate::Expression;

pub fn trim(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let string = as_string(Expression::eval(lhs, vars)?);
    let trim_with = as_string(Expression::eval(rhs, vars)?);

    ok_string(
        string
            .trim_end_matches(&trim_with)
            .trim_start_matches(&trim_with)
            .to_string(),
    )
}

pub fn contains(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let string = as_string(Expression::eval(lhs, vars)?);
    let contains = as_string(Expression::eval(rhs, vars)?);
    Ok(string.contains(&contains).into())
}

pub fn upper(lhs: Input, vars: &Vars) -> Output {
    ok_string(as_string(Expression::eval(lhs, vars)?).to_uppercase())
}

pub fn lower(lhs: Input, vars: &Vars) -> Output {
    ok_string(as_string(Expression::eval(lhs, vars)?).to_lowercase())
}
