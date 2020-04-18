use crate::{ExpressionValue, StringExpr, StringVariables};

type Input = StringExpr;
type Vars = StringVariables;
type Output = Option<ExpressionValue>;

pub fn concat(inputs: Vec<Input>, vars: &Vars) -> Output {
    into_value(inputs.iter().try_fold(String::new(), |mut acc, x| {
        acc.push_str(&StringExpr::eval(x.clone(), vars)?.to_string());
        Some(acc)
    }))
}

pub fn trim(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let string = StringExpr::eval(lhs, vars)?.to_string();
    let trim_with = StringExpr::eval(rhs, vars)?.to_string();

    some_string(
        string
            .trim_end_matches(&trim_with)
            .trim_start_matches(&trim_with)
            .to_string(),
    )
}

pub fn contains(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let string = StringExpr::eval(lhs, vars)?.to_string();
    let contains = StringExpr::eval(rhs, vars)?.to_string();
    Some(string.contains(&contains).into())
}

pub fn equal(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let string = StringExpr::eval(lhs, vars)?;
    let other = StringExpr::eval(rhs, vars)?;
    Some(string.eq(&other).into())
}

pub fn not_equal(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    use std::ops::Not;

    let string = StringExpr::eval(lhs, vars)?;
    let other = StringExpr::eval(rhs, vars)?;
    Some(string.eq(&other).not().into())
}

pub fn and(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let string: ExpressionValue = StringExpr::eval(lhs, vars)?;
    let other: ExpressionValue = StringExpr::eval(rhs, vars)?;
    Some(string.and(&other).into())
}

pub fn or(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let string: ExpressionValue = StringExpr::eval(lhs, vars)?;
    let other: ExpressionValue = StringExpr::eval(rhs, vars)?;
    Some(string.or(&other).into())
}

pub fn upper(lhs: Input, vars: &Vars) -> Output {
    some_string(StringExpr::eval(lhs, vars)?.to_string().to_uppercase())
}

fn into_value(result: Option<String>) -> Option<ExpressionValue> {
    some_string(result?)
}

fn some_string(string: String) -> Option<ExpressionValue> {
    Some(ExpressionValue::String(string))
}
