use crate::{Error, Expression, ExpressionValue, Variables};

pub type Input = Expression;
pub type Vars = Variables;
pub type Output = Result<ExpressionValue, Error>;

mod binary;
mod many;
mod number;
mod string;
pub use binary::*;
pub use many::*;
pub use number::*;
pub use string::*;

pub fn if_function(lhs: Input, mdl: Input, rhs: Input, vars: &Vars) -> Output {
    let condition = Expression::eval(lhs, vars)?;
    if condition.is_truthy() {
        Expression::eval(mdl, vars)
    } else {
        Expression::eval(rhs, vars)
    }
}

pub fn random(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    use rand::distributions::IndependentSample;

    let a = into_number(lhs, vars)?;
    let b = into_number(rhs, vars)?;

    let value = if a == b {
        a
    } else {
        let (c, d) = if a >= b { (b, a) } else { (a, b) };
        let between = rand::distributions::range::Range::new(c, d);
        let mut rng = rand::thread_rng();
        between.ind_sample(&mut rng)
    };

    ok_number(value)
}

// fn into_value(result: Result<String, Error>) -> Output {
//     ok_string(result?)
// }

pub(crate) fn as_string(val: ExpressionValue) -> String {
    val.to_string().trim_matches('"').to_string()
}

pub(crate) fn ok_string(string: String) -> Output {
    Ok(ExpressionValue::String(string))
}

pub(crate) fn ok_number(number: f64) -> Output {
    Ok(ExpressionValue::Number(number))
}

pub(crate) fn into_number(input: Input, vars: &Vars) -> Result<f64, Error> {
    Expression::eval(input, vars)?
        .as_number()
        .ok_or(Error::new_static("input should be a number"))
}

pub(crate) fn evaluate_inputs(
    inputs: Vec<Input>,
    vars: &Vars,
) -> Result<Vec<ExpressionValue>, Error> {
    inputs.into_iter().try_fold(Vec::new(), |mut acc, x| {
        acc.push(Expression::eval(x, vars)?);
        Ok(acc)
    })
}
