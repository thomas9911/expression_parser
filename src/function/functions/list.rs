use super::{as_string, ok_string};
use super::{Input, Output, VariableMap};
use crate::{Error, Expression, ExpressionValue};

pub fn join<Vars: VariableMap>(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let list = Expression::eval(lhs, vars)?
        .as_list()
        .ok_or(Error::new_static("first argument is not a list"))?;
    let join_with = Expression::eval(rhs, vars)?
        .as_string()
        .ok_or(Error::new_static("second argument is not a string"))?;

    let string_list = list
        .into_iter()
        .try_fold(Vec::new(), |mut acc, x| -> Result<Vec<String>, Error> {
            let value: ExpressionValue = Expression::eval(x, vars)?;
            acc.push(as_string(value));
            Ok(acc)
        })
        .or(Err(Error::new_static("first argument is not a valid list")))?;

    ok_string(string_list.join(&join_with))
}
