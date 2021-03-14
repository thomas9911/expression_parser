use super::{as_string, ok_string};
use super::{Input, Output, VariableMap};
use crate::{Environment, Error, Expression, ExpressionValue};

pub fn join(lhs: Input, rhs: Input, env: &Environment) -> Output {
    let list = eval_to_list(lhs, env)?;
    let join_with = Expression::eval(rhs, env)?
        .as_string()
        .ok_or(Error::new_static("second argument is not a string"))?;

    let string_list = list
        .into_iter()
        .try_fold(Vec::new(), |mut acc, x| -> Result<Vec<String>, Error> {
            let value: ExpressionValue = Expression::eval(x, env)?;
            acc.push(as_string(value));
            Ok(acc)
        })
        .or(Err(Error::new_static("first argument is not a valid list")))?;

    ok_string(string_list.join(&join_with))
}

pub fn get_list(list: Vec<Input>, rhs: Input, env: &Environment) -> Output {
    match Expression::eval(rhs, env)? {
        ExpressionValue::Number(x) => get_f64(list, x, env),
        _ => Err(Error::new_static(
            "second argument is not a valid index value",
        )),
    }
}

pub fn push(lhs: Input, rhs: Input, env: &Environment) -> Output {
    let mut list = eval_to_list(lhs, env)?;
    let item = Expression::eval(rhs, env)?;
    list.push(Expression::Value(item));

    Ok(ExpressionValue::List(list))
}

pub fn remove_list(lhs: Vec<Input>, rhs: Input, env: &Environment) -> Output {
    match Expression::eval(rhs, env)? {
        ExpressionValue::Number(x) => remove_f64(lhs, x),
        _ => Err(Error::new_static(
            "second argument is not a valid index value",
        )),
    }
}

fn get_f64(list: Vec<Expression>, index: f64, env: &Environment) -> Output {
    if index < 0.0 {
        let index = float_to_index(-index)?;
        let item = get_back_int(list, index)?;
        Expression::eval(item, env)
    } else {
        let index = float_to_index(index)?;
        let item = get_int(list, index)?;
        Expression::eval(item, env)
    }
}

fn get_int(list: Vec<Expression>, index: usize) -> Result<Expression, Error> {
    match list.get(index) {
        Some(x) => Ok(x.clone()),
        None => Err(Error::new(format!("item not found on index '{}'", index))),
    }
}

fn get_back_int(list: Vec<Expression>, index: usize) -> Result<Expression, Error> {
    match list.get(list.len() - index) {
        Some(x) => Ok(x.clone()),
        None => Err(Error::new_static("item not found on index below zero")),
    }
}

fn remove_f64(mut list: Vec<Expression>, index: f64) -> Output {
    if index < 0.0 {
        let index = float_to_index(-index)?;
        remove_back_int(&mut list, index)?;
        Ok(ExpressionValue::List(list))
    } else {
        let index = float_to_index(index)?;
        remove_int(&mut list, index)?;
        Ok(ExpressionValue::List(list))
    }
}

fn remove_int(list: &mut Vec<Expression>, index: usize) -> Result<(), Error> {
    if index > list.len() {
        Err(Error::new(format!("item not found on index '{}'", index)))
    } else {
        list.remove(index);
        Ok(())
    }
}

fn remove_back_int(list: &mut Vec<Expression>, index: usize) -> Result<(), Error> {
    if let Some(x) = list.len().checked_sub(index) {
        list.remove(x);
        Ok(())
    } else {
        Err(Error::new_static("item not found on index below zero"))
    }
}

fn eval_to_list(input: Input, env: &Environment) -> Result<Vec<Expression>, Error> {
    Expression::eval(input, env)?
        .as_list()
        .ok_or(Error::new_static("first argument is not a list"))
}

fn float_to_u32(input: f64) -> Option<u32> {
    if is_ok_float(input) && (input.is_sign_positive()) && !(input.fract() > 0.0) {
        Some(input as u32)
    } else {
        None
    }
}

fn float_to_index(input: f64) -> Result<usize, Error> {
    Ok(float_to_u32(input).ok_or(Error::new_static(
        "second argument is not a valid index value",
    ))? as usize)
}

fn is_ok_float(input: f64) -> bool {
    match input.classify() {
        std::num::FpCategory::Normal => true,
        std::num::FpCategory::Zero => true,
        _ => false,
    }
}
