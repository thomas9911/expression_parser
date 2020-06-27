use super::{as_string, ok_string};
use super::{Input, Output, VariableMap};
use crate::{Error, Expression, ExpressionValue};

pub fn join<Vars: VariableMap>(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let list = eval_to_list(lhs, vars)?;
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

pub fn get<Vars: VariableMap>(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let list = eval_to_list(lhs, vars)?;
    match Expression::eval(rhs, vars)? {
        ExpressionValue::Number(x) => get_f64(list, x, vars),
        _ => Err(Error::new_static(
            "second argument is not a valid index value",
        )),
    }
}

pub fn push<Vars: VariableMap>(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let mut list = eval_to_list(lhs, vars)?;
    let item = Expression::eval(rhs, vars)?;
    list.push(Expression::Value(item));

    Ok(ExpressionValue::List(list))
}

pub fn remove<Vars: VariableMap>(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let list = eval_to_list(lhs, vars)?;
    match Expression::eval(rhs, vars)? {
        ExpressionValue::Number(x) => remove_f64(list, x),
        _ => Err(Error::new_static(
            "second argument is not a valid index value",
        )),
    }
}

fn get_f64<Vars: VariableMap>(list: Vec<Expression>, index: f64, vars: &Vars) -> Output {
    if index < 0.0 {
        let index = float_to_index(-index)?;
        let item = get_back_int(list, index)?;
        Expression::eval(item, vars)
    } else {
        let index = float_to_index(index)?;
        let item = get_int(list, index)?;
        Expression::eval(item, vars)
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

fn eval_to_list<Vars: VariableMap>(input: Input, vars: &Vars) -> Result<Vec<Expression>, Error> {
    Expression::eval(input, vars)?
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
