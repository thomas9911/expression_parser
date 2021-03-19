use super::{as_string, ok_string, call};
use super::{Input, Output};
use crate::{Env, Error, Expression, ExpressionValue};

pub fn join<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
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

pub fn get_list<'a, 'b, E: Env<'a>>(list: Vec<Input>, rhs: Input, env: &'b mut E) -> Output {
    match Expression::eval(rhs, env)? {
        ExpressionValue::Number(x) => get_f64(list, x, env),
        _ => Err(Error::new_static(
            "second argument is not a valid index value",
        )),
    }
}

pub fn push<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    let mut list = eval_to_list(lhs, env)?;
    let item = Expression::eval(rhs, env)?;
    list.push(Expression::Value(item));

    Ok(ExpressionValue::List(list))
}

pub fn remove_list<'a, 'b, E: Env<'a>>(lhs: Vec<Input>, rhs: Input, env: &'b mut E) -> Output {
    match Expression::eval(rhs, env)? {
        ExpressionValue::Number(x) => remove_f64(lhs, x),
        _ => Err(Error::new_static(
            "second argument is not a valid index value",
        )),
    }
}

pub fn range<'a, 'b, E: Env<'a>>(lhs: Input, mdl: Input, rhs: Input, env: &'b mut E) -> Output {
    let a = Expression::eval(lhs, env)?
        .as_number()
        .ok_or(Error::new_static("first argument should be a number"))?;
    let b = Expression::eval(mdl, env)?
        .as_number()
        .ok_or(Error::new_static("second argument should be a number"))?;

    let from = float_to_index(a)?;
    let to = float_to_index(b)?;

    let range_object = from..to;

    let data: Vec<_> = match Expression::eval(rhs, env)? {
        ExpressionValue::Function(func, vars) => {
            let function = Expression::Value(ExpressionValue::Function(func, vars));

            range_object
                .into_iter()
                .try_fold(Vec::new(), |mut acc, x| {
                    let arg = Expression::Value(ExpressionValue::Number(x as f64));

                    let res =
                        match call(function.clone(), vec![arg], env) {
                            Ok(x) => {
                                acc.push(Expression::Value(x));
                                Ok(acc)
                            }
                            Err(e) => Err(e),
                        };
                    res
                })?
        }
        ExpressionValue::Number(x) => {
            let step_by = float_to_index(x)?;
            let iter = range_object
                .step_by(step_by)
                .map(|x| Expression::Value(x.into()));

            iter.collect()
        }
        _ => {
            return Err(Error::new_static(
                "last argument should a number or a function",
            ))
        }
    };

    Ok(ExpressionValue::List(data))
}

fn get_f64<'a, 'b, E: Env<'a>>(list: Vec<Expression>, index: f64, env: &'b mut E) -> Output {
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

fn eval_to_list<'a, 'b, E: Env<'a>>(
    input: Input,
    env: &'b mut E,
) -> Result<Vec<Expression>, Error> {
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
    Ok(
        float_to_u32(input).ok_or(Error::new_static("argument is not a valid index value"))?
            as usize,
    )
}

fn is_ok_float(input: f64) -> bool {
    match input.classify() {
        std::num::FpCategory::Normal => true,
        std::num::FpCategory::Zero => true,
        _ => false,
    }
}
