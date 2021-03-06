use super::{as_string, call, ok_string};
use super::{Input, Output};
use crate::{Env, Error, Expression, ExpressionValue};
use im::Vector;

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

pub fn get_list<'a, 'b, E: Env<'a>>(list: Vector<Input>, rhs: Input, env: &'b mut E) -> Output {
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
    list.push_back(Expression::Value(item));

    Ok(ExpressionValue::List(list))
}

pub fn put_list<'a, 'b, E: Env<'a>>(
    list: Vector<Input>,
    mdl: Input,
    rhs: Input,
    env: &'b mut E,
) -> Output {
    let value = Expression::eval(rhs, env)?;

    match Expression::eval(mdl, env)? {
        ExpressionValue::Number(x) => put_f64(list, x, Expression::Value(value), env),
        _ => Err(Error::new_static(
            "second argument is not a valid index value",
        )),
    }
}

pub fn remove_list<'a, 'b, E: Env<'a>>(lhs: Vector<Input>, rhs: Input, env: &'b mut E) -> Output {
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

    let data: Vector<_> = match Expression::eval(rhs, env)? {
        ExpressionValue::Function(func, vars) => {
            let function = Expression::Value(ExpressionValue::Function(func, vars));

            range_object
                .into_iter()
                .try_fold(Vector::new(), |mut acc, x| {
                    let arg = Expression::Value(ExpressionValue::Number(x as f64));

                    let res = match call(function.clone(), vec![arg], env) {
                        Ok(x) => {
                            acc.push_back(Expression::Value(x));
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

pub fn reduce<'a, 'b, E: Env<'a>>(lhs: Input, mdl: Input, rhs: Input, env: &'b mut E) -> Output {
    let list = Expression::eval(lhs, env)?
        .as_list()
        .ok_or(Error::new_static("first argument should be a list"))?;

    let mut accumilator = Expression::eval(mdl, env)?;

    match Expression::eval(rhs, env)? {
        ExpressionValue::Function(func, vars) => {
            let function = Expression::Value(ExpressionValue::Function(func, vars));

            for item in list {
                let args = vec![Expression::Value(accumilator), item];
                accumilator = call(function.clone(), args, env)?;
            }

            Ok(accumilator)
        }
        _ => Err(Error::new_static("last argument should a function")),
    }
}

pub fn shuffle<'a, 'b, E: Env<'a>>(lhs: Input, env: &'b mut E) -> Output {
    let mut list: Vec<_> = Expression::eval(lhs, env)?
        .as_list()
        .ok_or(Error::new_static("first argument should be a list"))?
        .into_iter()
        .collect();

    use rand::seq::SliceRandom;
    use rand::thread_rng;

    let mut rng = thread_rng();
    list.shuffle(&mut rng);

    Ok(ExpressionValue::List(Vector::from(list)))
}

fn get_f64<'a, 'b, E: Env<'a>>(list: Vector<Expression>, index: f64, env: &'b mut E) -> Output {
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

fn put_f64<'a, 'b, E: Env<'a>>(
    list: Vector<Expression>,
    index: f64,
    item: Expression,
    env: &'b mut E,
) -> Output {
    if index < 0.0 {
        let index = float_to_index(-index)?;
        let item = put_back_int(list, index, item)?;
        Expression::eval(item, env)
    } else {
        let index = float_to_index(index)?;
        let item = put_int(list, index, item)?;
        Expression::eval(item, env)
    }
}

fn get_int(list: Vector<Expression>, index: usize) -> Result<Expression, Error> {
    match list.get(index) {
        Some(x) => Ok(x.clone()),
        None => Err(Error::new(format!("item not found on index '{}'", index))),
    }
}

fn get_back_int(list: Vector<Expression>, index: usize) -> Result<Expression, Error> {
    let new_index = from_right_index(&list, index)?;
    match list.get(new_index) {
        Some(x) => Ok(x.clone()),
        None => Err(Error::new_static("item not found on index below zero")),
    }
}

fn put_int(
    mut list: Vector<Expression>,
    index: usize,
    item: Expression,
) -> Result<Expression, Error> {
    match list.get_mut(index) {
        Some(x) => {
            *x = item;
            Ok(ExpressionValue::List(list).into())
        }
        None => Err(Error::new(format!("item not found on index '{}'", index))),
    }
}

fn put_back_int(
    list: Vector<Expression>,
    index: usize,
    item: Expression,
) -> Result<Expression, Error> {
    let new_index = from_right_index(&list, index)?;
    put_int(list, new_index, item)
}

fn remove_f64(mut list: Vector<Expression>, index: f64) -> Output {
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

fn remove_int(list: &mut Vector<Expression>, index: usize) -> Result<(), Error> {
    if index > list.len() {
        Err(Error::new(format!("item not found on index '{}'", index)))
    } else {
        list.remove(index);
        Ok(())
    }
}

fn remove_back_int(list: &mut Vector<Expression>, index: usize) -> Result<(), Error> {
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
) -> Result<Vector<Expression>, Error> {
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

fn from_right_index(list: &Vector<Expression>, index: usize) -> Result<usize, Error> {
    list.len()
        .checked_sub(index)
        .ok_or(Error::new_static("item not found on index below zero"))
}
