use super::{as_string, call, ok_string};
use super::{Input, Output};
use crate::{Env, Error, Expression, ExpressionValue};
use std::sync::Arc;

pub type ExpressionItem = Result<Arc<Expression>, Error>;

pub fn join<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    let list = eval_to_list(lhs, env)?;
    let join_with = Expression::eval_rc(rhs, env)?
        .as_string()
        .ok_or(Error::new_static("second argument is not a string"))?;

    let string_list = list
        .into_iter()
        .try_fold(Vec::new(), |mut acc, x| -> Result<Vec<String>, Error> {
            let value = Expression::eval_rc(x, env)?;
            acc.push(as_string(value));
            Ok(acc)
        })
        .or(Err(Error::new_static("first argument is not a valid list")))?;

    ok_string(string_list.join(&join_with))
}

pub fn get_list<'a, 'b, E: Env<'a>>(list: Vec<Input>, rhs: Input, env: &'b mut E) -> Output {
    match *Expression::eval_rc(rhs, env)? {
        ExpressionValue::Number(x) => get_f64(list, x, env),
        _ => Err(Error::new_static(
            "second argument is not a valid index value",
        )),
    }
}

pub fn push<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    let mut list = eval_to_list(lhs, env)?;
    let item = Expression::eval_rc(rhs, env)?;
    list.push(Arc::new(Expression::Value(item)));

    Ok(ExpressionValue::List(list).into())
}

pub fn put_list<'a, 'b, E: Env<'a>>(
    list: Vec<Input>,
    mdl: Input,
    rhs: Input,
    env: &'b mut E,
) -> Output {
    let value = Expression::eval_rc(rhs, env)?;

    // match Expression::eval_rc(mdl, env)? {
    //     ExpressionValue::Number(x) => put_f64(list, x, Expression::Value(value), env),
    //     _ => Err(Error::new_static(
    //         "second argument is not a valid index value",
    //     )),
    // }
    Expression::eval_rc(mdl, env)?
        .as_number()
        .map(|x| put_f64(list, x, Expression::Value(value), env))
        .ok_or(Error::new_static(
            "second argument is not a valid index value",
        ))?
}

pub fn remove_list<'a, 'b, E: Env<'a>>(lhs: Vec<Input>, rhs: Input, env: &'b mut E) -> Output {
    match *Expression::eval_rc(rhs, env)? {
        ExpressionValue::Number(x) => remove_f64(lhs, x),
        _ => Err(Error::new_static(
            "second argument is not a valid index value",
        )),
    }
}

pub fn range<'a, 'b, E: Env<'a>>(lhs: Input, mdl: Input, rhs: Input, env: &'b mut E) -> Output {
    let a = Expression::eval_rc(lhs, env)?
        .as_number()
        .ok_or(Error::new_static("first argument should be a number"))?;
    let b = Expression::eval_rc(mdl, env)?
        .as_number()
        .ok_or(Error::new_static("second argument should be a number"))?;

    let from = float_to_index(a)?;
    let to = float_to_index(b)?;

    let range_object = from..to;

    let evaluated = Expression::eval_rc(rhs, env)?;
    let data: Vec<_> = match &*evaluated {
        ExpressionValue::Function(_, _) => {
            let function = Arc::new(Expression::Value(evaluated.clone()));

            range_object
                .into_iter()
                .try_fold::<_, _, Result<_, Error>>(Vec::new(), |mut acc, x| {
                    let arg = Arc::new(Expression::Value(ExpressionValue::Number(x as f64).into()));

                    let val = call(function.clone(), vec![arg], env)?;
                    acc.push(Arc::new(Expression::Value(val)));
                    Ok(acc)
                })?
        }
        ExpressionValue::Number(x) => {
            let step_by = float_to_index(*x)?;
            let iter = range_object
                .step_by(step_by)
                .map(|x| Arc::new(Expression::Value(Arc::new(x.into()))));

            iter.collect()
        }
        _ => {
            return Err(Error::new_static(
                "last argument should a number or a function",
            ))
        }
    };

    Ok(ExpressionValue::List(data).into())
}

pub fn reduce<'a, 'b, E: Env<'a>>(lhs: Input, mdl: Input, rhs: Input, env: &'b mut E) -> Output {
    let list = Expression::eval_rc(lhs, env)?
        .as_list()
        .ok_or(Error::new_static("first argument should be a list"))?;

    let mut accumilator = Expression::eval_rc(mdl, env)?;

    let evaluated = Expression::eval_rc(rhs, env)?;

    match &*evaluated {
        ExpressionValue::Function(_, _) => {
            let function = Arc::new(Expression::Value(evaluated));

            for item in list {
                let args = vec![Arc::new(Expression::Value(accumilator)), item];
                accumilator = call(function.clone(), args, env)?;
            }

            Ok(accumilator)
        }
        _ => Err(Error::new_static("last argument should a function")),
    }
    // let evaluated = Expression::eval_rc(rhs, env)?;
    // evaluated
    //     .clone()
    //     .is_function()
    //     .map(|(func, vars)| {
    //         let function = Arc::new(Expression::Value(evaluated));

    //         for item in list {
    //             let args = vec![Arc::new(Expression::Value(accumilator)), item];
    //             accumilator = call(function.clone(), args, env)?;
    //         }

    //         Ok(accumilator)
    //     })
    //     .ok_or(Error::new_static("last argument should a function"))?
}

pub fn shuffle<'a, 'b, E: Env<'a>>(lhs: Input, env: &'b mut E) -> Output {
    let mut list = Expression::eval_rc(lhs, env)?
        .as_list()
        .ok_or(Error::new_static("first argument should be a list"))?;

    use rand::seq::SliceRandom;
    use rand::thread_rng;

    let mut rng = thread_rng();
    list.shuffle(&mut rng);

    Ok(ExpressionValue::List(list).into())
}

fn get_f64<'a, 'b, E: Env<'a>>(list: Vec<Input>, index: f64, env: &'b mut E) -> Output {
    if index < 0.0 {
        let index = float_to_index(-index)?;
        let item = get_back_int(list, index)?;
        Expression::eval_rc(item, env)
    } else {
        let index = float_to_index(index)?;
        let item = get_int(list, index)?;
        Expression::eval_rc(item, env)
    }
}

fn put_f64<'a, 'b, E: Env<'a>>(
    list: Vec<Input>,
    index: f64,
    item: Expression,
    env: &'b mut E,
) -> Output {
    if index < 0.0 {
        let index = float_to_index(-index)?;
        let item = put_back_int(list, index, item)?;
        Expression::eval_rc(item, env)
    } else {
        let index = float_to_index(index)?;
        let item = put_int(list, index, item)?;
        Expression::eval_rc(item, env)
    }
}

fn get_int(list: Vec<Input>, index: usize) -> ExpressionItem {
    match list.get(index) {
        Some(x) => Ok(x.clone()),
        None => Err(Error::new(format!("item not found on index '{}'", index))),
    }
}

fn get_back_int(list: Vec<Input>, index: usize) -> ExpressionItem {
    let new_index = from_right_index(&list, index)?;
    match list.get(new_index) {
        Some(x) => Ok(x.clone()),
        None => Err(Error::new_static("item not found on index below zero")),
    }
}

fn put_int(mut list: Vec<Input>, index: usize, item: Expression) -> ExpressionItem {
    match list.get_mut(index) {
        Some(x) => {
            let t = Arc::make_mut(x);
            *t = item;
            Ok(Arc::new(ExpressionValue::List(list).into()))
        }
        None => Err(Error::new(format!("item not found on index '{}'", index))),
    }
}

fn put_back_int(list: Vec<Input>, index: usize, item: Expression) -> ExpressionItem {
    let new_index = from_right_index(&list, index)?;
    put_int(list, new_index, item)
}

fn remove_f64(mut list: Vec<Input>, index: f64) -> Output {
    if index < 0.0 {
        let index = float_to_index(-index)?;
        remove_back_int(&mut list, index)?;
        Ok(ExpressionValue::List(list).into())
    } else {
        let index = float_to_index(index)?;
        remove_int(&mut list, index)?;
        Ok(ExpressionValue::List(list).into())
    }
}

fn remove_int(list: &mut Vec<Input>, index: usize) -> Result<(), Error> {
    if index > list.len() {
        Err(Error::new(format!("item not found on index '{}'", index)))
    } else {
        list.remove(index);
        Ok(())
    }
}

fn remove_back_int(list: &mut Vec<Input>, index: usize) -> Result<(), Error> {
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
) -> Result<Vec<Arc<Expression>>, Error> {
    Expression::eval_rc(input, env)?
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

fn from_right_index(list: &Vec<Input>, index: usize) -> Result<usize, Error> {
    list.len()
        .checked_sub(index)
        .ok_or(Error::new_static("item not found on index below zero"))
}
