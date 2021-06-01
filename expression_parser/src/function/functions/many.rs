use super::{as_string, evaluate_inputs, ok_string};
use super::{Env, Input, Output};
use crate::{Error, ExpressionValue};
use im::Vector;
use std::iter::FromIterator;

pub fn sum<'a, 'b, E: Env<'a>>(inputs: Vec<Input>, env: &'b mut E) -> Output {
    let evaluated_inputs = evaluate_inputs(inputs, env)?;
    if evaluated_inputs.iter().all(|x| x.is_number_or_boolean()) {
        return Ok(evaluated_inputs
            .iter()
            .map(|x| x.as_number_or_boolean().expect("values should be numbers"))
            .sum::<f64>()
            .into());
    }
    if evaluated_inputs.len() == 1 {
        if let Some(list) = evaluated_inputs[0].as_list() {
            return sum(Vec::from_iter(list.into_iter()), env);
        }
    }
    Err(Error::new_static("sum contains non number inputs"))
}

pub fn product<'a, 'b, E: Env<'a>>(inputs: Vec<Input>, env: &'b mut E) -> Output {
    let evaluated_inputs = evaluate_inputs(inputs, env)?;
    if evaluated_inputs.iter().all(|x| x.is_number_or_boolean()) {
        return Ok(evaluated_inputs
            .iter()
            .map(|x| x.as_number_or_boolean().expect("values should be numbers"))
            .product::<f64>()
            .into());
    }
    if evaluated_inputs.len() == 1 {
        if let Some(list) = evaluated_inputs[0].as_list() {
            return product(Vec::from_iter(list.into_iter()), env);
        }
    }
    Err(Error::new_static("product contains non number inputs"))
}

pub fn all<'a, 'b, E: Env<'a>>(inputs: Vec<Input>, env: &'b mut E) -> Output {
    let evaluated_inputs = evaluate_inputs(inputs, env)?;
    if evaluated_inputs.len() == 1 {
        if let Some(list) = evaluated_inputs[0].as_list() {
            return all(Vec::from_iter(list.into_iter()), env);
        }
    }
    Ok(evaluated_inputs.iter().all(|x| x.is_truthy()).into())
}

pub fn any<'a, 'b, E: Env<'a>>(inputs: Vec<Input>, env: &'b mut E) -> Output {
    let evaluated_inputs = evaluate_inputs(inputs, env)?;
    if evaluated_inputs.len() == 1 {
        if let Some(list) = evaluated_inputs[0].as_list() {
            return any(Vec::from_iter(list.into_iter()), env);
        }
    }
    Ok(evaluated_inputs.iter().any(|x| x.is_truthy()).into())
}

pub fn concat<'a, 'b, E: Env<'a>>(inputs: Vec<Input>, env: &'b mut E) -> Output {
    let evaluated_inputs = evaluate_inputs(inputs, env)?;

    if evaluated_inputs.iter().all(|x| x.is_list()) {
        match evaluated_inputs
            .iter()
            .try_fold(Vector::new(), |mut acc, x| {
                let list = x.as_list()?;
                acc.append(list);
                Some(acc)
            }) {
            Some(x) => Ok(ExpressionValue::List(x)),
            None => Err(Error::empty()),
        }
    } else {
        ok_string(
            evaluated_inputs
                .into_iter()
                .fold(String::new(), |mut acc, x| {
                    acc.push_str(&as_string(x));
                    acc
                }),
        )
    }
}
