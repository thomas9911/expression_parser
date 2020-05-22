use super::{as_string, evaluate_inputs, ok_string};
use super::{Input, Output};
use crate::{Error, ExpressionValue, VariableMap};

pub fn sum(inputs: Vec<Input>, vars: &impl VariableMap) -> Output {
    let evaluated_inputs = evaluate_inputs(inputs, vars)?;
    if evaluated_inputs.iter().all(|x| x.is_number_or_boolean()) {
        Ok(evaluated_inputs
            .iter()
            .map(|x| x.as_number_or_boolean().expect("values should be numbers"))
            .sum::<f64>()
            .into())
    } else {
        Err(Error::new_static("sum contains non number inputs"))
    }
}

pub fn product(inputs: Vec<Input>, vars: &impl VariableMap) -> Output {
    let evaluated_inputs = evaluate_inputs(inputs, vars)?;
    if evaluated_inputs.iter().all(|x| x.is_number_or_boolean()) {
        Ok(evaluated_inputs
            .iter()
            .map(|x| x.as_number_or_boolean().expect("values should be numbers"))
            .product::<f64>()
            .into())
    } else {
        Err(Error::new_static("sum contains non number inputs"))
    }
}

pub fn all(inputs: Vec<Input>, vars: &impl VariableMap) -> Output {
    let evaluated_inputs = evaluate_inputs(inputs, vars)?;
    Ok(evaluated_inputs.iter().all(|x| x.is_truthy()).into())
}

pub fn any(inputs: Vec<Input>, vars: &impl VariableMap) -> Output {
    let evaluated_inputs = evaluate_inputs(inputs, vars)?;
    Ok(evaluated_inputs.iter().any(|x| x.is_truthy()).into())
}

pub fn concat(inputs: Vec<Input>, vars: &impl VariableMap) -> Output {
    let evaluated_inputs = evaluate_inputs(inputs, vars)?;

    if evaluated_inputs.iter().all(|x| x.is_list()) {
        match evaluated_inputs.iter().try_fold(Vec::new(), |mut acc, x| {
            let mut list = x.as_list()?;
            acc.append(&mut list);
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
