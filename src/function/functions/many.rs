use super::{as_string, evaluate_inputs, ok_string};
use super::{Input, Output, VariableMap};
use crate::{Error, ExpressionValue};

pub fn sum<Vars: VariableMap>(inputs: Vec<Input>, vars: &Vars) -> Output {
    let evaluated_inputs = evaluate_inputs(inputs, vars)?;
    if evaluated_inputs.iter().all(|x| x.is_number_or_boolean()) {
        return Ok(evaluated_inputs
            .iter()
            .map(|x| x.as_number_or_boolean().expect("values should be numbers"))
            .sum::<f64>()
            .into());
    }
    if evaluated_inputs.len() == 1 {
        if let Some(list) = evaluated_inputs[0].as_list() {
            return sum(list, vars);
        }
    }
    Err(Error::new_static("sum contains non number inputs"))
}

pub fn product<Vars: VariableMap>(inputs: Vec<Input>, vars: &Vars) -> Output {
    let evaluated_inputs = evaluate_inputs(inputs, vars)?;
    if evaluated_inputs.iter().all(|x| x.is_number_or_boolean()) {
        return Ok(evaluated_inputs
            .iter()
            .map(|x| x.as_number_or_boolean().expect("values should be numbers"))
            .product::<f64>()
            .into());
    }
    if evaluated_inputs.len() == 1 {
        if let Some(list) = evaluated_inputs[0].as_list() {
            return product(list, vars);
        }
    }
    Err(Error::new_static("product contains non number inputs"))
}

pub fn all<Vars: VariableMap>(inputs: Vec<Input>, vars: &Vars) -> Output {
    let evaluated_inputs = evaluate_inputs(inputs, vars)?;
    if evaluated_inputs.len() == 1 {
        if let Some(list) = evaluated_inputs[0].as_list() {
            return all(list, vars);
        }
    }
    Ok(evaluated_inputs.iter().all(|x| x.is_truthy()).into())
}

pub fn any<Vars: VariableMap>(inputs: Vec<Input>, vars: &Vars) -> Output {
    let evaluated_inputs = evaluate_inputs(inputs, vars)?;
    if evaluated_inputs.len() == 1 {
        if let Some(list) = evaluated_inputs[0].as_list() {
            return any(list, vars);
        }
    }
    Ok(evaluated_inputs.iter().any(|x| x.is_truthy()).into())
}

pub fn concat<Vars: VariableMap>(inputs: Vec<Input>, vars: &Vars) -> Output {
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
