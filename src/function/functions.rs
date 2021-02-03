use super::FunctionName;
use crate::{Error, Expression, ExpressionValue, Function, VariableMap};

pub type Input = Expression;
pub type Output = Result<ExpressionValue, Error>;

mod binary;
mod list;
mod many;
mod map;
mod number;
mod string;
pub use binary::*;
pub use list::*;
pub use many::*;
pub use map::*;
pub use number::*;
pub use string::*;

pub fn if_function<Vars: VariableMap>(lhs: Input, mdl: Input, rhs: Input, vars: &Vars) -> Output {
    let condition = Expression::eval(lhs, vars)?;
    if condition.is_truthy() {
        Expression::eval(mdl, vars)
    } else {
        Expression::eval(rhs, vars)
    }
}

pub fn now<Vars: VariableMap>(_vars: &Vars) -> Output {
    use std::time::SystemTime;

    let now = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap();

    Ok(now.as_secs_f64().into())
}

pub fn random<Vars: VariableMap>(lhs: Input, rhs: Input, vars: &Vars) -> Output {
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

pub fn print<Vars: VariableMap>(lhs: Input, vars: &Vars) -> Output {
    let value = Expression::eval(lhs, vars)?;
    println!("{}", value);
    Ok(value)
}

pub fn help<Vars: VariableMap>(lhs: Input, _vars: &Vars) -> Output {
    match &lhs {
        x if x == &Expression::default() => ok_string(Function::help()),
        _ => normal_help(lhs),
    }
}

/// overload get function for list and map
pub fn get<Vars: VariableMap>(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let value = Expression::eval(lhs, vars)?;
    match value {
        ExpressionValue::List(val) => get_list(val, rhs, vars),
        ExpressionValue::Map(val) => get_map(val, rhs, vars),
        _ => Err(Error::new_static("first argument is not a list or a map")),
    }
}

/// overload remove function for list and map
pub fn remove<Vars: VariableMap>(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let value = Expression::eval(lhs, vars)?;
    match value {
        ExpressionValue::List(val) => remove_list(val, rhs, vars),
        ExpressionValue::Map(val) => remove_map(val, rhs, vars),
        _ => Err(Error::new_static("first argument is not a list or a map")),
    }
}

fn normal_help(lhs: Input) -> Output {
    use std::str::FromStr;
    use strum::IntoEnumIterator;

    // let value = match Expression::eval(lhs.clone(), vars){
    //     Ok(x) => Expression::Value(x),
    //     Err(_e) => lhs,
    // };

    let value = as_variable_or_string(lhs)?;
    match value.as_ref() {
        "functions" => {
            let d: Vec<_> = Function::iter()
                .map(|x| {
                    Expression::Value(ExpressionValue::String(FunctionName::from(x).to_string()))
                })
                .collect();
            return Ok(ExpressionValue::List(d));
        }
        _ => (),
    };
    match Function::from_str(&value) {
        Err(_e) => return Err(Error::new(format!("'{}' is not a function", value))),
        Ok(x) => ok_string(print_function_help(x)),
    }
}

fn print_function_help(func: Function) -> String {
    use strum::EnumMessage;
    let help_text = func.get_message().unwrap_or_default();
    let mut usage_lines = func.get_usage().lines();
    usage_lines.next_back();
    let usage_text: String = usage_lines
        .map(|x| {
            let mut x = String::from(x);
            x.push_str("\n");
            x
        })
        .collect();

    format!("{}\nUsage:\n{}", help_text, usage_text)
}

// fn into_value(result: Result<String, Error>) -> Output {
//     ok_string(result?)
// }

pub(crate) fn as_string(val: ExpressionValue) -> String {
    val.to_string().trim_matches('"').to_string()
}

pub(crate) fn as_variable_or_string(expr: Expression) -> Result<String, Error> {
    match expr {
        Expression::Value(x) => Ok(as_string(x)),
        Expression::Var(x) => Ok(x),
        _ => Err(Error::new(format!("cannot call help on '{}'", expr))),
    }
}

pub(crate) fn ok_string(string: String) -> Output {
    Ok(ExpressionValue::String(string))
}

pub(crate) fn ok_number(number: f64) -> Output {
    Ok(ExpressionValue::Number(number))
}

pub(crate) fn into_number<Vars: VariableMap>(input: Input, vars: &Vars) -> Result<f64, Error> {
    Expression::eval(input, vars)?
        .as_number()
        .ok_or(Error::new_static("input should be a number"))
}

pub(crate) fn evaluate_inputs<Vars: VariableMap>(
    inputs: Vec<Input>,
    vars: &Vars,
) -> Result<Vec<ExpressionValue>, Error> {
    inputs.into_iter().try_fold(Vec::new(), |mut acc, x| {
        acc.push(Expression::eval(x, vars)?);
        Ok(acc)
    })
}
