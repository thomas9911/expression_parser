use super::FunctionName;
use crate::{
    Closure, Env, Environment, Error, Expression, ExpressionFile, ExpressionValue, Function,
    ScopedVariables, UserFunction, Variables,
};

use std::collections::HashMap;

pub type Input = Expression;
pub type Output = Result<ExpressionValue, Error>;

mod binary;
pub(crate) mod format;
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

pub fn if_function<'a, 'b, E: Env<'a>>(
    lhs: Input,
    mdl: Input,
    rhs: Input,
    env: &'b mut E,
) -> Output {
    let condition = Expression::eval(lhs, env)?;

    let res = if condition.is_truthy() {
        Expression::eval(mdl, env)?
    } else {
        Expression::eval(rhs, env)?
    };

    evaluate_lazy_function(res, env)
}

pub fn now<'a, 'b, E: Env<'a>>(_env: &'b mut E) -> Output {
    use std::time::SystemTime;

    let now = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap();

    Ok(now.as_secs_f64().into())
}

pub fn random<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    use rand::distributions::IndependentSample;

    let a = into_number(lhs, env)?;
    let b = into_number(rhs, env)?;

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

pub fn print<'a, 'b, E: Env<'a>>(lhs: Input, env: &'b mut E) -> Output {
    let value = Expression::eval(lhs, env)?;
    println!("{}", value);
    Ok(value)
}

pub fn try_function<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    match Expression::eval(lhs, env) {
        Ok(x) => Ok(x),
        Err(_) => Expression::eval(rhs, env),
    }
}

pub fn help<'a, 'b, E: Env<'a>>(lhs: Input, _env: &'b mut E) -> Output {
    match &lhs {
        x if x == &Expression::default() => ok_string(Function::help()),
        _ => normal_help(lhs),
    }
}

pub fn call<'a, 'b, E: Env<'a>>(func: Input, list: Vec<Input>, env: &'b mut E) -> Output {
    let list = evaluate_inputs(list, env)?;

    let logger = env.logger();
    let var = ScopedVariables::new(env.variables());

    let mut context = Environment {
        // original: Box::new(env),
        variables: Box::new(var),
        logger: logger,
    };

    {
        match Expression::eval(func, &mut context)? {
            ExpressionValue::ExternalFunction(closure) => {
                call_external_function(closure, list, &mut context)
            }
            ExpressionValue::Function(function, compiled_vars) => {
                call_function(function, compiled_vars, list, &mut context)
            }
            _ => Err(Error::new_static("input should be a function")),
        }
    }
}

pub fn call_function<'a, 'b>(
    user_func: UserFunction,
    compiled_vars: Variables,
    args: Vec<ExpressionValue>,
    context: &'b mut Environment<'a>,
) -> Output {
    for (key, value) in compiled_vars.into_iter() {
        context.variables_mut().insert(&key, value);
    }

    for (key, value) in user_func.arguments.iter().zip(args) {
        context.variables_mut().insert(key, value);
    }

    let result = ExpressionFile::eval(user_func.expression, context)?;

    Ok(result)
}

pub fn call_external_function<'a, 'b>(
    closure: Closure,
    args: Vec<ExpressionValue>,
    context: &'b mut Environment<'a>,
) -> Output {
    let f = closure.function;
    f(args, context)
}

pub fn format<'a, 'b, E: Env<'a>>(lhs: Input, list: Vec<Input>, env: &'b mut E) -> Output {
    let template = Expression::eval(lhs, env)?
        .as_string()
        .ok_or(Error::new_static("template is not a string"))?;

    let args = evaluate_inputs(list, env)?
        .into_iter()
        .map(|x| as_string(x))
        .collect();

    let result = format::format(&template, args)?;
    Ok(result.into())
}

pub fn type_function<'a, 'b, E: Env<'a>>(lhs: Input, env: &'b mut E) -> Output {
    let value = Expression::eval(lhs, env)?;
    Ok(value.what_type().into())
}

pub fn error<'a, 'b, E: Env<'a>>(lhs: Input, env: &'b mut E) -> Output {
    let value = Expression::eval(lhs, env)?;
    Err(Error::new(value.to_string()))
}

pub fn assert<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    let value = Expression::eval(lhs, env)?;

    if value.is_truthy() {
        Ok(value)
    } else {
        let error_message = Expression::eval(rhs, env)?
            .as_string()
            .ok_or(Error::new_static("error message is not a string"))?;
        Err(Error::new(error_message))
    }
}

/// overload get function for list and map
pub fn get<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    let value = Expression::eval(lhs, env)?;
    match value {
        ExpressionValue::List(val) => get_list(val, rhs, env),
        ExpressionValue::Map(val) => get_map(val, rhs, env),
        _ => Err(Error::new_static("first argument is not a list or a map")),
    }
}

/// overload remove function for list and map
pub fn remove<'a, 'b, E: Env<'a>>(lhs: Input, rhs: Input, env: &'b mut E) -> Output {
    let value = Expression::eval(lhs, env)?;
    match value {
        ExpressionValue::List(val) => remove_list(val, rhs, env),
        ExpressionValue::Map(val) => remove_map(val, rhs, env),
        _ => Err(Error::new_static("first argument is not a list or a map")),
    }
}

fn normal_help(lhs: Input) -> Output {
    use std::str::FromStr;

    let value = as_variable_or_string(lhs)?;
    match value.as_ref() {
        "functions" => {
            let d: Vec<_> = Function::iter_without_infixes()
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
    let mut usage_text: String = usage_lines
        .map(|x| {
            let mut x = String::from(x);
            x.push_str("\n");
            x
        })
        .collect();

    if usage_text.is_empty() {
        usage_text = String::from("\n\n");
    }

    format!("{}\n\nUsage:\n```{}```", help_text, usage_text)
}

// fn into_value(result: Result<String, Error>) -> Output {
//     ok_string(result?)
// }

pub(crate) fn as_string(val: ExpressionValue) -> String {
    let mut res = val.to_string();
    if res.starts_with('"') && res.ends_with('"') {
        res.remove(0);
        res.pop();
    }
    res
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

pub(crate) fn ok_boolean(boolean: bool) -> Output {
    Ok(ExpressionValue::Bool(boolean))
}

pub(crate) fn into_number<'a, 'b, E: Env<'a>>(input: Input, env: &'b mut E) -> Result<f64, Error> {
    Expression::eval(input, env)?
        .as_number()
        .ok_or(Error::new_static("input should be a number"))
}

pub(crate) fn evaluate_inputs<'a, 'b, E: Env<'a>>(
    inputs: Vec<Input>,
    env: &'b mut E,
) -> Result<Vec<ExpressionValue>, Error> {
    inputs.into_iter().try_fold(Vec::new(), |mut acc, x| {
        acc.push(Expression::eval(x, env)?);
        Ok(acc)
    })
}

pub(crate) fn evaluate_lazy_function<'a, 'b, E: Env<'a>>(
    result: ExpressionValue,
    env: &'b mut E,
) -> Output {
    match result {
        // if function doesn't take arguments evaluate it now
        ExpressionValue::Function(
            UserFunction {
                arguments,
                expression,
            },
            local_vars,
        ) if arguments.len() == 0 => {
            let val = ExpressionValue::Function(
                UserFunction {
                    arguments,
                    expression,
                },
                local_vars,
            );
            call(val.into(), Vec::new(), env)
        }
        ExpressionValue::Function(func, local_vars) => {
            Ok(ExpressionValue::Function(func, local_vars))
        }
        ExpressionValue::ExternalFunction(Closure {
            function,
            input_variables,
        }) if input_variables.len() == 0 => {
            let val = ExpressionValue::ExternalFunction(Closure {
                function,
                input_variables,
            });
            call(val.into(), Vec::new(), env)
        }
        ExpressionValue::ExternalFunction(closure) => {
            Ok(ExpressionValue::ExternalFunction(closure))
        }
        val => Ok(val),
    }
}
