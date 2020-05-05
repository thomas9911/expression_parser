use crate::{Error, ExpressionValue, StringExpr, StringVariables};

type Input = StringExpr;
type Vars = StringVariables;
type Output = Result<ExpressionValue, Error>;

// fn xd() -> Result<ExpressionValue, Error> {
//     let t: Option<String> = None;

//     let y = t.ok_or(Error::empty());

//     Ok("".into())
// }

pub fn concat(inputs: Vec<Input>, vars: &Vars) -> Output {
    let evaluated_inputs = inputs.iter().try_fold(Vec::new(), |mut acc, x| {
        acc.push(StringExpr::eval(x.clone(), vars)?);
        Ok(acc)
    })?;

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
        ok_string(evaluated_inputs.iter().fold(String::new(), |mut acc, x| {
            acc.push_str(&as_string(x.to_owned()));
            acc
        }))
    }
}

pub fn trim(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let string = as_string(StringExpr::eval(lhs, vars)?);
    let trim_with = as_string(StringExpr::eval(rhs, vars)?);

    ok_string(
        string
            .trim_end_matches(&trim_with)
            .trim_start_matches(&trim_with)
            .to_string(),
    )
}

pub fn contains(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let string = as_string(StringExpr::eval(lhs, vars)?);
    let contains = as_string(StringExpr::eval(rhs, vars)?);
    Ok(string.contains(&contains).into())
}

pub fn equal(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let string = StringExpr::eval(lhs, vars)?;
    let other = StringExpr::eval(rhs, vars)?;
    Ok(string.eq(&other).into())
}

pub fn not_equal(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    use std::ops::Not;

    let string = StringExpr::eval(lhs, vars)?;
    let other = StringExpr::eval(rhs, vars)?;
    Ok(string.eq(&other).not().into())
}

pub fn and(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let string: ExpressionValue = StringExpr::eval(lhs, vars)?;
    let other: ExpressionValue = StringExpr::eval(rhs, vars)?;
    Ok(string.and(&other).into())
}

pub fn or(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    let string: ExpressionValue = StringExpr::eval(lhs, vars)?;
    let other: ExpressionValue = StringExpr::eval(rhs, vars)?;
    Ok(string.or(&other).into())
}

pub fn upper(lhs: Input, vars: &Vars) -> Output {
    ok_string(as_string(StringExpr::eval(lhs, vars)?).to_uppercase())
}

pub fn lower(lhs: Input, vars: &Vars) -> Output {
    ok_string(as_string(StringExpr::eval(lhs, vars)?).to_lowercase())
}

pub fn add(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    ok_number(into_number(lhs, vars)? + into_number(rhs, vars)?)
}

pub fn sub(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    ok_number(into_number(lhs, vars)? - into_number(rhs, vars)?)
}

pub fn mul(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    ok_number(into_number(lhs, vars)? * into_number(rhs, vars)?)
}

pub fn div(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    ok_number(into_number(lhs, vars)? / into_number(rhs, vars)?)
}

pub fn pow(lhs: Input, rhs: Input, vars: &Vars) -> Output {
    ok_number(into_number(lhs, vars)?.powf(into_number(rhs, vars)?))
}

pub fn cos(lhs: Input, vars: &Vars) -> Output {
    ok_number(into_number(lhs, vars)?.cos())
}

pub fn sin(lhs: Input, vars: &Vars) -> Output {
    ok_number(into_number(lhs, vars)?.sin())
}

pub fn tan(lhs: Input, vars: &Vars) -> Output {
    ok_number(into_number(lhs, vars)?.tan())
}

pub fn random(lhs: Input, rhs: Input, vars: &Vars) -> Output {
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

// fn into_value(result: Result<String, Error>) -> Output {
//     ok_string(result?)
// }

fn as_string(val: ExpressionValue) -> String {
    val.to_string().trim_matches('"').to_string()
}

fn ok_string(string: String) -> Output {
    Ok(ExpressionValue::String(string))
}

fn ok_number(number: f64) -> Output {
    Ok(ExpressionValue::Number(number))
}

fn into_number(input: Input, vars: &Vars) -> Result<f64, Error> {
    StringExpr::eval(input, vars)?
        .as_number()
        .ok_or(Error::empty())
}
