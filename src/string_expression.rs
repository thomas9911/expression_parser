use std::collections::{BTreeMap, HashMap};

use pest::error::{Error as PestError, ErrorVariant};
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use pest::{Parser, Span};

use crate::Error;

mod functions;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[derive(Parser)]
#[grammar = "string_expression.pest"]
struct StringExpressionParser;

type EvalResult = Result<ExpressionValue, Error>;

lazy_static! {
    static ref DEFAULT_VARIABLES: HashMap<String, ExpressionValue> = {
        let mut m = HashMap::new();
        m.insert(String::from("true"), ExpressionValue::Bool(true));
        m.insert(String::from("false"), ExpressionValue::Bool(false));
        m.insert(
            String::from("e"),
            ExpressionValue::Number(std::f64::consts::E),
        );
        m.insert(
            String::from("pi"),
            ExpressionValue::Number(std::f64::consts::PI),
        );
        m.insert(
            String::from("tau"),
            ExpressionValue::Number(2.0 * std::f64::consts::PI),
        );
        m
    };
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Assoc::*;
        use Rule::*;

        PrecClimber::new(vec![
            Operator::new(concat_op, Left),
            Operator::new(equal, Right),
            Operator::new(not_equal, Right),
            Operator::new(and, Left),
            Operator::new(or, Left),
            Operator::new(add, Left) | Operator::new(subtract, Left),
            Operator::new(multiply, Left) | Operator::new(divide, Left),
            Operator::new(power, Right),
        ])
    };
}

// mostly copied from pest calculator example
fn parse_expression(expression: Pairs<Rule>) -> Result<StringExpr, PestError<Rule>> {
    use StringExpr::*;

    PREC_CLIMBER.climb(
        expression,
        |pair: Pair<Rule>| match pair.as_rule() {
            Rule::num => Ok(Value(pair.as_str().parse::<f64>().unwrap().into())),
            Rule::string => Ok(Value(ExpressionValue::String(
                pair.as_str().trim_matches('"').to_string(),
            ))),
            Rule::list => {
                let arguments: Vec<StringExpr> =
                    pair.into_inner().try_fold(Vec::new(), |mut acc, x| {
                        let p = parse_expression(x.into_inner())?;
                        acc.push(p);
                        Ok(acc)
                    })?;
                Ok(Value(ExpressionValue::List(arguments)))
            }
            Rule::expr => parse_expression(pair.into_inner()),
            Rule::var => Ok(Var(pair.as_str().trim().to_string())),
            Rule::func => make_function(pair),
            rule => {
                println!("{:?}", rule);
                unreachable!()
            }
        },
        |lhs: Result<StringExpr, PestError<Rule>>,
         op: Pair<Rule>,
         rhs: Result<StringExpr, PestError<Rule>>| {
            let lhs = lhs.unwrap();
            let rhs = rhs.unwrap();

            match op.as_rule() {
                Rule::concat_op => Ok(Expr(Box::new(Functions::Concat(vec![lhs, rhs])))),
                Rule::equal => Ok(Expr(Box::new(Functions::Equal(lhs, rhs)))),
                Rule::not_equal => Ok(Expr(Box::new(Functions::NotEqual(lhs, rhs)))),
                Rule::and => Ok(Expr(Box::new(Functions::And(lhs, rhs)))),
                Rule::or => Ok(Expr(Box::new(Functions::Or(lhs, rhs)))),
                Rule::add => Ok(Expr(Box::new(Functions::Add(lhs, rhs)))),
                Rule::subtract => Ok(Expr(Box::new(Functions::Sub(lhs, rhs)))),
                Rule::multiply => Ok(Expr(Box::new(Functions::Mul(lhs, rhs)))),
                Rule::divide => Ok(Expr(Box::new(Functions::Div(lhs, rhs)))),
                Rule::power => Ok(Expr(Box::new(Functions::Pow(lhs, rhs)))),
                // _ => unreachable!(),
                rule => {
                    println!("{:?}", rule);
                    unreachable!()
                }
            }
        },
    )
}

fn make_function(pair: Pair<Rule>) -> Result<StringExpr, PestError<Rule>> {
    use StringExpr::Expr;

    let pair_span = pair.as_span();
    let mut inner_pairs = pair.into_inner();
    let function_name = inner_pairs.next().expect("function always has a name");

    let arguments: Vec<StringExpr> = inner_pairs.try_fold(Vec::new(), |mut acc, x| {
        acc.push(parse_expression(x.into_inner())?);
        Ok(acc)
    })?;
    let func_name = function_name.as_str();

    Ok(match func_name {
        "upper" => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Box::new(Functions::Upper(arguments[0].clone())))
        }
        "lower" => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Box::new(Functions::Lower(arguments[0].clone())))
        }
        "cos" => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Box::new(Functions::Cos(arguments[0].clone())))
        }
        "sin" => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Box::new(Functions::Sin(arguments[0].clone())))
        }
        "tan" => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Box::new(Functions::Tan(arguments[0].clone())))
        }
        "trim" => {
            check_arguments(func_name, pair_span, 2, Some(2), &arguments)?;
            Expr(Box::new(Functions::Trim(
                arguments[0].clone(),
                arguments[1].clone(),
            )))
        }
        "contains" => {
            check_arguments(func_name, pair_span, 2, Some(2), &arguments)?;
            Expr(Box::new(Functions::Contains(
                arguments[0].clone(),
                arguments[1].clone(),
            )))
        }
        "concat" => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            Expr(Box::new(Functions::Concat(arguments)))
        }
        _ => unreachable!(),
    })
}

fn check_arguments(
    function_name: &str,
    span: Span,
    min_args: usize,
    max_args: Option<usize>,
    args: &Vec<StringExpr>,
) -> Result<(), PestError<Rule>> {
    if min_args > args.len() {
        let variant = ErrorVariant::<Rule>::CustomError {
            message: format!(
                "function {:?} should have more than {} arguments",
                function_name, min_args
            ),
        };
        return Err(PestError::new_from_span(variant, span));
    };

    if let Some(max_len) = max_args {
        if args.len() > max_len {
            let variant = ErrorVariant::<Rule>::CustomError {
                message: format!(
                    "function {:?} should have less than {} arguments",
                    function_name, max_len
                ),
            };
            return Err(PestError::new_from_span(variant, span));
        };
    };

    Ok(())
}

#[derive(Debug, Clone)]
pub struct StringVariables {
    state: HashMap<String, ExpressionValue>,
}

impl StringVariables {
    pub fn get(&self, key: &str) -> Option<&ExpressionValue> {
        self.state.get(key)
    }

    pub fn insert<V>(&mut self, key: &str, value: V) -> Option<ExpressionValue>
    where
        V: Into<ExpressionValue>,
    {
        self.state.insert(String::from(key), value.into())
    }

    pub fn from_iter<T: IntoIterator<Item = (String, ExpressionValue)>>(
        iter: T,
    ) -> StringVariables {
        let mut variables = DEFAULT_VARIABLES.to_owned();
        variables.extend(iter);

        StringVariables { state: variables }
    }
}

impl std::default::Default for StringVariables {
    fn default() -> StringVariables {
        StringVariables {
            state: DEFAULT_VARIABLES.to_owned(),
        }
    }
}

impl From<HashMap<String, ExpressionValue>> for StringVariables {
    fn from(state: HashMap<String, ExpressionValue>) -> StringVariables {
        StringVariables::from_iter(state.into_iter())
    }
}

impl From<BTreeMap<String, ExpressionValue>> for StringVariables {
    fn from(state: BTreeMap<String, ExpressionValue>) -> StringVariables {
        StringVariables::from_iter(state.into_iter())
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum StringExpr {
    Value(ExpressionValue),
    Expr(Box<Functions>),
    Var(String),
}

impl StringExpr {
    /// evaluate the syntax tree with given variables and returns a 'ExpressionValue'
    pub fn eval(expression: StringExpr, vars: &StringVariables) -> EvalResult {
        match expression {
            StringExpr::Expr(op) => Functions::eval(*op, vars),
            StringExpr::Value(value) => match value {
                ExpressionValue::List(list) => Ok(ExpressionValue::List(list.iter().try_fold(
                    Vec::new(),
                    |mut acc, x| {
                        acc.push(StringExpr::Value(StringExpr::eval(x.clone(), vars)?));
                        Ok(acc)
                    },
                )?)),
                x => Ok(x),
            },
            // StringExpr::Var(s) => vars.get(&s).clone(),
            StringExpr::Var(s) => match vars.get(&s) {
                Some(x) => Ok(x.clone()),
                None => Err(Error::new(format!("Variable {} not found", &s))),
            },
        }
    }

    /// Takes input and returns a syntax tree
    pub fn parse(input: &str) -> Result<StringExpr, PestError<Rule>> {
        parse_expression(StringExpressionParser::parse(Rule::calculation, input)?)
    }

    /// returns all variables defined in the expression
    pub fn iter_variables<'a>(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        match self {
            StringExpr::Value(_) => Box::new(std::iter::empty::<String>()),
            StringExpr::Var(x) => Box::new(std::iter::once(x.to_owned())),
            StringExpr::Expr(f) => f.iter_variables(),
        }
    }

    /// returns variables that don't have a default value, so these values are still not known
    pub fn iter_variables_without_defaults<'a>(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        Box::new(
            self.iter_variables()
                .filter(|x| !DEFAULT_VARIABLES.contains_key(x)),
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Functions {
    Concat(Vec<StringExpr>),
    Equal(StringExpr, StringExpr),
    NotEqual(StringExpr, StringExpr),
    And(StringExpr, StringExpr),
    Or(StringExpr, StringExpr),
    Trim(StringExpr, StringExpr),
    Contains(StringExpr, StringExpr),
    Upper(StringExpr),
    Lower(StringExpr),
    Add(StringExpr, StringExpr),
    Sub(StringExpr, StringExpr),
    Mul(StringExpr, StringExpr),
    Div(StringExpr, StringExpr),
    Pow(StringExpr, StringExpr),
    Cos(StringExpr),
    Sin(StringExpr),
    Tan(StringExpr),
}

impl Functions {
    pub fn eval(operator: Functions, vars: &StringVariables) -> EvalResult {
        use Functions::*;

        match operator {
            Concat(list) => functions::concat(list, vars),
            Trim(lhs, rhs) => functions::trim(lhs, rhs, vars),
            Equal(lhs, rhs) => functions::equal(lhs, rhs, vars),
            NotEqual(lhs, rhs) => functions::not_equal(lhs, rhs, vars),
            And(lhs, rhs) => functions::and(lhs, rhs, vars),
            Or(lhs, rhs) => functions::or(lhs, rhs, vars),
            Contains(lhs, rhs) => functions::contains(lhs, rhs, vars),
            Upper(lhs) => functions::upper(lhs, vars),
            Lower(lhs) => functions::lower(lhs, vars),
            Add(lhs, rhs) => functions::add(lhs, rhs, vars),
            Sub(lhs, rhs) => functions::sub(lhs, rhs, vars),
            Mul(lhs, rhs) => functions::mul(lhs, rhs, vars),
            Div(lhs, rhs) => functions::div(lhs, rhs, vars),
            Pow(lhs, rhs) => functions::pow(lhs, rhs, vars),
            Cos(lhs) => functions::cos(lhs, vars),
            Sin(lhs) => functions::sin(lhs, vars),
            Tan(lhs) => functions::tan(lhs, vars),
        }
    }

    pub fn iter_variables<'a>(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        use Functions::*;

        match self {
            Trim(lhs, rhs)
            | Equal(lhs, rhs)
            | NotEqual(lhs, rhs)
            | And(lhs, rhs)
            | Or(lhs, rhs)
            | Contains(lhs, rhs)
            | Add(lhs, rhs)
            | Sub(lhs, rhs)
            | Mul(lhs, rhs)
            | Div(lhs, rhs)
            | Pow(lhs, rhs) => Box::new(lhs.iter_variables().chain(rhs.iter_variables())),
            Lower(lhs) 
            | Upper(lhs) 
            | Cos(lhs) 
            | Sin(lhs)
            | Tan(lhs) => Box::new(lhs.iter_variables()),
            Concat(list) => Box::new(list.iter().flat_map(|x| x.iter_variables())),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum ExpressionValue {
    String(String),
    Bool(bool),
    Number(f64),
    List(Vec<StringExpr>),
}

impl std::fmt::Display for ExpressionValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ExpressionValue::{Bool, List, Number};

        match self {
            ExpressionValue::String(x) => write!(f, "{}", x),
            Bool(x) => write!(f, "{}", x),
            Number(x) => write!(f, "{}", x),
            List(list) => write!(f, "[ {} ]", list_to_string(list).join(", ")),
        }
    }
}

fn list_to_string(input: &Vec<StringExpr>) -> Vec<String> {
    use StringExpr::*;
    input
        .iter()
        .map(|x| match x {
            Value(y) => y.to_string(),
            Expr(y) => {
                // temp while StringExpr doesn't implement display
                format!("{:?}", y)
            }
            Var(y) => y.to_string(),
        })
        .collect()
}

macro_rules! impl_from_integers {
    ($type: ty) => {
        impl From<$type> for ExpressionValue {
            fn from(input: $type) -> ExpressionValue {
                ExpressionValue::Number(input as f64)
            }
        }
    };
}

impl From<String> for ExpressionValue {
    fn from(input: String) -> ExpressionValue {
        ExpressionValue::from(input.as_str())
    }
}

impl From<&str> for ExpressionValue {
    fn from(input: &str) -> ExpressionValue {
        if let Ok(x) = input.to_lowercase().parse::<bool>() {
            return ExpressionValue::Bool(x);
        }
        ExpressionValue::String(String::from(input))
    }
}

impl From<bool> for ExpressionValue {
    fn from(input: bool) -> ExpressionValue {
        ExpressionValue::Bool(input)
    }
}

impl From<Vec<ExpressionValue>> for ExpressionValue {
    fn from(input: Vec<ExpressionValue>) -> ExpressionValue {
        let expressions = input
            .iter()
            .map(|x| StringExpr::Value(x.to_owned()))
            .collect();
        ExpressionValue::List(expressions)
    }
}

impl_from_integers!(f32);
impl_from_integers!(f64);

impl_from_integers!(u8);
impl_from_integers!(u16);
impl_from_integers!(u32);
impl_from_integers!(u64);
impl_from_integers!(usize);

impl_from_integers!(i8);
impl_from_integers!(i16);
impl_from_integers!(i32);
impl_from_integers!(i64);
impl_from_integers!(isize);

impl ExpressionValue {
    pub fn as_number(&self) -> Option<f64> {
        use ExpressionValue::*;

        match self {
            Number(x) => Some(*x),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        use ExpressionValue::*;

        match self {
            Bool(x) => Some(*x),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<String> {
        use ExpressionValue::*;

        match self {
            String(x) => Some(x.to_owned()),
            _ => None,
        }
    }

    pub fn as_list(&self) -> Option<Vec<StringExpr>> {
        use ExpressionValue::*;

        match self {
            List(x) => Some(x.to_owned()),
            _ => None,
        }
    }

    pub fn is_number(&self) -> bool {
        use ExpressionValue::*;

        match self {
            Number(_) => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        use ExpressionValue::*;

        match self {
            Bool(_) => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        use ExpressionValue::*;

        match self {
            String(_) => true,
            _ => false,
        }
    }

    pub fn is_list(&self) -> bool {
        use ExpressionValue::*;

        match self {
            List(_) => true,
            _ => false,
        }
    }

    pub fn is_falsy(&self) -> bool {
        use ExpressionValue::*;
        match self {
            String(string) => string == "",
            Bool(b) => !b,
            Number(float) => nearly_zero(float),
            List(list) => list.is_empty(),
        }
    }

    pub fn is_truthy(&self) -> bool {
        !self.is_falsy()
    }

    pub fn and(&self, other: &ExpressionValue) -> ExpressionValue {
        if self.is_truthy() {
            other.to_owned()
        } else {
            self.to_owned()
        }
    }

    pub fn or(&self, other: &ExpressionValue) -> ExpressionValue {
        if self.is_truthy() {
            self.to_owned()
        } else {
            other.to_owned()
        }
    }
}

fn nearly_zero(number: &f64) -> bool {
    if number > &0.0 {
        return false;
    }
    if number < &0.0 {
        return false;
    }
    true
}

#[test]
fn test_nearly_zero() {
    assert!(nearly_zero(&0.0));
}

#[test]
fn test_not_nearly_zero() {
    assert!(!nearly_zero(&1.5e-12));
    assert!(!nearly_zero(&-1.5e-12));
}

#[cfg(test)]
mod tests {
    mod iter_variables {
        use crate::StringExpr;

        #[test]
        fn simple_expression() {
            let parsed = StringExpr::parse(r#"true | false"#).unwrap();
            let result: Vec<String> = parsed.iter_variables().collect();
            assert_eq!(result, ["true", "false"]);
        }

        #[test]
        fn advanced_expression() {
            let parsed = StringExpr::parse(
                r#"
                upper(
                    concat(
                        1234, 
                        "_", 
                        contains(
                            "test", 
                            something
                        ) or trim(user_input, "_"),
                        "_", 
                        true
                    )
                )
            "#,
            )
            .unwrap();
            let result: Vec<String> = parsed.iter_variables().collect();
            assert_eq!(result, ["something", "user_input", "true"]);
        }

        #[test]
        fn simple_expression_without_defaults() {
            let parsed = StringExpr::parse(r#"true | false | test"#).unwrap();
            let result: Vec<String> = parsed.iter_variables_without_defaults().collect();
            assert_eq!(result, ["test"]);
        }
    }

    mod or {
        use crate::{ExpressionValue, StringExpr, StringVariables};

        #[test]
        fn operator_true() {
            let parsed = StringExpr::parse(r#"true | false"#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok(true.into()), result);
        }

        #[test]
        fn operator_false() {
            let parsed = StringExpr::parse(r#"false | false"#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok(false.into()), result);
        }

        #[test]
        fn operator_false_string() {
            let parsed = StringExpr::parse(r#"false | "test""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok("test".into()), result);
        }

        #[test]
        fn operator_true_string() {
            let parsed = StringExpr::parse(r#"true | "test""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok(true.into()), result);
        }

        #[test]
        fn operator_false_number_string() {
            let parsed = StringExpr::parse(r#"0 | "test""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok("test".into()), result);
        }

        #[test]
        fn operator_true_number_string() {
            let parsed = StringExpr::parse(r#"1 | "test""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok(1.0f64.into()), result);
        }

        #[test]
        fn operator_string_string() {
            let parsed = StringExpr::parse(r#""test" | "other""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok("test".into()), result);
        }

        #[test]
        fn operator_vec_string() {
            use ExpressionValue::{List, Number};
            use StringExpr::Value;

            let parsed = StringExpr::parse(r#"[1] | "other""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok(List(vec![Value(Number(1.0))])), result);
        }

        #[test]
        fn operator_empty_vec_string() {
            let parsed = StringExpr::parse(r#"[] | "other""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok("other".into()), result);
        }
    }

    mod and {
        use crate::{ExpressionValue, StringExpr, StringVariables};

        #[test]
        fn operator_false() {
            let parsed = StringExpr::parse(r#"true & false"#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok(false.into()), result);
        }

        #[test]
        fn operator_false_2() {
            let parsed = StringExpr::parse(r#"false & false"#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok(false.into()), result);
        }

        #[test]
        fn operator_true() {
            let parsed = StringExpr::parse(r#"true & true"#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok(true.into()), result);
        }

        #[test]
        fn operator_number_false() {
            let parsed = StringExpr::parse(r#"5 & false"#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok(false.into()), result);
        }

        #[test]
        fn operator_number_true() {
            let parsed = StringExpr::parse(r#"1 & true"#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok(true.into()), result);
        }

        #[test]
        fn operator_false_string() {
            let parsed = StringExpr::parse(r#"false & "test""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok(false.into()), result);
        }

        #[test]
        fn operator_true_string() {
            let parsed = StringExpr::parse(r#"true & "test""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok("test".into()), result);
        }

        #[test]
        fn operator_string_string() {
            let parsed = StringExpr::parse(r#""test" & "other""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok("other".into()), result);
        }

        #[test]
        fn operator_string_string_2() {
            let parsed = StringExpr::parse(r#"false & "other""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok(false.into()), result);
        }

        #[test]
        fn operator_vec_string() {
            let parsed = StringExpr::parse(r#"[1] & "other""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok("other".into()), result);
        }

        #[test]
        fn operator_empty_vec_string() {
            use ExpressionValue::List;
            let parsed = StringExpr::parse(r#"[] & "other""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok(List(vec![])), result);
        }
    }

    mod number {
        use crate::{StringExpr, StringVariables};

        #[test]
        fn simple_addition() {
            let parsed = StringExpr::parse("1 + 2").unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok(3.into()), result);
        }

        #[test]
        fn simple_subtraction() {
            let parsed = StringExpr::parse("1 - 2").unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok((-1).into()), result);
        }

        #[test]
        fn simple_multiplication() {
            let parsed = StringExpr::parse("1 * 2").unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok(2.into()), result);
        }

        #[test]
        fn simple_division() {
            let parsed = StringExpr::parse("1 / 2").unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok(0.5.into()), result);
        }

        #[test]
        fn simple_power() {
            let parsed = StringExpr::parse("2^2").unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok(4.into()), result);
        }

        #[test]
        fn cosine() {
            let parsed = StringExpr::parse("cos(0)").unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok(1.into()), result);
        }

        #[test]
        fn sine() {
            let parsed = StringExpr::parse("sin(0)").unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok(0.into()), result);
        }

        #[test]
        fn tangent() {
            let parsed = StringExpr::parse("tan(0)").unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Ok(0.into()), result);
        }

        #[test]
        fn sine_invalid_argments() {
            assert!(StringExpr::parse("sin()").is_err());
            assert!(StringExpr::parse("sin(1,2,3)").is_err());
        }

        #[test]
        fn combine_functions() {
            let parsed =
                StringExpr::parse(r#"3*((((1 + 2) / (3**2) + 5) - 2 + 5) * (2 / 4) * 4) / 2.5"#)
                    .unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default())
                .unwrap()
                .as_number()
                .unwrap();
            assert_eq!(20.0, result.round());
        }
    }

    use crate::{StringExpr, StringVariables};

    #[test]
    fn equal_operator_true() {
        let parsed = StringExpr::parse(r#""test" == "test""#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Ok(true.into()), result);
    }

    #[test]
    fn equal_operator_false() {
        let parsed = StringExpr::parse(r#""test" == "other""#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Ok(false.into()), result);
    }

    #[test]
    fn not_equal_operator_true() {
        let parsed = StringExpr::parse(r#""test" != "test""#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Ok(false.into()), result);
    }

    #[test]
    fn not_equal_operator_false() {
        let parsed = StringExpr::parse(r#""test" != "other""#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Ok(true.into()), result);
    }

    #[test]
    fn concat_operator() {
        // let parsed = StringExpr::parse(r#""test" ++ "test""#).unwrap();
        let parsed = match StringExpr::parse(r#""test" ++ "test""#) {
            Ok(x) => x,
            Err(e) => {
                println!("{}", e);
                panic!("error")
            }
        };
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Ok("testtest".into()), result);
    }

    #[test]
    fn concat_function() {
        let parsed = StringExpr::parse(r#"concat("other", "test")"#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Ok("othertest".into()), result);
    }

    #[test]
    fn concat_function_multi() {
        let parsed = StringExpr::parse(r#"concat("1", 2, "3", "4")"#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Ok("1234".into()), result);
    }

    #[test]
    fn concat_function_variables() {
        let parsed = StringExpr::parse(r#"concat(test, "-", other, third, "!!")"#).unwrap();

        let mut vars = StringVariables::default();
        vars.insert("test", "1");
        vars.insert("other", "test");
        vars.insert("third", "3456");

        let result = StringExpr::eval(parsed, &vars);
        assert_eq!(Ok("1-test3456!!".into()), result);
    }

    #[test]
    fn concat_function_one() {
        let parsed = StringExpr::parse(r#"concat("test")"#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Ok("test".into()), result);
    }

    #[test]
    fn concat_function_list() {
        let parsed = StringExpr::parse(r#"[1, 4, 5] ++ [2, 3]"#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(
            Ok(vec![1.into(), 4.into(), 5.into(), 2.into(), 3.into()].into()),
            result
        );
    }

    #[test]
    fn upper() {
        let parsed = StringExpr::parse(r#"upper("test")"#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Ok("TEST".into()), result);
    }

    #[test]
    fn trim() {
        let parsed = StringExpr::parse(r#"trim("..test...............", ".")"#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Ok("test".into()), result);
    }

    #[test]
    fn contains_true() {
        let parsed = StringExpr::parse(r#"contains("test", "test")"#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Ok(true.into()), result);
    }

    #[test]
    fn contains_false() {
        let parsed = StringExpr::parse(r#"contains("test", "something")"#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Ok(false.into()), result);
    }

    #[test]
    fn combine_functions() {
        let parsed = StringExpr::parse(
            r#"
            upper(
                concat(
                    1234, 
                    "_", 
                    contains(
                        "test", 
                        "something"
                    ) or trim("__testing", "_"),
                    "_", 
                    true
                )
            )
        "#,
        )
        .unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Ok("1234_TESTING_TRUE".into()), result);
    }
}
