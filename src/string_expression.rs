use std::collections::{BTreeMap, HashMap};
use std::fmt::Write;

use pest::error::Error as PestError;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use pest::Parser;

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
fn parse_expression(expression: Pairs<Rule>) -> StringExpr {
    use StringExpr::*;

    PREC_CLIMBER.climb(
        expression,
        |pair: Pair<Rule>| match pair.as_rule() {
            Rule::num => Value(pair.as_str().parse::<f64>().unwrap().into()),
            Rule::string => Value(ExpressionValue::String(
                pair.as_str().trim_matches('"').to_string(),
            )),
            Rule::list => {
                let arguments: Vec<StringExpr> = pair
                    .into_inner()
                    .map(|x| parse_expression(x.into_inner()))
                    .collect();
                Value(ExpressionValue::List(arguments))
            }
            Rule::expr => parse_expression(pair.into_inner()),
            Rule::var => Var(pair.as_str().trim().to_string()),
            rule => make_function(rule, pair),
        },
        |lhs: StringExpr, op: Pair<Rule>, rhs: StringExpr| match op.as_rule() {
            Rule::concat_op => Expr(Box::new(Functions::Concat(vec![lhs, rhs]))),
            Rule::equal => Expr(Box::new(Functions::Equal(lhs, rhs))),
            Rule::not_equal => Expr(Box::new(Functions::NotEqual(lhs, rhs))),
            Rule::and => Expr(Box::new(Functions::And(lhs, rhs))),
            Rule::or => Expr(Box::new(Functions::Or(lhs, rhs))),
            Rule::add => Expr(Box::new(Functions::Add(lhs, rhs))),
            Rule::subtract => Expr(Box::new(Functions::Sub(lhs, rhs))),
            Rule::multiply => Expr(Box::new(Functions::Mul(lhs, rhs))),
            Rule::divide => Expr(Box::new(Functions::Div(lhs, rhs))),
            Rule::power => Expr(Box::new(Functions::Pow(lhs, rhs))),
            // _ => unreachable!(),
            rule => {
                println!("{:?}", rule);
                unreachable!()
            }
        },
    )
}

fn make_function(rule: Rule, pair: Pair<Rule>) -> StringExpr {
    use Rule::*;
    use StringExpr::Expr;

    let arguments: Vec<StringExpr> = pair
        .into_inner()
        .map(|x| parse_expression(x.into_inner()))
        .collect();
    match rule {
        upper => Expr(Box::new(Functions::Upper(arguments[0].clone()))),
        cos => Expr(Box::new(Functions::Cos(arguments[0].clone()))),
        sin => Expr(Box::new(Functions::Sin(arguments[0].clone()))),
        tan => Expr(Box::new(Functions::Tan(arguments[0].clone()))),
        trim => Expr(Box::new(Functions::Trim(
            arguments[0].clone(),
            arguments[1].clone(),
        ))),
        contains => Expr(Box::new(Functions::Contains(
            arguments[0].clone(),
            arguments[1].clone(),
        ))),
        concat => Expr(Box::new(Functions::Concat(arguments))),
        _ => unreachable!(),
    }
}

// fn check_arguments(min_args: usize, max_args: usize, args: &Vec<StringExpr>) {
//     if (min_args < args.len()) | (args.len() < max_args) {
//         panic!("invalid arguments")
//     }
// }

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
        Ok(parse_expression(StringExpressionParser::parse(
            Rule::calculation,
            input,
        )?))
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
            Upper(lhs) | Cos(lhs) | Sin(lhs) | Tan(lhs) => Box::new(lhs.iter_variables()),
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
    input.iter()
    .map(|x| match x {
        Value(y) => y.to_string(),
        Expr(y) => {
            let mut s = String::new();
            write!(&mut s, "{:?}", y).unwrap();
            s
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

    pub fn and(&self, other: &ExpressionValue) -> ExpressionValue {
        use ExpressionValue::*;

        match self {
            String(_) => match other {
                String(_) | Bool(false) => other.to_owned(),
                Number(float) if nearly_zero(float) => other.to_owned(),
                Bool(true) | Number(_) => self.to_owned(),
                _ => self.to_owned(),
            },
            Bool(true) => other.to_owned(),
            Bool(false) => self.to_owned(),
            Number(float) if nearly_zero(float) => self.to_owned(),
            Number(_) => other.to_owned(),
            _ => self.to_owned(),
        }
    }

    pub fn or(&self, other: &ExpressionValue) -> ExpressionValue {
        use ExpressionValue::*;

        match self {
            String(_) | Bool(true) => self.to_owned(),
            Bool(false) => other.to_owned(),
            Number(float) if nearly_zero(float) => other.to_owned(),
            Number(_) => self.to_owned(),
            _ => self.to_owned(),
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
        use crate::{StringExpr, StringVariables};

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
    }

    mod and {
        use crate::{StringExpr, StringVariables};

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
