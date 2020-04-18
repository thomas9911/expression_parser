use std::collections::{BTreeMap, HashMap};

use pest::error::Error;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use pest::Parser;

mod functions;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[derive(Parser)]
#[grammar = "string_expression.pest"]
struct StringExpressionParser;

lazy_static! {
    static ref DEFAULT_VARIABLES: HashMap<String, ExpressionValue> = {
        let mut m = HashMap::new();
        m.insert(String::from("true"), ExpressionValue::Bool(true));
        m.insert(String::from("false"), ExpressionValue::Bool(false));
        m
    };
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Assoc::*;
        use Rule::*;

        PrecClimber::new(vec![
            Operator::new(add, Left),
            Operator::new(equal, Right),
            Operator::new(not_equal, Right),
            Operator::new(and, Left),
            Operator::new(or, Left),
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
            Rule::string => Value(pair.as_str().trim_matches('"').to_string().into()),
            Rule::expr => parse_expression(pair.into_inner()),
            Rule::var => Var(pair.as_str().trim().to_string()),
            rule => make_function(rule, pair),
        },
        |lhs: StringExpr, op: Pair<Rule>, rhs: StringExpr| match op.as_rule() {
            Rule::add => Expr(Box::new(Functions::Concat(vec![lhs, rhs]))),
            Rule::equal => Expr(Box::new(Functions::Equal(lhs, rhs))),
            Rule::not_equal => Expr(Box::new(Functions::NotEqual(lhs, rhs))),
            Rule::and => Expr(Box::new(Functions::And(lhs, rhs))),
            Rule::or => Expr(Box::new(Functions::Or(lhs, rhs))),
            // _ => unreachable!(),
            rule => {
                println!("{:?}", rule);
                unreachable!()
            }
        },
    )
}

fn make_function(rule: Rule, pair: Pair<Rule>) -> StringExpr {
    use StringExpr::Expr;
    let arguments: Vec<StringExpr> = pair
        .into_inner()
        .map(|x| parse_expression(x.into_inner()))
        .collect();
    match rule {
        Rule::upper => Expr(Box::new(Functions::Upper(arguments[0].clone()))),
        Rule::trim => Expr(Box::new(Functions::Trim(
            arguments[0].clone(),
            arguments[1].clone(),
        ))),
        Rule::contains => Expr(Box::new(Functions::Contains(
            arguments[0].clone(),
            arguments[1].clone(),
        ))),
        Rule::concat => Expr(Box::new(Functions::Concat(arguments))),
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

    pub fn insert(&mut self, key: &str, value: ExpressionValue) -> Option<ExpressionValue> {
        self.state.insert(String::from(key), value)
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

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum StringExpr {
    Value(ExpressionValue),
    Expr(Box<Functions>),
    Var(String),
}

impl StringExpr {
    pub fn eval(expression: StringExpr, vars: &StringVariables) -> Option<ExpressionValue> {
        Some(match expression {
            StringExpr::Expr(op) => Functions::eval(*op, vars)?,
            StringExpr::Value(x) => x,
            StringExpr::Var(s) => vars.get(&s)?.clone(),
        })
    }

    pub fn parse(input: &str) -> Result<StringExpr, Error<Rule>> {
        Ok(parse_expression(StringExpressionParser::parse(
            Rule::calculation,
            input,
        )?))
    }
}

#[derive(Debug, Clone)]
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
}

impl Functions {
    pub fn eval(operator: Functions, vars: &StringVariables) -> Option<ExpressionValue> {
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
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionValue {
    String(String),
    Bool(bool),
    Number(f64),
}

impl std::fmt::Display for ExpressionValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionValue::String(x) => write!(f, "{}", x),
            ExpressionValue::Bool(x) => write!(f, "{}", x),
            ExpressionValue::Number(x) => write!(f, "{}", x),
        }
    }
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

impl From<u64> for ExpressionValue {
    fn from(input: u64) -> ExpressionValue {
        ExpressionValue::Number(input as f64)
    }
}

impl From<f64> for ExpressionValue {
    fn from(input: f64) -> ExpressionValue {
        ExpressionValue::Number(input)
    }
}

impl ExpressionValue {
    pub fn and(&self, other: &ExpressionValue) -> ExpressionValue {
        match self {
            ExpressionValue::String(_) => match other {
                ExpressionValue::String(_) | ExpressionValue::Bool(false) => other.to_owned(),
                ExpressionValue::Number(float) if nearly_zero(float) => other.to_owned(),
                ExpressionValue::Bool(true) | ExpressionValue::Number(_) => self.to_owned(),
            },
            ExpressionValue::Bool(true) => other.to_owned(),
            ExpressionValue::Bool(false) => self.to_owned(),
            ExpressionValue::Number(float) if nearly_zero(float) => self.to_owned(),
            ExpressionValue::Number(_) => other.to_owned(),
        }
    }

    pub fn or(&self, other: &ExpressionValue) -> ExpressionValue {
        match self {
            ExpressionValue::String(_) | ExpressionValue::Bool(true) => self.to_owned(),
            ExpressionValue::Bool(false) => other.to_owned(),
            ExpressionValue::Number(float) if nearly_zero(float) => other.to_owned(),
            ExpressionValue::Number(_) => self.to_owned(),
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
    mod or {
        use crate::{StringExpr, StringVariables};

        #[test]
        fn operator_true() {
            let parsed = StringExpr::parse(r#""true" | "false""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Some("true".into()), result);
        }

        #[test]
        fn operator_false() {
            let parsed = StringExpr::parse(r#""false" | "false""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Some("false".into()), result);
        }

        #[test]
        fn operator_false_string() {
            let parsed = StringExpr::parse(r#""false" | "test""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Some("test".into()), result);
        }

        #[test]
        fn operator_true_string() {
            let parsed = StringExpr::parse(r#""true" | "test""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Some("true".into()), result);
        }

        #[test]
        fn operator_string_string() {
            let parsed = StringExpr::parse(r#""test" | "other""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Some("test".into()), result);
        }
    }

    mod and {
        use crate::{StringExpr, StringVariables};

        #[test]
        fn operator_false() {
            let parsed = StringExpr::parse(r#""true" & "false""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Some("false".into()), result);
        }

        #[test]
        fn operator_false_2() {
            let parsed = StringExpr::parse(r#""false" & "false""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Some("false".into()), result);
        }

        #[test]
        fn operator_true() {
            let parsed = StringExpr::parse(r#""true" & "true""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Some("true".into()), result);
        }

        #[test]
        fn operator_false_string() {
            let parsed = StringExpr::parse(r#""false" & "test""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Some("false".into()), result);
        }

        #[test]
        fn operator_true_string() {
            let parsed = StringExpr::parse(r#""true" & "test""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Some("test".into()), result);
        }

        #[test]
        fn operator_string_string() {
            let parsed = StringExpr::parse(r#""test" & "other""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Some("other".into()), result);
        }

        #[test]
        fn operator_string_string_2() {
            let parsed = StringExpr::parse(r#""false" & "other""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Some("false".into()), result);
        }
    }
    use crate::{ExpressionValue, StringExpr, StringVariables};

    #[test]
    fn equal_operator_true() {
        let parsed = StringExpr::parse(r#""test" == "test""#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("true".into()), result);
    }

    #[test]
    fn equal_operator_false() {
        let parsed = StringExpr::parse(r#""test" == "other""#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("false".into()), result);
    }

    #[test]
    fn not_equal_operator_true() {
        let parsed = StringExpr::parse(r#""test" != "test""#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("false".into()), result);
    }

    #[test]
    fn not_equal_operator_false() {
        let parsed = StringExpr::parse(r#""test" != "other""#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("true".into()), result);
    }

    #[test]
    fn concat_operator() {
        let parsed = StringExpr::parse(r#""test" ++ "test""#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("testtest".into()), result);
    }

    #[test]
    fn concat_function() {
        let parsed = StringExpr::parse(r#"concat("other", "test")"#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("othertest".into()), result);
    }

    #[test]
    fn concat_function_multi() {
        let parsed = StringExpr::parse(r#"concat("1", "2", "3", "4")"#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("1234".into()), result);
    }

    #[test]
    fn concat_function_variables() {
        let parsed = StringExpr::parse(r#"concat(test, "-", other, third, "!!")"#).unwrap();

        let mut vars = StringVariables::default();
        vars.insert("test", ExpressionValue::from("1"));
        vars.insert("other", ExpressionValue::from("test"));
        vars.insert("third", ExpressionValue::from("3456"));

        let result = StringExpr::eval(parsed, &vars);
        assert_eq!(Some("1-test3456!!".into()), result);
    }

    #[test]
    fn concat_function_one() {
        let parsed = StringExpr::parse(r#"concat("test")"#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("test".into()), result);
    }

    #[test]
    fn upper() {
        let parsed = StringExpr::parse(r#"upper("test")"#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("TEST".into()), result);
    }

    #[test]
    fn trim() {
        let parsed = StringExpr::parse(r#"trim("..test...............", ".")"#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("test".into()), result);
    }

    #[test]
    fn contains_true() {
        let parsed = StringExpr::parse(r#"contains("test", "test")"#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("true".into()), result);
    }

    #[test]
    fn contains_false() {
        let parsed = StringExpr::parse(r#"contains("test", "something")"#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("false".into()), result);
    }

    #[test]
    fn combine_functions() {
        let parsed = StringExpr::parse(
            r#"
            upper(
                concat(
                    "1234", 
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
        assert_eq!(Some("1234_TESTING_TRUE".into()), result);
    }
}
