use std::collections::{BTreeMap, HashMap};

use pest::error::Error;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use pest::Parser;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[derive(Parser)]
#[grammar = "string_expression.pest"]
struct StringExpressionParser;

lazy_static! {
    static ref DEFAULT_VARIABLES: HashMap<String, String> = {
        let mut m = HashMap::new();
        m.insert(String::from("true"), String::from("true"));
        m.insert(String::from("false"), String::from("false"));
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
            Rule::num => Value(pair.as_str().trim().to_string()),
            Rule::string => Value(pair.as_str().trim_matches('"').to_string()),
            Rule::expr => parse_expression(pair.into_inner()),
            Rule::var => Var(pair.as_str().trim().to_string()),
            rule => make_function(rule, pair),
        },
        |lhs: StringExpr, op: Pair<Rule>, rhs: StringExpr| match op.as_rule() {
            Rule::add => Expr(Box::new(StringOps::Concat(vec![lhs, rhs]))),
            Rule::equal => Expr(Box::new(StringOps::Equal(lhs, rhs))),
            Rule::not_equal => Expr(Box::new(StringOps::NotEqual(lhs, rhs))),
            Rule::and => Expr(Box::new(StringOps::And(lhs, rhs))),
            Rule::or => Expr(Box::new(StringOps::Or(lhs, rhs))),
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
        Rule::upper => Expr(Box::new(StringOps::Upper(arguments[0].clone()))),
        Rule::trim => Expr(Box::new(StringOps::Trim(
            arguments[0].clone(),
            arguments[1].clone(),
        ))),
        Rule::contains => Expr(Box::new(StringOps::Contains(
            arguments[0].clone(),
            arguments[1].clone(),
        ))),
        Rule::concat => Expr(Box::new(StringOps::Concat(arguments))),
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
    state: HashMap<String, String>,
}

impl StringVariables {
    pub fn get(&self, key: &str) -> Option<&String> {
        self.state.get(key)
    }

    pub fn insert(&mut self, key: &str, value: String) -> Option<String> {
        self.state.insert(String::from(key), value)
    }

    pub fn from_iter<T: IntoIterator<Item = (String, String)>>(iter: T) -> StringVariables {
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

impl From<HashMap<String, String>> for StringVariables {
    fn from(state: HashMap<String, String>) -> StringVariables {
        StringVariables::from_iter(state.into_iter())
    }
}

impl From<BTreeMap<String, String>> for StringVariables {
    fn from(state: BTreeMap<String, String>) -> StringVariables {
        StringVariables::from_iter(state.into_iter())
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum StringExpr {
    Value(String),
    Expr(Box<StringOps>),
    Var(String),
}

impl StringExpr {
    pub fn eval(expression: StringExpr, vars: &StringVariables) -> Option<String> {
        Some(match expression {
            StringExpr::Expr(op) => StringOps::eval(*op, vars)?,
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
pub enum StringOps {
    Concat(Vec<StringExpr>),
    Equal(StringExpr, StringExpr),
    NotEqual(StringExpr, StringExpr),
    And(StringExpr, StringExpr),
    Or(StringExpr, StringExpr),
    Trim(StringExpr, StringExpr),
    Contains(StringExpr, StringExpr),
    Upper(StringExpr),
}

impl StringOps {
    pub fn eval(operator: StringOps, vars: &StringVariables) -> Option<String> {
        use StringOps::*;

        Some(match operator {
            Concat(list) => list.iter().try_fold(String::new(), |mut acc, x| {
                acc.push_str(&StringExpr::eval(x.clone(), vars)?);
                Some(acc)
            })?,
            Trim(lhs, rhs) => {
                let string = StringExpr::eval(lhs, vars)?;
                let trim_with = &StringExpr::eval(rhs, vars)?;
                string
                    .trim_end_matches(trim_with)
                    .trim_start_matches(trim_with)
                    .to_string()
            }
            Equal(lhs, rhs) => {
                let string = StringExpr::eval(lhs, vars)?;
                let other = &StringExpr::eval(rhs, vars)?;
                string.eq(other).to_string()
            }
            NotEqual(lhs, rhs) => {
                use std::ops::Not;

                let string = StringExpr::eval(lhs, vars)?;
                let other = &StringExpr::eval(rhs, vars)?;
                string.eq(other).not().to_string()
            }
            And(lhs, rhs) => {
                let string: StringBoolean = StringExpr::eval(lhs, vars)?.into();
                let other: StringBoolean = StringExpr::eval(rhs, vars)?.into();
                string.and(&other).to_string()
            }
            Or(lhs, rhs) => {
                let string: StringBoolean = StringExpr::eval(lhs, vars)?.into();
                let other: StringBoolean = StringExpr::eval(rhs, vars)?.into();
                string.or(&other).to_string()
            }
            Contains(lhs, rhs) => {
                let string = StringExpr::eval(lhs, vars)?;
                let contains = &StringExpr::eval(rhs, vars)?;
                string.contains(contains).to_string()
            }
            Upper(lhs) => StringExpr::eval(lhs, vars)?.to_uppercase(),
        })
    }
}

#[derive(Debug, Clone)]
enum StringBoolean {
    String(String),
    Bool(bool),
}

impl std::fmt::Display for StringBoolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StringBoolean::String(x) => write!(f, "{}", x),
            StringBoolean::Bool(x) => write!(f, "{}", x),
        }
    }
}

impl From<String> for StringBoolean {
    fn from(input: String) -> StringBoolean {
        if let Ok(x) = input.parse::<bool>() {
            StringBoolean::Bool(x)
        } else {
            StringBoolean::String(input)
        }
    }
}

impl From<bool> for StringBoolean {
    fn from(input: bool) -> StringBoolean {
        StringBoolean::Bool(input)
    }
}

impl StringBoolean {
    pub fn and(&self, other: &StringBoolean) -> StringBoolean {
        match self {
            StringBoolean::String(_) => match other {
                StringBoolean::String(_) | StringBoolean::Bool(false) => other.to_owned(),
                StringBoolean::Bool(true) => self.to_owned(),
            },
            StringBoolean::Bool(true) => other.to_owned(),
            StringBoolean::Bool(false) => self.to_owned(),
        }
    }

    pub fn or(&self, other: &StringBoolean) -> StringBoolean {
        match self {
            StringBoolean::String(_) | StringBoolean::Bool(true) => self.to_owned(),
            StringBoolean::Bool(false) => other.to_owned(),
        }
    }
}

#[cfg(test)]
mod tests {
    mod or {
        use crate::{StringExpr, StringVariables};

        #[test]
        fn operator_true() {
            let parsed = StringExpr::parse(r#""true" | "false""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Some("true".to_string()), result);
        }

        #[test]
        fn operator_false() {
            let parsed = StringExpr::parse(r#""false" | "false""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Some("false".to_string()), result);
        }

        #[test]
        fn operator_false_string() {
            let parsed = StringExpr::parse(r#""false" | "test""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Some("test".to_string()), result);
        }

        #[test]
        fn operator_true_string() {
            let parsed = StringExpr::parse(r#""true" | "test""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Some("true".to_string()), result);
        }

        #[test]
        fn operator_string_string() {
            let parsed = StringExpr::parse(r#""test" | "other""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Some("test".to_string()), result);
        }
    }

    mod and {
        use crate::{StringExpr, StringVariables};

        #[test]
        fn operator_false() {
            let parsed = StringExpr::parse(r#""true" & "false""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Some("false".to_string()), result);
        }

        #[test]
        fn operator_false_2() {
            let parsed = StringExpr::parse(r#""false" & "false""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Some("false".to_string()), result);
        }

        #[test]
        fn operator_true() {
            let parsed = StringExpr::parse(r#""true" & "true""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Some("true".to_string()), result);
        }

        #[test]
        fn operator_false_string() {
            let parsed = StringExpr::parse(r#""false" & "test""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Some("false".to_string()), result);
        }

        #[test]
        fn operator_true_string() {
            let parsed = StringExpr::parse(r#""true" & "test""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Some("test".to_string()), result);
        }

        #[test]
        fn operator_string_string() {
            let parsed = StringExpr::parse(r#""test" & "other""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Some("other".to_string()), result);
        }

        #[test]
        fn operator_string_string_2() {
            let parsed = StringExpr::parse(r#""false" & "other""#).unwrap();
            let result = StringExpr::eval(parsed, &StringVariables::default());
            assert_eq!(Some("false".to_string()), result);
        }
    }
    use crate::{StringExpr, StringVariables};

    #[test]
    fn equal_operator_true() {
        let parsed = StringExpr::parse(r#""test" == "test""#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("true".to_string()), result);
    }

    #[test]
    fn equal_operator_false() {
        let parsed = StringExpr::parse(r#""test" == "other""#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("false".to_string()), result);
    }

    #[test]
    fn not_equal_operator_true() {
        let parsed = StringExpr::parse(r#""test" != "test""#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("false".to_string()), result);
    }

    #[test]
    fn not_equal_operator_false() {
        let parsed = StringExpr::parse(r#""test" != "other""#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("true".to_string()), result);
    }

    #[test]
    fn concat_operator() {
        let parsed = StringExpr::parse(r#""test" ++ "test""#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("testtest".to_string()), result);
    }

    #[test]
    fn concat_function() {
        let parsed = StringExpr::parse(r#"concat("other", "test")"#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("othertest".to_string()), result);
    }

    #[test]
    fn concat_function_multi() {
        let parsed = StringExpr::parse(r#"concat("1", "2", "3", "4")"#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("1234".to_string()), result);
    }

    #[test]
    fn concat_function_variables() {
        let parsed = StringExpr::parse(r#"concat(test, "-", other, third, "!!")"#).unwrap();

        let mut vars = StringVariables::default();
        vars.insert("test", String::from("1"));
        vars.insert("other", String::from("test"));
        vars.insert("third", String::from("3456"));

        let result = StringExpr::eval(parsed, &vars);
        assert_eq!(Some("1-test3456!!".to_string()), result);
    }

    #[test]
    fn concat_function_one() {
        let parsed = StringExpr::parse(r#"concat("test")"#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("test".to_string()), result);
    }

    #[test]
    fn upper() {
        let parsed = StringExpr::parse(r#"upper("test")"#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("TEST".to_string()), result);
    }

    #[test]
    fn trim() {
        let parsed = StringExpr::parse(r#"trim("..test...............", ".")"#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("test".to_string()), result);
    }

    #[test]
    fn contains_true() {
        let parsed = StringExpr::parse(r#"contains("test", "test")"#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("true".to_string()), result);
    }

    #[test]
    fn contains_false() {
        let parsed = StringExpr::parse(r#"contains("test", "something")"#).unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("false".to_string()), result);
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
        assert_eq!(Some("1234_TESTING_TRUE".to_string()), result);
    }
}
