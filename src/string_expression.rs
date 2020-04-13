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
        let m = HashMap::new();
        m
    };
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Assoc::*;
        use Rule::*;

        PrecClimber::new(vec![Operator::new(add, Left)])
    };
}

// mostly copied from pest calculator example
fn parse_expression(expression: Pairs<Rule>) -> StringExpr {
    PREC_CLIMBER.climb(
        expression,
        |pair: Pair<Rule>| match pair.as_rule() {
            Rule::num => StringExpr::Value(pair.as_str().trim().to_string()),
            Rule::string => StringExpr::Value(pair.as_str().trim_matches('"').to_string()),
            Rule::expr => parse_expression(pair.into_inner()),
            Rule::var => StringExpr::Var(pair.as_str().trim().to_string()),
            rule => make_function(rule, pair),
        },
        |lhs: StringExpr, op: Pair<Rule>, rhs: StringExpr| match op.as_rule() {
            Rule::add => StringExpr::Expr(Box::new(StringOps::Concat(vec![lhs, rhs]))),
            // _ => unreachable!(),
            rule => {
                println!("{:?}", rule);
                unreachable!()
            }
        },
    )
}

fn make_function(rule: Rule, pair: Pair<Rule>) -> StringExpr {
    let arguments: Vec<StringExpr> = pair
        .into_inner()
        .map(|x| parse_expression(x.into_inner()))
        .collect();
    match rule {
        Rule::upper => StringExpr::Expr(Box::new(StringOps::Upper(arguments[0].clone()))),
        Rule::trim => StringExpr::Expr(Box::new(StringOps::Trim(
            arguments[0].clone(),
            arguments[1].clone(),
        ))),
        Rule::concat => StringExpr::Expr(Box::new(StringOps::Concat(arguments))),
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
    Trim(StringExpr, StringExpr),
    Upper(StringExpr),
}

impl StringOps {
    pub fn eval(operator: StringOps, vars: &StringVariables) -> Option<String> {
        Some(match operator {
            StringOps::Concat(list) => {
                // let mut string = String::new();

                // let mut string = StringExpr::eval(lhs, vars)?;
                // string.push_str(&StringExpr::eval(rhs, vars)?);
                // string
                list.iter().try_fold(String::new(), |mut acc, x| {
                    acc.push_str(&StringExpr::eval(x.clone(), vars)?);
                    Some(acc)
                })?
            }
            StringOps::Trim(lhs, rhs) => {
                let string = StringExpr::eval(lhs, vars)?;
                let trim_with = &StringExpr::eval(rhs, vars)?;
                string
                    .trim_end_matches(trim_with)
                    .trim_start_matches(trim_with)
                    .to_string()
            }
            StringOps::Upper(lhs) => StringExpr::eval(lhs, vars)?.to_uppercase(),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{StringExpr, StringVariables};

    #[test]
    fn concat_operator() {
        let parsed = StringExpr::parse("\"test\" + \"test\"").unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("testtest".to_string()), result);
    }

    #[test]
    fn concat_function() {
        let parsed = StringExpr::parse("concat(\"other\", \"test\")").unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("othertest".to_string()), result);
    }

    #[test]
    fn concat_function_multi() {
        let parsed = StringExpr::parse("concat(\"1\", \"2\", \"3\", \"4\")").unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("1234".to_string()), result);
    }

    #[test]
    fn concat_function_variables() {
        let parsed = StringExpr::parse("concat(test, \"-\", other, third, \"!!\")").unwrap();

        let mut vars = StringVariables::default();
        vars.insert("test", String::from("1"));
        vars.insert("other", String::from("test"));
        vars.insert("third", String::from("3456"));

        let result = StringExpr::eval(parsed, &vars);
        assert_eq!(Some("1-test3456!!".to_string()), result);
    }

    #[test]
    fn concat_function_one() {
        let parsed = StringExpr::parse("concat(\"test\")").unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("test".to_string()), result);
    }

    #[test]
    fn upper() {
        let parsed = StringExpr::parse("upper(\"test\")").unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("TEST".to_string()), result);
    }

    #[test]
    fn trim() {
        let parsed = StringExpr::parse("trim(\"..test...............\", \".\")").unwrap();
        let result = StringExpr::eval(parsed, &StringVariables::default());
        assert_eq!(Some("test".to_string()), result);
    }
}
