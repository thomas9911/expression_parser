//! ## A sort of calculator in Rust using Pest.
//! 
//! Take a look at the calculator example:
//! ```sh
//! cargo run --example calculator 1 + 12
//! ```
//! 
//! ### library usage
//! 
//! Simple example 
//! ```
//! # use pest::error::Error;
//! use expression_parser::{Expr, Variables};
//! # use expression_parser::Rule;
//! 
//! # fn main() -> Result<(), Error<Rule>> {
//! let parsed = Expr::parse("1 + 5 - 2")?;
//! let result = Expr::eval(parsed, &Variables::default());
//! 
//! assert_eq!(Some(4.0), result);
//! # Ok(())
//! # }
//! ```
//! 
//! Another example 
//! ```
//! # use pest::error::Error;
//! use expression_parser::{Expr, Variables};
//! # use expression_parser::Rule;
//! 
//! # fn main() -> Result<(), Error<Rule>> {
//! let parsed = Expr::parse("e ^ (1 + 5 - 2)")?;
//! let result = Expr::eval(parsed, &Variables::default());
//! 
//! assert_eq!(Some(std::f64::consts::E.powf(4.0)), result);
//! # Ok(())
//! # }
//! ```
//! 
//! Use build-in variables and functions
//! ```
//! # use pest::error::Error;
//! use expression_parser::{Expr, Variables};
//! # use expression_parser::Rule;
//! 
//! # fn main() -> Result<(), Error<Rule>> {
//! let parsed = Expr::parse("sin(e) + 1")?;
//! let result = Expr::eval(parsed, &Variables::default());
//! 
//! assert_eq!(Some(std::f64::consts::E.sin() + 1.0), result);
//! # Ok(())
//! # }
//! ```
//! 
//! Use your own variables
//! ```
//! # use pest::error::Error;
//! use expression_parser::{Expr, Variables};
//! # use expression_parser::Rule;
//! 
//! # fn main() -> Result<(), Error<Rule>> {
//! let parsed = Expr::parse("x + y + z")?;
//! 
//! let mut vars = std::collections::HashMap::new();
//! vars.insert(String::from("x"), 3.0);
//! vars.insert(String::from("y"), 3.0);
//! vars.insert(String::from("z"), 10.0);
//! 
//! let result = Expr::eval(parsed.clone(), &vars.into());
//! 
//! assert_eq!(Some(16.0), result); 
//! 
//! let mut vars = Variables::default();
//! vars.insert("x", 3.0);
//! vars.insert("y", 3.0);
//! vars.insert("z", 10.0);
//! 
//! let result = Expr::eval(parsed, &vars);
//! assert_eq!(Some(16.0), result);
//! # Ok(())
//! # }
//! ```

#![recursion_limit = "1024"]

#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;

use std::collections::{BTreeMap, HashMap};

use pest::error::Error;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use pest::Parser;

#[derive(Parser)]
#[grammar = "expression.pest"]
struct InfixParser;

lazy_static! {
    static ref DEFAULT_VARIABLES: HashMap<String, f64> = {
        let mut m = HashMap::new();
        m.insert(String::from("e"), std::f64::consts::E);
        m.insert(String::from("pi"), std::f64::consts::PI);
        m.insert(String::from("tau"), 2.0 * std::f64::consts::PI);
        m
    };
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Assoc::*;
        use Rule::*;

        PrecClimber::new(vec![
            Operator::new(add, Left) | Operator::new(subtract, Left),
            Operator::new(multiply, Left) | Operator::new(divide, Left),
            Operator::new(power, Right),
        ])
    };
}

// mostly copied from pest calculator example
fn parse_expression(expression: Pairs<Rule>) -> Expr {
    PREC_CLIMBER.climb(
        expression,
        |pair: Pair<Rule>| match pair.as_rule() {
            Rule::num => Expr::Value(pair.as_str().parse::<f64>().unwrap()),
            Rule::expr => parse_expression(pair.into_inner()),
            Rule::var => Expr::Var(pair.as_str().trim().to_string()),
            rule => make_function(rule, pair),
        },
        |lhs: Expr, op: Pair<Rule>, rhs: Expr| match op.as_rule() {
            Rule::add => Expr::Expr(Box::new(Ops::Add(lhs, rhs))),
            Rule::subtract => Expr::Expr(Box::new(Ops::Sub(lhs, rhs))),
            Rule::multiply => Expr::Expr(Box::new(Ops::Mul(lhs, rhs))),
            Rule::divide => Expr::Expr(Box::new(Ops::Div(lhs, rhs))),
            Rule::power => Expr::Expr(Box::new(Ops::Pow(lhs, rhs))),
            _ => unreachable!(),
        },
    )
}

fn make_function(rule: Rule, pair: Pair<Rule>) -> Expr {
    let sub_expression = parse_expression(pair.into_inner());
    match rule {
        Rule::sin => Expr::Expr(Box::new(Ops::Sin(sub_expression))),
        Rule::cos => Expr::Expr(Box::new(Ops::Cos(sub_expression))),
        Rule::tan => Expr::Expr(Box::new(Ops::Tan(sub_expression))),
        _ => unreachable!(),
    }
}

#[derive(Debug, Clone)]
pub struct Variables {
    state: HashMap<String, f64>,
}

impl Variables {
    pub fn get(&self, key: &str) -> Option<&f64> {
        self.state.get(key)
    }

    pub fn insert(&mut self, key: &str, value: f64) -> Option<f64> {
        self.state.insert(String::from(key), value)
    }

    pub fn from_iter<T: IntoIterator<Item = (String, f64)>>(iter: T) -> Variables {
        let mut variables = DEFAULT_VARIABLES.to_owned();
        variables.extend(iter);

        Variables { state: variables }
    }
}

impl std::default::Default for Variables {
    fn default() -> Variables {
        Variables {
            state: DEFAULT_VARIABLES.to_owned(),
        }
    }
}

impl From<HashMap<String, f64>> for Variables {
    fn from(state: HashMap<String, f64>) -> Variables {
        Variables::from_iter(state.into_iter())
    }
}

impl From<BTreeMap<String, f64>> for Variables {
    fn from(state: BTreeMap<String, f64>) -> Variables {
        Variables::from_iter(state.into_iter())
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Value(f64),
    Expr(Box<Ops>),
    Var(String),
}

impl Expr {
    pub fn eval(expression: Expr, vars: &Variables) -> Option<f64> {
        Some(match expression {
            Expr::Expr(op) => Ops::eval(*op, vars)?,
            Expr::Value(x) => x,
            Expr::Var(s) => *vars.get(&s)?,
        })
    }

    pub fn parse(input: &str) -> Result<Expr, Error<Rule>> {
        Ok(parse_expression(InfixParser::parse(
            Rule::calculation,
            input,
        )?))
    }
}

#[derive(Debug, Clone)]
pub enum Ops {
    Add(Expr, Expr),
    Sub(Expr, Expr),
    Mul(Expr, Expr),
    Div(Expr, Expr),
    Pow(Expr, Expr),
    Sin(Expr),
    Cos(Expr),
    Tan(Expr),
}

impl Ops {
    pub fn eval(operator: Ops, vars: &Variables) -> Option<f64> {
        Some(match operator {
            Ops::Add(lhs, rhs) => Expr::eval(lhs, vars)? + Expr::eval(rhs, vars)?,
            Ops::Sub(lhs, rhs) => Expr::eval(lhs, vars)? - Expr::eval(rhs, vars)?,
            Ops::Mul(lhs, rhs) => Expr::eval(lhs, vars)? * Expr::eval(rhs, vars)?,
            Ops::Div(lhs, rhs) => Expr::eval(lhs, vars)? / Expr::eval(rhs, vars)?,
            Ops::Pow(lhs, rhs) => Expr::eval(lhs, vars)?.powf(Expr::eval(rhs, vars)?),

            Ops::Sin(lhs) => Expr::eval(lhs, vars)?.sin(),
            Ops::Cos(lhs) => Expr::eval(lhs, vars)?.cos(),
            Ops::Tan(lhs) => Expr::eval(lhs, vars)?.tan(),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{Expr, Variables};

    #[test]
    fn simple() {
        let parsed = Expr::parse("1 + 5 - 2").unwrap();

        assert_eq!(Some(4.0), Expr::eval(parsed, &Variables::default()));
    }

    #[test]
    fn multiply() {
        let parsed = Expr::parse("5 * 4 * 1.2").unwrap();

        assert_eq!(Some(24.0), Expr::eval(parsed, &Variables::default()));
    }

    #[test]
    fn divide() {
        let parsed = Expr::parse("5 / 4 * 8").unwrap();

        assert_eq!(Some(10.0), Expr::eval(parsed, &Variables::default()));
    }

    #[test]
    fn nested() {
        let parsed = Expr::parse("1 + 5 - ((2 * 8) - 24 / 2)").unwrap();

        assert_eq!(Some(2.0), Expr::eval(parsed, &Variables::default()));
    }

    #[test]
    fn simple_with_variable() {
        let parsed = Expr::parse("1 + 5 - test").unwrap();

        assert_eq!(
            Some(5.25),
            Expr::eval(
                parsed,
                &Variables::from_iter(vec![(String::from("test"), 0.75)])
            )
        );
    }

    #[test]
    fn simple_non_existing_variable() {
        let parsed = Expr::parse("1 + 5 - test").unwrap();

        assert_eq!(None, Expr::eval(parsed, &Variables::default()));
    }

    #[test]
    fn build_in_variable() {
        let parsed = Expr::parse("2*e - pi").unwrap();

        assert_eq!(
            Some(2.0 * std::f64::consts::E - std::f64::consts::PI),
            Expr::eval(parsed, &Variables::default())
        );
    }

    #[test]
    fn override_build_in_variable() {
        let mut vars = std::collections::HashMap::new();
        // for the physics people
        vars.insert(String::from("e"), 3.0);
        vars.insert(String::from("pi"), 3.0);
        vars.insert(String::from("g"), 10.0);

        let parsed = Expr::parse("(2*e - pi) * g").unwrap();

        assert_eq!(Some(30.0), Expr::eval(parsed, &vars.into()));
    }

    #[test]
    fn trigonometric_functions() {
        let parsed = Expr::parse("sin(2) + tan(1) - cos(3)").unwrap();

        assert_eq!(
            Some(2.0f64.sin() + 1.0f64.tan() - 3.0f64.cos()),
            Expr::eval(parsed, &Variables::default())
        );
    }
}
