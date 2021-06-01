use pest::error::Error as PestError;
use pest::iterators::Pairs;
use pest::Parser;

use crate::grammar::{ExpressionessionParser, Rule};
use crate::string_expression::parse_expression;
use crate::{Env, Error, Expression};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

pub type AssignmentParseResult = Result<Assignment, PestError<Rule>>;
pub type UnassignmentParseResult = Result<Unassignment, PestError<Rule>>;
pub type EvalResult = Result<(), Error>;

pub fn parse_assignment(mut assignment: Pairs<'_, Rule>) -> AssignmentParseResult {
    let var = assignment
        .next()
        .expect("grammar of assignment is incorrect")
        .as_str();
    let expression = parse_expression(
        assignment
            .next()
            .expect("grammar of assignment is incorrect")
            .into_inner(),
    )?;

    Ok(Assignment {
        variable: String::from(var),
        expression: expression,
    })
}

pub fn parse_unassignment(mut assignment: Pairs<'_, Rule>) -> UnassignmentParseResult {
    let var = assignment
        .next()
        .expect("grammar of unassignment is incorrect")
        .as_str();

    Ok(Unassignment {
        variable: String::from(var),
    })
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Assignment {
    pub(crate) variable: String,
    pub(crate) expression: Expression,
}

impl std::fmt::Display for Assignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.variable, self.expression)
    }
}

impl Assignment {
    /// Takes input and returns a syntax tree
    pub fn parse(input: &str) -> AssignmentParseResult {
        parse_assignment(
            ExpressionessionParser::parse(Rule::assignment, input)?
                .next()
                .expect("grammar is invalid")
                .into_inner(),
        )
    }

    pub fn eval<'a, 'b, V: Env<'a>>(assigning: Self, env: &'b mut V) -> EvalResult {
        let tmp = Expression::eval(assigning.expression, env)?;

        env.variables_mut().insert(&assigning.variable, tmp);
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Unassignment {
    variable: String,
}

impl std::fmt::Display for Unassignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "unset {}", self.variable)
    }
}

impl Unassignment {
    /// Takes input and returns a syntax tree
    pub fn parse(input: &str) -> UnassignmentParseResult {
        parse_unassignment(
            ExpressionessionParser::parse(Rule::assignment, input)?
                .next()
                .expect("grammar is invalid")
                .into_inner(),
        )
    }

    pub fn eval<'a, 'b, V: Env<'a>>(assigning: Self, env: &mut V) -> EvalResult {
        env.variables_mut().remove(&assigning.variable);
        Ok(())
    }
}

#[test]
fn assignment_parse() {
    use crate::{Expression, Function};

    let a = Assignment::parse("a = 12 * 15").unwrap();

    assert_eq!("a = (12 * 15)", a.to_string());

    let expected = Assignment {
        variable: "a".to_string(),
        expression: Expression::Expr(Box::new(Function::Mul(
            Expression::Value(12.0.into()),
            Expression::Value(15.0.into()),
        ))),
    };

    assert_eq!(expected, a);
}

#[test]
fn assignment_add_to_variables() {
    use crate::{Environment, Expression, Function};

    let a = Assignment {
        variable: "a".to_string(),
        expression: Expression::Expr(Box::new(Function::Mul(
            Expression::Value(12.0.into()),
            Expression::Value(15.0.into()),
        ))),
    };

    let mut env = Environment::default();
    Assignment::eval(a, &mut env).unwrap();

    assert_eq!(Some(&180.into()), env.variables().get("a"));
}

#[test]
fn unassignment_parse() {
    let a = Unassignment::parse("unset a").unwrap();

    assert_eq!("unset a", a.to_string());

    let expected = Unassignment {
        variable: "a".to_string(),
    };

    assert_eq!(expected, a);
}
