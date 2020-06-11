use pest::error::Error as PestError;
use pest::iterators::Pairs;
use pest::Parser;

use crate::grammar::{ExpressionessionParser, Rule};
use crate::string_expression::parse_expression;
use crate::{Error, Expression, VariableMap};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

pub type ParseResult = Result<Assignment, PestError<Rule>>;
pub type EvalResult = Result<(), Error>;

pub fn parse_assignment(mut assignment: Pairs<Rule>) -> ParseResult {
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

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Assignment {
    variable: String,
    expression: Expression,
}

impl std::fmt::Display for Assignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.variable, self.expression)
    }
}

impl Assignment {
    /// Takes input and returns a syntax tree
    pub fn parse(input: &str) -> ParseResult {
        parse_assignment(
            ExpressionessionParser::parse(Rule::assignment, input)?
                .next()
                .expect("grammar is invalid")
                .into_inner(),
        )
    }

    pub fn eval<V: VariableMap>(assigning: Self, vars: &mut V) -> EvalResult {
        let tmp = Expression::eval(assigning.expression, vars)?;
        vars.insert(&assigning.variable, tmp);
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
    use crate::{Expression, Function, Variables};

    let a = Assignment {
        variable: "a".to_string(),
        expression: Expression::Expr(Box::new(Function::Mul(
            Expression::Value(12.0.into()),
            Expression::Value(15.0.into()),
        ))),
    };

    let mut vars = Variables::default();
    Assignment::eval(a, &mut vars).unwrap();

    assert_eq!(Some(&180.into()), vars.get("a"));
}
