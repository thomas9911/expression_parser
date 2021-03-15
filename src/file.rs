use pest::error::Error as PestError;
use pest::iterators::Pairs;
use pest::Parser;

use crate::assignment::{parse_assignment, parse_unassignment};
use crate::error::ErrorCodes;
use crate::grammar::{ExpressionessionParser, Rule};
use crate::string_expression::parse_expression;
use crate::{Assignment, Env, Error, Expression, ExpressionValue, Unassignment};

/// minimal amount of stacksize needed in bytes
const MINIMAL_STACKSIZE: usize = 65536;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

pub type ParseResult = Result<ExpressionFile, PestError<Rule>>;
pub type EvalResult = Result<ExpressionValue, Error>;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ExpressionFile {
    pub(crate) lines: Vec<ExpressionLine>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum ExpressionLine {
    Expression(Expression),
    Assignment(Assignment),
    Unassignment(Unassignment),
}

impl std::fmt::Display for ExpressionLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ExpressionLine::*;

        match self {
            Expression(x) => write!(f, "{}", x),
            Assignment(x) => write!(f, "{}", x),
            Unassignment(x) => write!(f, "{}", x),
        }
    }
}

impl ExpressionLine {
    pub fn eval<'a, 'b, E: Env<'a>>(line: Self, env: &'b mut E) -> EvalResult {
        // vars.check_recursion_limit()?;
        check_stack()?;

        match line {
            ExpressionLine::Expression(Expression::UserFunction(function)) => {
                for item in function.iter_variables() {
                    let val = if let Some(val) = env.variables().get(&item) {
                        val.to_owned()
                    } else {
                        continue;
                    };
                    env.variables_mut().insert(&item, val);
                }
                Expression::eval(Expression::UserFunction(function), env)
            }
            ExpressionLine::Expression(ex) => Expression::eval(ex, env),
            ExpressionLine::Assignment(ass) => {
                Assignment::eval(ass, env)?;
                Ok(ExpressionValue::Null)
            }
            ExpressionLine::Unassignment(un) => {
                Unassignment::eval(un, env)?;
                Ok(ExpressionValue::Null)
            }
        }
    }

    pub fn iter_variables<'a>(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        use ExpressionLine::*;
        match self {
            Expression(ex) => ex.iter_variables(),
            Assignment(_) | Unassignment(_) => Box::new(std::iter::empty::<String>()),
        }
    }
}

fn check_stack() -> Result<(), Error> {
    if let Some(x) = stacker::remaining_stack() {
        if x > MINIMAL_STACKSIZE {
            return Ok(());
        } else {
            return Err(Error::new_code(ErrorCodes::STACKOVERFLOW));
        }
    }
    // on not supported targets always return ok (for instance wasm)
    Ok(())
}

pub fn parse_file(expression: Pairs<'_, Rule>) -> ParseResult {
    let mut lines = vec![];
    for pair in expression {
        let line = match pair.as_rule() {
            Rule::expr => ExpressionLine::Expression(parse_expression(pair.into_inner())?),
            Rule::assign => ExpressionLine::Assignment(parse_assignment(pair.into_inner())?),
            Rule::unassign => ExpressionLine::Unassignment(parse_unassignment(pair.into_inner())?),
            Rule::EOI => continue,
            rule => {
                println!("{:?}", rule);
                unreachable!()
            }
        };
        lines.push(line);
    }
    Ok(ExpressionFile { lines: lines })
}

impl std::fmt::Display for ExpressionFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let a: Vec<String> = self.lines.iter().map(|x| x.to_string()).collect();
        write!(f, "{}", a.join("; "))
    }
}

impl Default for ExpressionFile {
    fn default() -> ExpressionFile {
        ExpressionFile { lines: Vec::new() }
    }
}

impl ExpressionFile {
    /// Takes input and returns a syntax tree
    pub fn parse(input: &str) -> ParseResult {
        parse_file(ExpressionessionParser::parse(Rule::file, input)?)
    }

    pub fn iter_variables<'a>(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        Box::new(self.lines.iter().flat_map(|x| x.iter_variables()))
    }

    pub fn eval<'a, 'b, E: Env<'a>>(file: Self, env: &'b mut E) -> EvalResult {
        let mut last = ExpressionValue::Null;
        for line in file.lines {
            last = ExpressionLine::eval(line, env)?;
        }

        Ok(last)
    }

    pub fn run<'a, 'b, E: Env<'a>>(input: &str, env: &'b mut E) -> EvalResult {
        let file = Self::parse(input)?;
        Self::eval(file, env)
    }
}

#[test]
fn last_seperator_is_optional() {
    let file1 = ExpressionFile::parse("1 + 2").unwrap();
    let file2 = ExpressionFile::parse("1 + 2;").unwrap();
    assert_eq!(file1, file2);
}

#[test]
fn print_expression_file() {
    let expected = "a = (e + pi); a = (a + 12); ((a - e) - pi)";
    let file = ExpressionFile::parse("a = e + pi; a = a + 12; a - e - pi").unwrap();
    assert_eq!(expected, file.to_string());
    let file = ExpressionFile::parse(&file.to_string()).unwrap();
    assert_eq!(expected, file.to_string());
}

#[test]
fn simple() {
    use crate::Environment;
    let file = ExpressionFile::parse("a = 1 + 2; a = a + 12 + 123; a - 1").unwrap();
    let evaluated = ExpressionFile::eval(file, &mut Environment::default());
    assert_eq!(Ok(137.into()), evaluated);
}

#[test]
fn simple_with_default_variables() {
    use crate::Environment;
    let file = ExpressionFile::parse("a = e + pi; a = a + 12; a - e - pi").unwrap();
    let evaluated = ExpressionFile::eval(file, &mut Environment::default());
    assert_eq!(Ok(12.into()), evaluated);
}

#[test]
fn medium() {
    use crate::Environment;
    let input = r#"
        a = [1, 2, 3];
        b = [3, 2, 1];
        c = concat(a, b);
        d = concat(b, a);
        concat(c, [4,4], d);
    "#;
    let file = ExpressionFile::parse(input).unwrap();
    let evaluated = ExpressionFile::eval(file, &mut Environment::default()).unwrap();
    assert_eq!(
        ExpressionValue::from(vec![1, 2, 3, 3, 2, 1, 4, 4, 3, 2, 1, 1, 2, 3]),
        evaluated
    );
}

#[test]
fn unassign() {
    use crate::Environment;
    let file = ExpressionFile::parse("a = 1 + 2").unwrap();
    let mut env = Environment::default();
    let _evaluated = ExpressionFile::eval(file, &mut env);

    assert_eq!(Some(&3.0.into()), env.variables().get("a"));

    let file = ExpressionFile::parse("unset a").unwrap();
    let _evaluated = ExpressionFile::eval(file, &mut env);
    assert_eq!(None, env.variables().get("a"));
}
