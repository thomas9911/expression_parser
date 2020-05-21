use pest::error::Error as PestError;
use pest::iterators::Pairs;
use pest::Parser;

use crate::assignment::parse_assignment;
use crate::grammar::{ExpressionessionParser, Rule};
use crate::string_expression::parse_expression;
use crate::{Assignment, Error, Expression, ExpressionValue, Variables};

pub type ParseResult = Result<ExpressionFile, PestError<Rule>>;
pub type EvalResult = Result<ExpressionValue, Error>;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ExpressionFile {
    lines: Vec<ExpressionLine>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum ExpressionLine {
    Expression(Expression),
    Assignment(Assignment),
}

impl ExpressionLine {
    pub fn eval(line: Self, vars: &mut Variables) -> EvalResult {
        match line {
            ExpressionLine::Expression(ex) => Expression::eval(ex, vars),
            ExpressionLine::Assignment(ass) => {
                Assignment::eval(ass, vars)?;
                Ok(ExpressionValue::Null)
            }
        }
    }
}

fn parse_file(expression: Pairs<Rule>) -> ParseResult {
    let mut lines = vec![];
    for pair in expression {
        let line = match pair.as_rule() {
            Rule::expr => ExpressionLine::Expression(parse_expression(pair.into_inner())?),
            Rule::assign => ExpressionLine::Assignment(parse_assignment(pair.into_inner())?),
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

impl ExpressionFile {
    /// Takes input and returns a syntax tree
    pub fn parse(input: &str) -> ParseResult {
        parse_file(ExpressionessionParser::parse(Rule::file, input)?)
    }

    pub fn eval(file: Self, vars: &mut Variables) -> EvalResult {
        let mut last = ExpressionValue::Null;
        for line in file.lines {
            last = ExpressionLine::eval(line, vars)?;
        }

        Ok(last)
    }
}

#[test]
fn last_seperator_is_optional() {
    let file1 = ExpressionFile::parse("1 + 2").unwrap();
    let file2 = ExpressionFile::parse("1 + 2;").unwrap();
    assert_eq!(file1, file2);
}

#[test]
fn simple() {
    let file = ExpressionFile::parse("a = 1 + 2; a = a + 12 + 123; a - 1").unwrap();
    let evaluated = ExpressionFile::eval(file, &mut Variables::default());
    assert_eq!(Ok(137.into()), evaluated);
}

#[test]
fn simple_with_default_variables() {
    let file = ExpressionFile::parse("a = e + pi; a = a + 12; a - e - pi").unwrap();
    let evaluated = ExpressionFile::eval(file, &mut Variables::default());
    assert_eq!(Ok(12.into()), evaluated);
}

#[test]
fn medium() {
    let input = r#"
        a = [1, 2, 3];
        b = [3, 2, 1];
        c = concat(a, b);
        d = concat(b, a);
        concat(c, [4,4], d);
    "#;
    let file = ExpressionFile::parse(input).unwrap();
    let evaluated = ExpressionFile::eval(file, &mut Variables::default()).unwrap();
    assert_eq!(
        ExpressionValue::from(vec![1, 2, 3, 3, 2, 1, 4, 4, 3, 2, 1, 1, 2, 3]),
        evaluated
    );
}
