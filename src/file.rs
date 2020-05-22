use pest::error::Error as PestError;
use pest::iterators::Pairs;
use pest::Parser;

use crate::assignment::parse_assignment;
use crate::grammar::{ExpressionessionParser, Rule};
use crate::string_expression::parse_expression;
use crate::{Assignment, Error, Expression, ExpressionValue, VariableMap};

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

impl std::fmt::Display for ExpressionLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ExpressionLine::*;

        match self {
            Expression(x) => write!(f, "{}", x),
            Assignment(x) => write!(f, "{}", x),
        }
    }
}

impl ExpressionLine {
    pub fn eval<V: VariableMap>(line: Self, vars: &mut V) -> EvalResult {
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

impl std::fmt::Display for ExpressionFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let a: Vec<String> = self.lines.iter().map(|x| x.to_string()).collect();
        write!(f, "{}", a.join("; "))
    }
}

impl ExpressionFile {
    /// Takes input and returns a syntax tree
    pub fn parse(input: &str) -> ParseResult {
        parse_file(ExpressionessionParser::parse(Rule::file, input)?)
    }

    pub fn eval<V: VariableMap>(file: Self, vars: &mut V) -> EvalResult {
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
fn print_expression_file() {
    let expected = "a = (e + pi); a = (a + 12); ((a - e) - pi)";
    let file = ExpressionFile::parse("a = e + pi; a = a + 12; a - e - pi").unwrap();
    assert_eq!(expected, file.to_string());
    let file = ExpressionFile::parse(&file.to_string()).unwrap();
    assert_eq!(expected, file.to_string());
}

#[test]
fn simple() {
    use crate::Variables;
    let file = ExpressionFile::parse("a = 1 + 2; a = a + 12 + 123; a - 1").unwrap();
    let evaluated = ExpressionFile::eval(file, &mut Variables::default());
    assert_eq!(Ok(137.into()), evaluated);
}

#[test]
fn simple_with_default_variables() {
    use crate::Variables;
    let file = ExpressionFile::parse("a = e + pi; a = a + 12; a - e - pi").unwrap();
    let evaluated = ExpressionFile::eval(file, &mut Variables::default());
    assert_eq!(Ok(12.into()), evaluated);
}

#[test]
fn medium() {
    use crate::Variables;
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
