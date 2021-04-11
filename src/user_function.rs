use pest::error::Error as PestError;
use pest::iterators::Pairs;
use std::sync::Arc;

use crate::file::parse_file;
use crate::grammar::Rule;
use crate::{Error, ExpressionFile};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

pub type FunctionParseResult = Result<UserFunction, PestError<Rule>>;
pub type EvalResult = Result<(), Error>;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct UserFunction {
    pub arguments: Vec<String>,
    pub expression: Arc<ExpressionFile>,
}

pub fn parse_user_function(mut assignment: Pairs<'_, Rule>) -> FunctionParseResult {
    let vars: Vec<String> = assignment
        .next()
        .expect("grammar of function is incorrect")
        .into_inner()
        .map(|x| x.as_str().to_owned())
        .collect();

    let expr = parse_file(assignment)?;

    Ok(UserFunction {
        arguments: vars,
        expression: Arc::new(expr),
    })
}

impl std::fmt::Display for UserFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{{} => {}}}",
            self.arguments.join(", "),
            self.expression
        )
    }
}

impl UserFunction {
    pub fn iter_variables<'a>(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        Box::new(
            self.expression
                .iter_variables()
                .filter(move |x| !self.arguments.contains(x)),
        )
    }

    pub fn expression(self: Arc<Self>) -> Arc<ExpressionFile> {
        self.expression.clone()
    }
}

#[test]
fn function_parse() {
    use crate::file::ExpressionLine;
    use crate::{Expression, ExpressionFile, Function};

    let file = ExpressionFile::parse("{x, y => 1 + 1}").unwrap();

    let inner_lines = vec![ExpressionLine::Expression(
        Expression::Expr(Arc::new(Function::Add(
            Expression::Value(Arc::new(1.0.into())).into(),
            Expression::Value(Arc::new(1.0.into())).into(),
        )))
        .into(),
    )
    .into()];
    let user_function = Expression::UserFunction(
        UserFunction {
            arguments: vec!["x".to_string(), "y".to_string()],
            expression: Arc::new(ExpressionFile { lines: inner_lines }),
        }
        .into(),
    );
    let lines = vec![ExpressionLine::Expression(Arc::new(user_function)).into()];
    let expected = ExpressionFile { lines: lines };

    assert_eq!(expected, file);
}

#[test]
fn function_parse_no_variables() {
    use crate::file::ExpressionLine;
    use crate::{Expression, ExpressionFile, Function};

    let file = ExpressionFile::parse("{ => 1 + 1}").unwrap();
    let inner_lines = vec![ExpressionLine::Expression(
        Expression::Expr(Arc::new(Function::Add(
            Expression::Value(Arc::new(1.0.into())).into(),
            Expression::Value(Arc::new(1.0.into())).into(),
        )))
        .into(),
    )
    .into()];
    let user_function = Expression::UserFunction(
        UserFunction {
            arguments: vec![],
            expression: Arc::new(ExpressionFile { lines: inner_lines }),
        }
        .into(),
    );
    let lines = vec![ExpressionLine::Expression(Arc::new(user_function)).into()];
    let expected = ExpressionFile { lines: lines };

    assert_eq!(expected, file);
}
