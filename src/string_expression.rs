use pest::error::{Error as PestError, ErrorVariant};
use pest::iterators::{Pair, Pairs};
use pest::{Parser, Span};
use std::collections::HashMap;

use crate::grammar::{ExpressionessionParser, Rule};
use crate::statics::{DEFAULT_VARIABLES, PREC_CLIMBER};
use crate::{Error, ExpressionMap, ExpressionValue, Function, Variables};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

pub type EvalResult = Result<ExpressionValue, Error>;
pub type ParseResult = Result<Expression, PestError<Rule>>;

fn parse_expression(expression: Pairs<Rule>) -> ParseResult {
    use Expression::*;

    PREC_CLIMBER.climb(
        expression,
        parse_single_pair,
        |lhs: ParseResult, op: Pair<Rule>, rhs: ParseResult| {
            let lhs = lhs?;
            let rhs = rhs?;

            match op.as_rule() {
                Rule::concat_op => Ok(Expr(Box::new(Function::Concat(vec![lhs, rhs])))),
                Rule::equal => Ok(Expr(Box::new(Function::Equal(lhs, rhs)))),
                Rule::not_equal => Ok(Expr(Box::new(Function::NotEqual(lhs, rhs)))),
                Rule::and => Ok(Expr(Box::new(Function::And(lhs, rhs)))),
                Rule::or => Ok(Expr(Box::new(Function::Or(lhs, rhs)))),
                Rule::add => Ok(Expr(Box::new(Function::Add(lhs, rhs)))),
                Rule::subtract => Ok(Expr(Box::new(Function::Sub(lhs, rhs)))),
                Rule::multiply => Ok(Expr(Box::new(Function::Mul(lhs, rhs)))),
                Rule::divide => Ok(Expr(Box::new(Function::Div(lhs, rhs)))),
                Rule::power => Ok(Expr(Box::new(Function::Pow(lhs, rhs)))),
                // _ => unreachable!(),
                rule => {
                    println!("{:?}", rule);
                    unreachable!()
                }
            }
        },
    )
}

fn parse_single_pair(pair: Pair<Rule>) -> ParseResult {
    use Expression::*;

    match pair.as_rule() {
        Rule::num => Ok(Value(pair.as_str().parse::<f64>().unwrap().into())),
        Rule::string => Ok(Value(ExpressionValue::String(
            pair.as_str().trim_matches('"').to_string(),
        ))),
        Rule::list => {
            let arguments: Vec<Expression> =
                pair.into_inner().try_fold(Vec::new(), |mut acc, x| {
                    let p = parse_expression(x.into_inner())?;
                    acc.push(p);
                    Ok(acc)
                })?;
            Ok(Value(ExpressionValue::List(arguments)))
        }
        Rule::map => {
            let iter: Vec<_> = pair.into_inner().collect();
            let mut data = HashMap::new();
            for p in iter.chunks(2) {
                if let Expression::Value(ExpressionValue::String(key)) =
                    parse_expression(Pairs::single(p[0].clone()))?
                {
                    let val = parse_expression(Pairs::single(p[1].clone()))?;
                    data.insert(key, val);
                } else {
                    unreachable!()
                }
            }
            Ok(Expression::Value(ExpressionValue::Map(ExpressionMap(data))))
        }
        Rule::expr => parse_expression(pair.into_inner()),
        Rule::var => Ok(Var(pair.as_str().trim().to_string())),
        Rule::func => make_function(pair),
        rule => {
            println!("{:?}", rule);
            unreachable!()
        }
    }
}

fn make_function(pair: Pair<Rule>) -> ParseResult {
    use Expression::Expr;

    let pair_span = pair.as_span();
    let mut inner_pairs = pair.into_inner();
    let function_name = inner_pairs.next().expect("function always has a name");

    let arguments: Vec<Expression> = inner_pairs.try_fold(Vec::new(), |mut acc, x| {
        acc.push(parse_expression(x.into_inner())?);
        Ok(acc)
    })?;
    let func_name = function_name.as_str();

    Ok(match func_name {
        "upper" => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Box::new(Function::Upper(arguments[0].clone())))
        }
        "lower" => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Box::new(Function::Lower(arguments[0].clone())))
        }
        "cos" => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Box::new(Function::Cos(arguments[0].clone())))
        }
        "sin" => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Box::new(Function::Sin(arguments[0].clone())))
        }
        "tan" => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Box::new(Function::Tan(arguments[0].clone())))
        }
        "trim" => {
            check_arguments(func_name, pair_span, 1, Some(2), &arguments)?;
            Expr(Box::new(Function::Trim(
                arguments[0].clone(),
                arguments
                    .get(1)
                    .unwrap_or(&Expression::Value(String::from(" ").into()))
                    .clone(),
            )))
        }
        "contains" => {
            check_arguments(func_name, pair_span, 2, Some(2), &arguments)?;
            Expr(Box::new(Function::Contains(
                arguments[0].clone(),
                arguments[1].clone(),
            )))
        }
        "if" => {
            check_arguments(func_name, pair_span, 3, Some(3), &arguments)?;
            Expr(Box::new(Function::If(
                arguments[0].clone(),
                arguments[1].clone(),
                arguments[2].clone(),
            )))
        }
        "concat" => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            Expr(Box::new(Function::Concat(arguments)))
        }
        "sum" => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            Expr(Box::new(Function::Sum(arguments)))
        }
        "product" => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            Expr(Box::new(Function::Product(arguments)))
        }
        "all" => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            Expr(Box::new(Function::All(arguments)))
        }
        "any" => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            Expr(Box::new(Function::Any(arguments)))
        }
        "random" | "rnd" => {
            check_arguments(func_name, pair_span, 0, Some(2), &arguments)?;

            let default_a = &Expression::Value(0.0.into());
            let default_b = &Expression::Value(1.0.into());

            let (a, b) = match arguments.get(0) {
                None => (default_a, default_b),
                Some(x) => match arguments.get(1) {
                    None => (default_a, x),
                    Some(y) => (x, y),
                },
            };

            Expr(Box::new(Function::Random(a.to_owned(), b.to_owned())))
        }
        _ => {
            return Err(make_pest_error(
                pair_span,
                format!("function {:?} is not defined", func_name),
            ));
        }
    })
}

fn check_arguments(
    function_name: &str,
    span: Span,
    min_args: usize,
    max_args: Option<usize>,
    args: &Vec<Expression>,
) -> Result<(), PestError<Rule>> {
    if min_args > args.len() {
        return Err(make_pest_error(
            span,
            format!(
                "function {:?} should have more than {} arguments",
                function_name, min_args
            ),
        ));
    };

    if let Some(max_len) = max_args {
        if args.len() > max_len {
            return Err(make_pest_error(
                span,
                format!(
                    "function {:?} should have less than {} arguments",
                    function_name, max_len
                ),
            ));
        };
    };

    Ok(())
}

fn make_pest_error(span: Span, message: String) -> PestError<Rule> {
    let variant = ErrorVariant::<Rule>::CustomError { message: message };
    PestError::new_from_span(variant, span)
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Expression {
    Value(ExpressionValue),
    Expr(Box<Function>),
    Var(String),
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Expression::*;
        match self {
            Value(val) => write!(f, "{}", val),
            Var(val) => write!(f, "{}", val),
            Expr(val) => write!(f, "{}", val),
        }
    }
}

impl Expression {
    /// evaluate the syntax tree with given variables and returns a 'ExpressionValue'
    pub fn eval(expression: Expression, vars: &Variables) -> EvalResult {
        match expression {
            Expression::Expr(op) => Function::eval(*op, vars),
            Expression::Value(value) => match value {
                ExpressionValue::List(list) => Ok(ExpressionValue::List(
                    list.into_iter().try_fold(Vec::new(), |mut acc, x| {
                        acc.push(Expression::Value(Expression::eval(x, vars)?));
                        Ok(acc)
                    })?,
                )),
                ExpressionValue::Map(ExpressionMap(map)) => {
                    Ok(ExpressionValue::Map(ExpressionMap({
                        map.into_iter()
                            .try_fold(HashMap::new(), |mut acc, (k, v)| {
                                acc.insert(k, Expression::Value(Expression::eval(v, vars)?));
                                Ok(acc)
                            })?
                    })))
                }
                x => Ok(x),
            },
            // Expression::Var(s) => vars.get(&s).clone(),
            Expression::Var(s) => match vars.get(&s) {
                Some(x) => Ok(x.clone()),
                None => Err(Error::new(format!("Variable {} not found", &s))),
            },
        }
    }

    /// Takes input and returns a syntax tree
    pub fn parse(input: &str) -> Result<Expression, PestError<Rule>> {
        parse_expression(ExpressionessionParser::parse(Rule::calculation, input)?)
    }

    pub fn compile(expression: Expression) -> Result<Expression, Error> {
        match expression {
            Expression::Value(value) => match value {
                ExpressionValue::List(list) => Ok(Expression::Value(ExpressionValue::List(
                    list.iter().try_fold(Vec::new(), |mut acc, x| {
                        acc.push(Expression::compile(x.clone())?);
                        Ok(acc)
                    })?,
                ))),
                x => Ok(Expression::Value(x)),
            },
            Expression::Expr(op) => Function::compile(*op),
            other => Ok(other),
        }
    }

    /// returns all variables defined in the expression
    pub fn iter_variables<'a>(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        match self {
            Expression::Value(_) => Box::new(std::iter::empty::<String>()),
            Expression::Var(x) => Box::new(std::iter::once(x.to_owned())),
            Expression::Expr(f) => f.iter_variables(),
        }
    }

    /// returns variables that don't have a default value, so these values are still not known
    pub fn iter_variables_without_defaults<'a>(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        Box::new(
            self.iter_variables()
                .filter(|x| !DEFAULT_VARIABLES.contains_key(x)),
        )
    }

    pub fn is_value(&self) -> bool {
        use Expression::Value;
        match self {
            Value(_) => true,
            _ => false,
        }
    }
}
