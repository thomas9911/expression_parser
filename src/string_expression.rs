use pest::error::{Error as PestError, ErrorVariant};
use pest::iterators::{Pair, Pairs};
use pest::{Parser, Span};
use std::collections::HashMap;

use crate::function::FunctionName;
use crate::grammar::{ExpressionessionParser, Rule};
use crate::statics::{DEFAULT_VARIABLES, PREC_CLIMBER};
use crate::user_function::{parse_user_function, UserFunction};
use crate::{Environment, Error, ExpressionMap, ExpressionValue, Function, VariableMap, Variables};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

pub type EvalResult = Result<ExpressionValue, Error>;
pub type ParseResult = Result<Expression, PestError<Rule>>;

pub fn parse_expression(expression: Pairs<Rule>) -> ParseResult {
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
                Rule::lesser => Ok(Expr(Box::new(Function::Lesser(lhs, rhs)))),
                Rule::greater => Ok(Expr(Box::new(Function::Greater(lhs, rhs)))),
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
                    println!("{:?}, {:?}", lhs, rhs);
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
        Rule::string => match snailquote::unescape(&make_string(pair.clone().into_inner())) {
            Ok(x) => Ok(Value(ExpressionValue::String(x))),
            Err(e) => Err(make_pest_error(pair.as_span(), e.to_string())),
        },
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
        Rule::var => Ok(make_var(pair)),
        Rule::func => make_function(pair),
        Rule::function => Ok(UserFunction(parse_user_function(pair.into_inner())?)),
        Rule::dot_function => make_dot_function(pair.into_inner()),
        rule => {
            println!("{:?}", rule);
            unreachable!()
        }
    }
}

fn make_dot_function(mut pairs: Pairs<Rule>) -> ParseResult {
    let first_pair = pairs.next().expect("invalid dot function grammar");

    let lhs = match first_pair.as_rule() {
        Rule::function => Expression::UserFunction(parse_user_function(first_pair.into_inner())?),
        Rule::var => make_var(first_pair),
        Rule::func => make_function(first_pair)?,
        _ => panic!("ivalid dot function grammar"),
    };

    let _dot_operator = pairs.next().expect("invalid dot function grammar");

    let mut arguments = Vec::new();
    for pair in pairs {
        arguments.push(parse_single_pair(pair)?)
    }

    Ok(Expression::Expr(Box::new(Function::Call(lhs, arguments))))
}

fn make_var(pair: Pair<Rule>) -> Expression {
    Expression::Var(pair.as_str().trim().to_string())
}

fn make_string(string: Pairs<Rule>) -> String {
    let mut buffer = String::from("\"");
    for pair in string {
        match pair.as_rule() {
            Rule::unicode_code => buffer.push_str(&format!(r"\u{{{}}}", pair.as_str())),
            _ => buffer.push_str(pair.as_str()),
        }
    }
    buffer.push('"');
    buffer
}

fn make_function(pair: Pair<Rule>) -> ParseResult {
    use std::str::FromStr;
    use Expression::Expr;
    use FunctionName::*;

    let pair_span = pair.as_span();
    let mut inner_pairs = pair.into_inner();
    let function_name = inner_pairs.next().expect("function always has a name");

    let mut arguments: Vec<Expression> = inner_pairs.try_fold(Vec::new(), |mut acc, x| {
        acc.push(parse_expression(x.into_inner())?);
        Ok(acc)
    })?;
    let func_name = match FunctionName::from_str(function_name.as_str()) {
        Ok(x) => x,
        _ => {
            return Err(make_pest_error(
                pair_span,
                format!("function {:?} is not defined", function_name.as_str()),
            ))
        }
    };

    Ok(match func_name {
        Upper => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Box::new(Function::Upper(arguments.remove(0))))
        }
        Lower => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Box::new(Function::Lower(arguments.remove(0))))
        }
        Print => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Box::new(Function::Print(arguments.remove(0))))
        }
        Help => {
            check_arguments(func_name, pair_span, 0, Some(1), &arguments)?;
            let argument = arguments.pop().unwrap_or_default();
            Expr(Box::new(Function::Help(argument)))
        }
        Cos => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Box::new(Function::Cos(arguments.remove(0))))
        }
        Sin => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Box::new(Function::Sin(arguments.remove(0))))
        }
        Tan => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Box::new(Function::Tan(arguments.remove(0))))
        }
        Trim => {
            check_arguments(func_name, pair_span, 1, Some(2), &arguments)?;
            Expr(Box::new(Function::Trim(
                arguments.remove(0),
                arguments.pop().unwrap_or(Expression::Value(" ".into())),
            )))
        }
        Contains => {
            check_arguments(func_name, pair_span, 2, Some(2), &arguments)?;
            Expr(Box::new(Function::Contains(
                arguments.remove(0),
                arguments.remove(0),
            )))
        }
        Join => {
            check_arguments(func_name, pair_span, 2, Some(2), &arguments)?;
            Expr(Box::new(Function::Join(
                arguments.remove(0),
                arguments.remove(0),
            )))
        }
        Get => {
            check_arguments(func_name, pair_span, 2, Some(2), &arguments)?;
            Expr(Box::new(Function::Get(
                arguments.remove(0),
                arguments.remove(0),
            )))
        }
        Push => {
            check_arguments(func_name, pair_span, 2, Some(2), &arguments)?;
            Expr(Box::new(Function::Push(
                arguments.remove(0),
                arguments.remove(0),
            )))
        }
        Remove => {
            check_arguments(func_name, pair_span, 2, Some(2), &arguments)?;
            Expr(Box::new(Function::Remove(
                arguments.remove(0),
                arguments.remove(0),
            )))
        }
        Put => {
            check_arguments(func_name, pair_span, 3, Some(3), &arguments)?;
            Expr(Box::new(Function::Put(
                arguments.remove(0),
                arguments.remove(0),
                arguments.remove(0),
            )))
        }
        If => {
            check_arguments(func_name, pair_span, 3, Some(3), &arguments)?;
            Expr(Box::new(Function::If(
                arguments.remove(0),
                arguments.remove(0),
                arguments.remove(0),
            )))
        }
        Concat => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            Expr(Box::new(Function::Concat(arguments)))
        }
        Sum => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            Expr(Box::new(Function::Sum(arguments)))
        }
        Product => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            Expr(Box::new(Function::Product(arguments)))
        }
        All => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            Expr(Box::new(Function::All(arguments)))
        }
        Any => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            Expr(Box::new(Function::Any(arguments)))
        }
        Random => {
            check_arguments(func_name, pair_span, 0, Some(2), &arguments)?;

            let default_a = Expression::Value(0.0.into());
            let default_b = Expression::Value(1.0.into());

            let arg2 = arguments.pop();
            let arg1 = arguments.pop();

            let (a, b) = match arg1 {
                None => (default_a, default_b),
                Some(x) => match arg2 {
                    None => (default_a, x),
                    Some(y) => (x, y),
                },
            };

            Expr(Box::new(Function::Random(a, b)))
        }
        Try => {
            check_arguments(func_name, pair_span, 2, Some(2), &arguments)?;
            Expr(Box::new(Function::Try(
                arguments.remove(0),
                arguments.remove(0),
            )))
        }
        Type => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Box::new(Function::Type(arguments.remove(0))))
        }
        Error => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Box::new(Function::Error(arguments.remove(0))))
        }
        Assert => {
            check_arguments(func_name, pair_span, 1, Some(2), &arguments)?;
            Expr(Box::new(Function::Assert(
                arguments.remove(0),
                arguments
                    .pop()
                    .unwrap_or(Expression::Value("assertion failed".into())),
            )))
        }
        Now => {
            check_arguments(func_name, pair_span, 0, Some(0), &arguments)?;
            Expr(Box::new(Function::Now()))
        }
        Format => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            let template = arguments.remove(0);

            Expr(Box::new(Function::Format(template, arguments)))
        }
        Call => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            let func = arguments.remove(0);

            Expr(Box::new(Function::Call(func, arguments)))
        }

        // infix functions
        Equal | NotEqual | Greater | Lesser | Add | Sub | Mul | Div | Pow | And | Or => {
            return Err(make_pest_error(
                pair_span,
                format!("function {:?} is not defined", function_name.as_str()),
            ))
        }
    })
}

fn check_arguments(
    function_name: FunctionName,
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
    UserFunction(UserFunction),
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Expression::*;
        match self {
            Value(val) => write!(f, "{}", val),
            Var(val) => write!(f, "{}", val),
            Expr(val) => write!(f, "{}", val),
            UserFunction(val) => write!(f, "{}", val),
        }
    }
}

impl Default for Expression {
    fn default() -> Self {
        Expression::Value(Default::default())
    }
}

impl Expression {
    /// evaluate the syntax tree with given variables and returns a 'ExpressionValue'
    pub fn eval(expression: Expression, env: &Environment) -> EvalResult {
        match expression {
            Expression::Expr(op) => Function::eval(*op, env),
            Expression::Value(value) => match value {
                ExpressionValue::List(list) => Ok(ExpressionValue::List(
                    list.into_iter().try_fold::<_, _, Result<_, Error>>(
                        Vec::new(),
                        |mut acc, x| {
                            acc.push(Expression::Value(Expression::eval(x, env)?));
                            Ok(acc)
                        },
                    )?,
                )),
                ExpressionValue::Map(ExpressionMap(map)) => {
                    Ok(ExpressionValue::Map(ExpressionMap({
                        map.into_iter().try_fold::<_, _, Result<_, Error>>(
                            HashMap::new(),
                            |mut acc, (k, v)| {
                                acc.insert(k, Expression::Value(Expression::eval(v, env)?));
                                Ok(acc)
                            },
                        )?
                    })))
                }
                x => Ok(x),
            },
            Expression::Var(s) => match env.variables.get(&s) {
                Some(x) => Ok(x.clone()),
                None => Err(Error::new(format!("Variable {} not found", &s))),
            },
            Expression::UserFunction(function) => {
                let mut compiled = if let Some(compiled) = env.variables.local() {
                    compiled
                } else {
                    Variables::new()
                };

                for local_var in function.iter_variables() {
                    if let Some(var) = env.variables.get(&local_var) {
                        compiled.insert(&local_var, var.to_owned());
                    }
                }

                Ok(ExpressionValue::Function(function, compiled))
            }
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
                    list.into_iter().try_fold::<_, _, Result<_, Error>>(
                        Vec::new(),
                        |mut acc, x| {
                            acc.push(Expression::compile(x)?);
                            Ok(acc)
                        },
                    )?,
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
            Expression::UserFunction(func) => func.expression.iter_variables(),
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

impl From<ExpressionValue> for Expression {
    fn from(input: ExpressionValue) -> Expression {
        Expression::Value(input)
    }
}

impl From<ExpressionMap> for Expression {
    fn from(input: ExpressionMap) -> Expression {
        Expression::Value(ExpressionValue::Map(input))
    }
}

#[test]
fn unicode_json() {
    let parsed = Expression::parse(r#""testing \u1F996""#).unwrap();
    let evaluated = Expression::eval(parsed, &Environment::default()).unwrap();

    assert_eq!(r#""testing 🦖""#, evaluated.to_string());
}

#[test]
fn unicode_rust() {
    let parsed = Expression::parse(r#""testing \u{1F980}""#).unwrap();
    let evaluated = Expression::eval(parsed, &Environment::default()).unwrap();

    assert_eq!(r#""testing 🦀""#, evaluated.to_string());
}
