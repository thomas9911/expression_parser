use pest::error::Error as PestError;
use pest::iterators::{Pair, Pairs};
use pest::{Parser, Span};
use std::collections::HashMap;
use std::sync::Arc;

use crate::arc_utils::may_clone;
use crate::function::FunctionName;
use crate::grammar::{create_string, make_pest_error, ExpressionessionParser, Rule};
use crate::statics::{DEFAULT_VARIABLES, PREC_CLIMBER};
use crate::user_function::{parse_user_function, UserFunction};
use crate::{Env, Error, ExpressionMap, ExpressionValue, Function, VariableMap, Variables};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

pub type EvalResult = Result<Arc<ExpressionValue>, Error>;
pub type ParseResult = Result<Expression, PestError<Rule>>;

pub fn parse_expression(expression: Pairs<'_, Rule>) -> ParseResult {
    use Expression::*;

    PREC_CLIMBER.climb(
        expression,
        parse_single_pair,
        |lhs: ParseResult, op: Pair<'_, Rule>, rhs: ParseResult| {
            let lhs = Arc::new(lhs?);
            let rhs = Arc::new(rhs?);

            match op.as_rule() {
                Rule::concat_op => Ok(Expr(Arc::new(Function::Concat(vec![lhs, rhs])))),
                Rule::equal => Ok(Expr(Arc::new(Function::Equal(lhs, rhs)))),
                Rule::not_equal => Ok(Expr(Arc::new(Function::NotEqual(lhs, rhs)))),
                Rule::lesser => Ok(Expr(Arc::new(Function::Lesser(lhs, rhs)))),
                Rule::greater => Ok(Expr(Arc::new(Function::Greater(lhs, rhs)))),
                Rule::and => Ok(Expr(Arc::new(Function::And(lhs, rhs)))),
                Rule::or => Ok(Expr(Arc::new(Function::Or(lhs, rhs)))),
                Rule::add => Ok(Expr(Arc::new(Function::Add(lhs, rhs)))),
                Rule::subtract => Ok(Expr(Arc::new(Function::Sub(lhs, rhs)))),
                Rule::multiply => Ok(Expr(Arc::new(Function::Mul(lhs, rhs)))),
                Rule::divide => Ok(Expr(Arc::new(Function::Div(lhs, rhs)))),
                Rule::power => Ok(Expr(Arc::new(Function::Pow(lhs, rhs)))),
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

fn parse_single_pair(pair: Pair<'_, Rule>) -> ParseResult {
    use Expression::*;

    match pair.as_rule() {
        Rule::num => Ok(Value(
            ExpressionValue::from(pair.as_str().parse::<f64>().unwrap()).into(),
        )),
        Rule::string => match create_string(pair.clone()) {
            Ok(x) => Ok(Value(ExpressionValue::String(x).into())),
            Err(e) => Err(make_pest_error(pair.as_span(), e.to_string())),
        },
        Rule::list => {
            let arguments: Vec<_> = pair.into_inner().try_fold(Vec::new(), |mut acc, x| {
                let p = parse_expression(x.into_inner())?;
                acc.push(Arc::new(p));
                Ok(acc)
            })?;
            Ok(Value(ExpressionValue::List(arguments).into()))
        }
        Rule::map => {
            let iter: Vec<_> = pair.into_inner().collect();
            let mut data = HashMap::new();
            for p in iter.chunks(2) {
                if let Expression::Value(value) = parse_expression(Pairs::single(p[0].clone()))? {
                    let key = value.as_string().unwrap();
                    let val = parse_expression(Pairs::single(p[1].clone()))?;
                    data.insert(key, Arc::new(val));
                } else {
                    unreachable!()
                }
            }
            Ok(Expression::Value(
                ExpressionValue::Map(ExpressionMap(data)).into(),
            ))
        }
        Rule::expr => parse_expression(pair.into_inner()),
        Rule::var => Ok(make_var(pair)),
        Rule::func => make_function(pair),
        Rule::function => Ok(UserFunction(parse_user_function(pair.into_inner())?.into())),
        Rule::dot_function => make_dot_function(pair.into_inner()),
        rule => {
            println!("{:?}", rule);
            unreachable!()
        }
    }
}

fn make_dot_function(mut pairs: Pairs<'_, Rule>) -> ParseResult {
    let first_pair = pairs.next().expect("invalid dot function grammar");

    let lhs = match first_pair.as_rule() {
        Rule::function => {
            Expression::UserFunction(parse_user_function(first_pair.into_inner())?.into())
        }
        Rule::var => make_var(first_pair),
        Rule::func => make_function(first_pair)?,
        _ => panic!("invalid dot function grammar"),
    };

    let _dot_operator = pairs.next().expect("invalid dot function grammar");

    let mut arguments = Vec::new();
    for pair in pairs {
        arguments.push(Arc::new(parse_single_pair(pair)?))
    }

    Ok(Expression::Expr(Arc::new(Function::Call(
        Arc::new(lhs),
        arguments,
    ))))
}

fn make_var(pair: Pair<'_, Rule>) -> Expression {
    Expression::Var(pair.as_str().trim().to_string())
}

fn make_function(pair: Pair<'_, Rule>) -> ParseResult {
    use std::str::FromStr;
    use Expression::Expr;
    use FunctionName::*;

    let pair_span = pair.as_span();
    let mut inner_pairs = pair.into_inner();
    let function_name = inner_pairs.next().expect("function always has a name");

    let mut arguments: Vec<Arc<Expression>> = inner_pairs.try_fold(Vec::new(), |mut acc, x| {
        acc.push(Arc::new(parse_expression(x.into_inner())?));
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
            Expr(Arc::new(Function::Upper(arguments.remove(0))))
        }
        Lower => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Arc::new(Function::Lower(arguments.remove(0))))
        }
        Print => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Arc::new(Function::Print(arguments.remove(0))))
        }
        Help => {
            check_arguments(func_name, pair_span, 0, Some(1), &arguments)?;
            let argument = arguments.pop().unwrap_or_default();
            Expr(Arc::new(Function::Help(argument)))
        }
        Cos => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Arc::new(Function::Cos(arguments.remove(0))))
        }
        Sin => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Arc::new(Function::Sin(arguments.remove(0))))
        }
        Tan => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Arc::new(Function::Tan(arguments.remove(0))))
        }
        Trim => {
            check_arguments(func_name, pair_span, 1, Some(2), &arguments)?;
            Expr(Arc::new(Function::Trim(
                arguments.remove(0),
                arguments
                    .pop()
                    .unwrap_or(Arc::new(Expression::Value(Arc::new(" ".into())))),
            )))
        }
        Contains => {
            check_arguments(func_name, pair_span, 2, Some(2), &arguments)?;
            Expr(Arc::new(Function::Contains(
                arguments.remove(0),
                arguments.remove(0),
            )))
        }
        Join => {
            check_arguments(func_name, pair_span, 2, Some(2), &arguments)?;
            Expr(Arc::new(Function::Join(
                arguments.remove(0),
                arguments.remove(0),
            )))
        }
        Length => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Arc::new(Function::Length(arguments.remove(0))))
        }
        Get => {
            check_arguments(func_name, pair_span, 2, Some(2), &arguments)?;
            Expr(Arc::new(Function::Get(
                arguments.remove(0),
                arguments.remove(0),
            )))
        }
        Push => {
            check_arguments(func_name, pair_span, 2, Some(2), &arguments)?;
            Expr(Arc::new(Function::Push(
                arguments.remove(0),
                arguments.remove(0),
            )))
        }
        Remove => {
            check_arguments(func_name, pair_span, 2, Some(2), &arguments)?;
            Expr(Arc::new(Function::Remove(
                arguments.remove(0),
                arguments.remove(0),
            )))
        }
        Put => {
            check_arguments(func_name, pair_span, 3, Some(3), &arguments)?;
            Expr(Arc::new(Function::Put(
                arguments.remove(0),
                arguments.remove(0),
                arguments.remove(0),
            )))
        }
        If => {
            check_arguments(func_name, pair_span, 3, Some(3), &arguments)?;
            Expr(Arc::new(Function::If(
                arguments.remove(0),
                arguments.remove(0),
                arguments.remove(0),
            )))
        }
        Range => {
            check_arguments(func_name, pair_span, 1, Some(3), &arguments)?;

            let arg3 = arguments.pop();
            let arg2 = arguments.pop();
            let arg1 = arguments.pop();

            let zero = Arc::new(Expression::Value(Arc::new(0.0.into())));
            let one = Arc::new(Expression::Value(Arc::new(1.0.into())));

            let (a, b, c) = match (arg1, arg2, arg3) {
                (None, None, Some(c)) => (zero, c, one),
                (None, Some(b), Some(c)) => (b, c, one),
                (Some(a), Some(b), Some(c)) => (a, b, c),
                (_, _, _) => unreachable!(),
            };

            Expr(Arc::new(Function::Range(a, b, c)))
        }
        Reduce => {
            check_arguments(func_name, pair_span, 3, Some(3), &arguments)?;
            Expr(Arc::new(Function::Reduce(
                arguments.remove(0),
                arguments.remove(0),
                arguments.remove(0),
            )))
        }
        Shuffle => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Arc::new(Function::Shuffle(arguments.remove(0))))
        }
        Concat => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            Expr(Arc::new(Function::Concat(arguments)))
        }
        Sum => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            Expr(Arc::new(Function::Sum(arguments)))
        }
        Product => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            Expr(Arc::new(Function::Product(arguments)))
        }
        All => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            Expr(Arc::new(Function::All(arguments)))
        }
        Any => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            Expr(Arc::new(Function::Any(arguments)))
        }
        Random => {
            check_arguments(func_name, pair_span, 0, Some(2), &arguments)?;

            let default_a = Arc::new(Expression::Value(Arc::new(0.0.into())));
            let default_b = Arc::new(Expression::Value(Arc::new(1.0.into())));

            let arg2 = arguments.pop();
            let arg1 = arguments.pop();

            let (a, b) = match arg2 {
                None => (default_a, default_b),
                Some(x) => match arg1 {
                    None => (default_a, x),
                    Some(y) => (x, y),
                },
            };

            Expr(Arc::new(Function::Random(a, b)))
        }
        Try => {
            check_arguments(func_name, pair_span, 2, Some(2), &arguments)?;
            Expr(Arc::new(Function::Try(
                arguments.remove(0),
                arguments.remove(0),
            )))
        }
        Type => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Arc::new(Function::Type(arguments.remove(0))))
        }
        Error => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Arc::new(Function::Error(arguments.remove(0))))
        }
        Assert => {
            check_arguments(func_name, pair_span, 1, Some(2), &arguments)?;
            Expr(Arc::new(Function::Assert(
                arguments.remove(0),
                arguments
                    .pop()
                    .unwrap_or(Arc::new(Expression::Value(Arc::new(
                        "assertion failed".into(),
                    )))),
            )))
        }
        Now => {
            check_arguments(func_name, pair_span, 0, Some(0), &arguments)?;
            Expr(Arc::new(Function::Now()))
        }
        Format => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            let template = arguments.remove(0);

            Expr(Arc::new(Function::Format(template, arguments)))
        }
        Call => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            let func = arguments.remove(0);

            Expr(Arc::new(Function::Call(func, arguments)))
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
    span: Span<'_>,
    min_args: usize,
    max_args: Option<usize>,
    args: &Vec<Arc<Expression>>,
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

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Expression {
    Value(Arc<ExpressionValue>),
    Expr(Arc<Function>),
    Var(String),
    UserFunction(Arc<UserFunction>),
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
    pub fn eval<'a, 'b, E: Env<'a> + std::fmt::Debug>(
        expression: Expression,
        env: &'b mut E,
    ) -> Result<ExpressionValue, Error> {
        Self::eval_rc(Arc::new(expression), env).map(may_clone)
    }

    pub fn eval_rc<'a, 'b, E: Env<'a> + std::fmt::Debug>(
        expression: Arc<Expression>,
        env: &'b mut E,
    ) -> EvalResult {
        match &*expression {
            Expression::Expr(op) => Function::eval_rc(op.clone(), env),
            Expression::Value(value) => match &**value {
                ExpressionValue::List(list) => {
                    let value_list = list
                        .clone()
                        .into_iter()
                        .try_fold::<_, _, Result<_, Error>>(Vec::new(), |mut acc, x| {
                            acc.push(Arc::new(Expression::Value(Expression::eval_rc(x, env)?)));
                            Ok(acc)
                        })?;
                    Ok(ExpressionValue::List(value_list).into())
                }
                ExpressionValue::Map(ExpressionMap(map)) => {
                    let value_map = map.clone().into_iter().try_fold::<_, _, Result<_, Error>>(
                        HashMap::new(),
                        |mut acc, (k, v)| {
                            acc.insert(
                                k,
                                Arc::new(Expression::Value(Expression::eval_rc(v, env)?)),
                            );
                            Ok(acc)
                        },
                    )?;
                    Ok(ExpressionValue::Map(ExpressionMap(value_map)).into())
                }
                _ => Ok(value.clone()),
            },
            Expression::Var(s) => match env.variables().get_arc(&s) {
                Some(x) => Ok(x),
                None => Err(Error::new(format!("Variable {} not found", &s))),
            },
            Expression::UserFunction(function) => {
                let vars = env.variables();
                let mut compiled = if let Some(compiled) = vars.local() {
                    compiled
                } else {
                    Variables::new()
                };

                for local_var in function.iter_variables() {
                    if let Some(var) = vars.get(&local_var) {
                        compiled.insert(&local_var, var.to_owned());
                    }
                }

                Ok(ExpressionValue::Function(function.clone(), compiled).into())
            }
        }
    }

    /// Takes input and returns a syntax tree
    pub fn parse(input: &str) -> Result<Expression, PestError<Rule>> {
        parse_expression(ExpressionessionParser::parse(Rule::calculation, input)?)
    }

    pub fn compile(expression: Expression) -> Result<Expression, Error> {
        // match expression {
        //     Expression::Value(value) => match &*value {
        //         ExpressionValue::List(list) => Ok(Expression::Value(
        //             ExpressionValue::List(list.into_iter().try_fold::<_, _, Result<_, Error>>(
        //                 Vec::new(),
        //                 |mut acc, x| {
        //                     acc.push(Expression::compile(*x)?.into());
        //                     Ok(acc)
        //                 },
        //             )?)
        //             .into(),
        //         )),
        //         x => Ok(Expression::Value(value)),
        //     },
        //     Expression::Expr(op) => Function::compile(*op),
        //     other => Ok(other),
        // }
        Ok(Expression::default())
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
        Expression::Value(Arc::new(input))
    }
}

impl From<ExpressionMap> for Expression {
    fn from(input: ExpressionMap) -> Expression {
        Expression::Value(Arc::new(ExpressionValue::Map(input)))
    }
}

#[test]
fn unicode_json() {
    use crate::Environment;

    let parsed = Expression::parse(r#""testing \u1F996""#).unwrap();
    let evaluated = Expression::eval(parsed, &mut Environment::default()).unwrap();

    assert_eq!(r#""testing 🦖""#, evaluated.to_string());
}

#[test]
fn unicode_rust() {
    use crate::Environment;

    let parsed = Expression::parse(r#""testing \u{1F980}""#).unwrap();
    let evaluated = Expression::eval(parsed, &mut Environment::default()).unwrap();

    assert_eq!(r#""testing 🦀""#, evaluated.to_string());
}
