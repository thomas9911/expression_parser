use std::collections::{BTreeMap, HashMap};

use pest::error::{Error as PestError, ErrorVariant};
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use pest::{Parser, Span};

use crate::Error;

mod functions;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[derive(Parser)]
#[grammar = "string_expression.pest"]
struct ExpressionessionParser;

type EvalResult = Result<ExpressionValue, Error>;

lazy_static! {
    static ref DEFAULT_VARIABLES: HashMap<String, ExpressionValue> = {
        let mut m = HashMap::new();
        m.insert(String::from("true"), ExpressionValue::Bool(true));
        m.insert(String::from("false"), ExpressionValue::Bool(false));
        m.insert(
            String::from("e"),
            ExpressionValue::Number(std::f64::consts::E),
        );
        m.insert(
            String::from("pi"),
            ExpressionValue::Number(std::f64::consts::PI),
        );
        m.insert(
            String::from("tau"),
            ExpressionValue::Number(2.0 * std::f64::consts::PI),
        );
        m
    };
    static ref DEFAULT_STRING_VARIABLES: Variables = { Variables::default() };
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Assoc::*;
        use Rule::*;

        PrecClimber::new(vec![
            Operator::new(concat_op, Left),
            Operator::new(equal, Right),
            Operator::new(not_equal, Right),
            Operator::new(and, Left),
            Operator::new(or, Left),
            Operator::new(add, Left) | Operator::new(subtract, Left),
            Operator::new(multiply, Left) | Operator::new(divide, Left),
            Operator::new(power, Right),
        ])
    };
}

// mostly copied from pest calculator example
fn parse_expression(expression: Pairs<Rule>) -> Result<Expression, PestError<Rule>> {
    use Expression::*;

    PREC_CLIMBER.climb(
        expression,
        |pair: Pair<Rule>| match pair.as_rule() {
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
            Rule::expr => parse_expression(pair.into_inner()),
            Rule::var => Ok(Var(pair.as_str().trim().to_string())),
            Rule::func => make_function(pair),
            rule => {
                println!("{:?}", rule);
                unreachable!()
            }
        },
        |lhs: Result<Expression, PestError<Rule>>,
         op: Pair<Rule>,
         rhs: Result<Expression, PestError<Rule>>| {
            let lhs = lhs?;
            let rhs = rhs?;

            match op.as_rule() {
                Rule::concat_op => Ok(Expr(Box::new(Functions::Concat(vec![lhs, rhs])))),
                Rule::equal => Ok(Expr(Box::new(Functions::Equal(lhs, rhs)))),
                Rule::not_equal => Ok(Expr(Box::new(Functions::NotEqual(lhs, rhs)))),
                Rule::and => Ok(Expr(Box::new(Functions::And(lhs, rhs)))),
                Rule::or => Ok(Expr(Box::new(Functions::Or(lhs, rhs)))),
                Rule::add => Ok(Expr(Box::new(Functions::Add(lhs, rhs)))),
                Rule::subtract => Ok(Expr(Box::new(Functions::Sub(lhs, rhs)))),
                Rule::multiply => Ok(Expr(Box::new(Functions::Mul(lhs, rhs)))),
                Rule::divide => Ok(Expr(Box::new(Functions::Div(lhs, rhs)))),
                Rule::power => Ok(Expr(Box::new(Functions::Pow(lhs, rhs)))),
                // _ => unreachable!(),
                rule => {
                    println!("{:?}", rule);
                    unreachable!()
                }
            }
        },
    )
}

fn make_function(pair: Pair<Rule>) -> Result<Expression, PestError<Rule>> {
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
            Expr(Box::new(Functions::Upper(arguments[0].clone())))
        }
        "lower" => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Box::new(Functions::Lower(arguments[0].clone())))
        }
        "cos" => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Box::new(Functions::Cos(arguments[0].clone())))
        }
        "sin" => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Box::new(Functions::Sin(arguments[0].clone())))
        }
        "tan" => {
            check_arguments(func_name, pair_span, 1, Some(1), &arguments)?;
            Expr(Box::new(Functions::Tan(arguments[0].clone())))
        }
        "trim" => {
            check_arguments(func_name, pair_span, 1, Some(2), &arguments)?;
            Expr(Box::new(Functions::Trim(
                arguments[0].clone(),
                arguments
                    .get(1)
                    .unwrap_or(&Expression::Value(String::from(" ").into()))
                    .clone(),
            )))
        }
        "contains" => {
            check_arguments(func_name, pair_span, 2, Some(2), &arguments)?;
            Expr(Box::new(Functions::Contains(
                arguments[0].clone(),
                arguments[1].clone(),
            )))
        }
        "if" => {
            check_arguments(func_name, pair_span, 3, Some(3), &arguments)?;
            Expr(Box::new(Functions::If(
                arguments[0].clone(),
                arguments[1].clone(),
                arguments[2].clone(),
            )))
        }
        "concat" => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            Expr(Box::new(Functions::Concat(arguments)))
        }
        "sum" => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            Expr(Box::new(Functions::Sum(arguments)))
        }
        "product" => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            Expr(Box::new(Functions::Product(arguments)))
        }
        "all" => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            Expr(Box::new(Functions::All(arguments)))
        }
        "any" => {
            check_arguments(func_name, pair_span, 1, None, &arguments)?;
            Expr(Box::new(Functions::Any(arguments)))
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

            Expr(Box::new(Functions::Random(a.to_owned(), b.to_owned())))
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

#[derive(Debug, Clone)]
pub struct Variables {
    state: HashMap<String, ExpressionValue>,
}

impl Variables {
    pub fn get(&self, key: &str) -> Option<&ExpressionValue> {
        self.state.get(key)
    }

    pub fn insert<V>(&mut self, key: &str, value: V) -> Option<ExpressionValue>
    where
        V: Into<ExpressionValue>,
    {
        self.state.insert(String::from(key), value.into())
    }

    pub fn from_iter<T: IntoIterator<Item = (String, ExpressionValue)>>(iter: T) -> Variables {
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

impl From<HashMap<String, ExpressionValue>> for Variables {
    fn from(state: HashMap<String, ExpressionValue>) -> Variables {
        Variables::from_iter(state.into_iter())
    }
}

impl From<BTreeMap<String, ExpressionValue>> for Variables {
    fn from(state: BTreeMap<String, ExpressionValue>) -> Variables {
        Variables::from_iter(state.into_iter())
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Expression {
    Value(ExpressionValue),
    Expr(Box<Functions>),
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
            Expression::Expr(op) => Functions::eval(*op, vars),
            Expression::Value(value) => match value {
                ExpressionValue::List(list) => Ok(ExpressionValue::List(
                    list.into_iter().try_fold(Vec::new(), |mut acc, x| {
                        acc.push(Expression::Value(Expression::eval(x, vars)?));
                        Ok(acc)
                    })?,
                )),
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
            Expression::Expr(op) => Functions::compile(*op),
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

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Functions {
    Concat(Vec<Expression>),
    Sum(Vec<Expression>),
    Product(Vec<Expression>),
    All(Vec<Expression>),
    Any(Vec<Expression>),
    Equal(Expression, Expression),
    NotEqual(Expression, Expression),
    And(Expression, Expression),
    Or(Expression, Expression),
    Trim(Expression, Expression),
    Contains(Expression, Expression),
    If(Expression, Expression, Expression),
    Upper(Expression),
    Lower(Expression),
    Add(Expression, Expression),
    Sub(Expression, Expression),
    Mul(Expression, Expression),
    Div(Expression, Expression),
    Pow(Expression, Expression),
    Cos(Expression),
    Sin(Expression),
    Tan(Expression),
    Random(Expression, Expression),
}

impl std::fmt::Display for Functions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Functions::*;
        match self {
            Concat(list) => write!(f, "concat({})", list_to_string(list).join(", ")),
            Sum(list) => write!(f, "sum({})", list_to_string(list).join(", ")),
            Product(list) => write!(f, "product({})", list_to_string(list).join(", ")),
            All(list) => write!(f, "all({})", list_to_string(list).join(", ")),
            Any(list) => write!(f, "any({})", list_to_string(list).join(", ")),
            Trim(lhs, rhs) => write!(f, "trim({}, {})", lhs, rhs),
            Equal(lhs, rhs) => write!(f, "({} == {})", lhs, rhs),
            NotEqual(lhs, rhs) => write!(f, "({} != {})", lhs, rhs),
            And(lhs, rhs) => write!(f, "({} and {})", lhs, rhs),
            Or(lhs, rhs) => write!(f, "({} or {})", lhs, rhs),
            Contains(lhs, rhs) => write!(f, "contains({}, {})", lhs, rhs),
            If(lhs, mdl, rhs) => write!(f, "if({}, {}, {})", lhs, mdl, rhs),
            Upper(lhs) => write!(f, "upper({})", lhs),
            Lower(lhs) => write!(f, "lower({})", lhs),
            Add(lhs, rhs) => write!(f, "({} + {})", lhs, rhs),
            Sub(lhs, rhs) => write!(f, "({} - {})", lhs, rhs),
            Mul(lhs, rhs) => write!(f, "({} * {})", lhs, rhs),
            Div(lhs, rhs) => write!(f, "({} / {})", lhs, rhs),
            Pow(lhs, rhs) => write!(f, "({} ^ {})", lhs, rhs),
            Cos(lhs) => write!(f, "cos({})", lhs),
            Sin(lhs) => write!(f, "sin({})", lhs),
            Tan(lhs) => write!(f, "tan({})", lhs),
            Random(lhs, rhs) => write!(f, "random({}, {})", lhs, rhs),
        }
    }
}

impl Functions {
    pub fn eval(operator: Functions, vars: &Variables) -> EvalResult {
        use Functions::*;

        match operator {
            Concat(list) => functions::concat(list, vars),
            Sum(list) => functions::sum(list, vars),
            Product(list) => functions::product(list, vars),
            All(list) => functions::all(list, vars),
            Any(list) => functions::any(list, vars),
            Trim(lhs, rhs) => functions::trim(lhs, rhs, vars),
            Equal(lhs, rhs) => functions::equal(lhs, rhs, vars),
            NotEqual(lhs, rhs) => functions::not_equal(lhs, rhs, vars),
            And(lhs, rhs) => functions::and(lhs, rhs, vars),
            Or(lhs, rhs) => functions::or(lhs, rhs, vars),
            Contains(lhs, rhs) => functions::contains(lhs, rhs, vars),
            If(lhs, mdl, rhs) => functions::if_function(lhs, mdl, rhs, vars),
            Upper(lhs) => functions::upper(lhs, vars),
            Lower(lhs) => functions::lower(lhs, vars),
            Add(lhs, rhs) => functions::add(lhs, rhs, vars),
            Sub(lhs, rhs) => functions::sub(lhs, rhs, vars),
            Mul(lhs, rhs) => functions::mul(lhs, rhs, vars),
            Div(lhs, rhs) => functions::div(lhs, rhs, vars),
            Pow(lhs, rhs) => functions::pow(lhs, rhs, vars),
            Cos(lhs) => functions::cos(lhs, vars),
            Sin(lhs) => functions::sin(lhs, vars),
            Tan(lhs) => functions::tan(lhs, vars),
            Random(lhs, rhs) => functions::random(lhs, rhs, vars),
        }
    }

    pub fn compile(operator: Functions) -> Result<Expression, Error> {
        use Functions::*;

        if operator.contains_variables() | operator.cannot_be_pre_evaluated() {
            let funcs = match operator.to_owned() {
                Concat(list) => Concat(Functions::compile_list(list)?),
                Sum(list) => Sum(Functions::compile_list(list)?),
                Product(list) => Product(Functions::compile_list(list)?),
                All(list) => All(Functions::compile_list(list)?),
                Any(list) => Any(Functions::compile_list(list)?),
                Trim(lhs, rhs) => Trim(Expression::compile(lhs)?, Expression::compile(rhs)?),
                Equal(lhs, rhs) => Equal(Expression::compile(lhs)?, Expression::compile(rhs)?),
                NotEqual(lhs, rhs) => {
                    NotEqual(Expression::compile(lhs)?, Expression::compile(rhs)?)
                }
                And(lhs, rhs) => And(Expression::compile(lhs)?, Expression::compile(rhs)?),
                Or(lhs, rhs) => Or(Expression::compile(lhs)?, Expression::compile(rhs)?),
                Contains(lhs, rhs) => {
                    Contains(Expression::compile(lhs)?, Expression::compile(rhs)?)
                }
                If(lhs, mdl, rhs) => If(
                    Expression::compile(lhs)?,
                    Expression::compile(mdl)?,
                    Expression::compile(rhs)?,
                ),
                Upper(lhs) => Upper(Expression::compile(lhs)?),
                Lower(lhs) => Lower(Expression::compile(lhs)?),
                Add(lhs, rhs) => Add(Expression::compile(lhs)?, Expression::compile(rhs)?),
                Sub(lhs, rhs) => Sub(Expression::compile(lhs)?, Expression::compile(rhs)?),
                Mul(lhs, rhs) => Mul(Expression::compile(lhs)?, Expression::compile(rhs)?),
                Div(lhs, rhs) => Div(Expression::compile(lhs)?, Expression::compile(rhs)?),
                Pow(lhs, rhs) => Pow(Expression::compile(lhs)?, Expression::compile(rhs)?),
                Cos(lhs) => Cos(Expression::compile(lhs)?),
                Sin(lhs) => Sin(Expression::compile(lhs)?),
                Tan(lhs) => Tan(Expression::compile(lhs)?),
                Random(lhs, rhs) => Random(lhs, rhs),
            };

            Ok(Expression::Expr(Box::new(funcs)))
        } else {
            Ok(Expression::Value(Functions::eval(
                operator,
                &DEFAULT_STRING_VARIABLES,
            )?))
        }
    }

    fn compile_list(list: Vec<Expression>) -> Result<Vec<Expression>, Error> {
        list.into_iter().try_fold(Vec::new(), |mut acc, x| {
            acc.push(Expression::compile(x)?);
            Ok(acc)
        })
    }

    fn cannot_be_pre_evaluated(&self) -> bool {
        match self {
            Functions::Random(_, _) => true,
            _ => false,
        }
    }

    fn contains_variables(&self) -> bool {
        self.iter_variables_without_defaults().count() != 0
    }

    pub fn iter_variables<'a>(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        use Functions::*;

        match self {
            Trim(lhs, rhs)
            | Equal(lhs, rhs)
            | NotEqual(lhs, rhs)
            | And(lhs, rhs)
            | Or(lhs, rhs)
            | Contains(lhs, rhs)
            | Add(lhs, rhs)
            | Sub(lhs, rhs)
            | Mul(lhs, rhs)
            | Div(lhs, rhs)
            | Pow(lhs, rhs)
            | Random(lhs, rhs) => Box::new(lhs.iter_variables().chain(rhs.iter_variables())),

            Lower(lhs) | Upper(lhs) | Cos(lhs) | Sin(lhs) | Tan(lhs) => {
                Box::new(lhs.iter_variables())
            }

            If(lhs, mdl, rhs) => Box::new(
                lhs.iter_variables()
                    .chain(mdl.iter_variables())
                    .chain(rhs.iter_variables()),
            ),

            Concat(list) | Product(list) | Sum(list) | All(list) | Any(list) => {
                Box::new(list.iter().flat_map(|x| x.iter_variables()))
            }
        }
    }

    pub fn iter_variables_without_defaults<'a>(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        Box::new(
            self.iter_variables()
                .filter(|x| !DEFAULT_VARIABLES.contains_key(x)),
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum ExpressionValue {
    String(String),
    Bool(bool),
    Number(f64),
    List(Vec<Expression>),
}

impl std::fmt::Display for ExpressionValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ExpressionValue::{Bool, List, Number};

        match self {
            ExpressionValue::String(x) => write!(f, "\"{}\"", x),
            Bool(x) => write!(f, "{}", x),
            Number(x) => write!(f, "{}", x),
            List(list) => write!(f, "[ {} ]", list_to_string(list).join(", ")),
        }
    }
}

fn list_to_string(input: &Vec<Expression>) -> Vec<String> {
    input.iter().map(|x| format!("{}", x)).collect()
}

macro_rules! impl_from_integers {
    ($type: ty) => {
        impl From<$type> for ExpressionValue {
            fn from(input: $type) -> ExpressionValue {
                ExpressionValue::Number(input as f64)
            }
        }
    };
}

impl From<String> for ExpressionValue {
    fn from(input: String) -> ExpressionValue {
        ExpressionValue::from(input.as_str())
    }
}

impl From<&str> for ExpressionValue {
    fn from(input: &str) -> ExpressionValue {
        if let Ok(x) = input.to_lowercase().parse::<bool>() {
            return ExpressionValue::Bool(x);
        }
        ExpressionValue::String(String::from(input))
    }
}

impl From<bool> for ExpressionValue {
    fn from(input: bool) -> ExpressionValue {
        ExpressionValue::Bool(input)
    }
}

impl From<Vec<ExpressionValue>> for ExpressionValue {
    fn from(input: Vec<ExpressionValue>) -> ExpressionValue {
        let expressions = input
            .iter()
            .map(|x| Expression::Value(x.to_owned()))
            .collect();
        ExpressionValue::List(expressions)
    }
}

impl_from_integers!(f32);
impl_from_integers!(f64);

impl_from_integers!(u8);
impl_from_integers!(u16);
impl_from_integers!(u32);
impl_from_integers!(u64);
impl_from_integers!(usize);

impl_from_integers!(i8);
impl_from_integers!(i16);
impl_from_integers!(i32);
impl_from_integers!(i64);
impl_from_integers!(isize);

impl ExpressionValue {
    /// casts value as a number
    pub fn as_number(&self) -> Option<f64> {
        use ExpressionValue::*;

        match self {
            Number(x) => Some(*x),
            _ => None,
        }
    }

    /// casts value as a number, if the value was a boolean returns 0, for false, or 1, for true.
    pub fn as_number_or_boolean(&self) -> Option<f64> {
        use ExpressionValue::*;

        match self {
            Number(x) => Some(*x),
            Bool(true) => Some(1.0),
            Bool(false) => Some(0.0),
            _ => None,
        }
    }

    /// casts value as a boolean
    pub fn as_bool(&self) -> Option<bool> {
        use ExpressionValue::*;

        match self {
            Bool(x) => Some(*x),
            _ => None,
        }
    }

    /// casts value as a string
    pub fn as_string(&self) -> Option<String> {
        use ExpressionValue::*;

        match self {
            String(x) => Some(x.to_owned()),
            _ => None,
        }
    }

    /// casts value as a list
    pub fn as_list(&self) -> Option<Vec<Expression>> {
        use ExpressionValue::*;

        match self {
            List(x) => Some(x.to_owned()),
            _ => None,
        }
    }

    pub fn is_number(&self) -> bool {
        use ExpressionValue::*;

        match self {
            Number(_) => true,
            _ => false,
        }
    }

    pub fn is_number_or_boolean(&self) -> bool {
        use ExpressionValue::*;

        match self {
            Number(_) | Bool(_) => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        use ExpressionValue::*;

        match self {
            Bool(_) => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        use ExpressionValue::*;

        match self {
            String(_) => true,
            _ => false,
        }
    }

    pub fn is_list(&self) -> bool {
        use ExpressionValue::*;

        match self {
            List(_) => true,
            _ => false,
        }
    }

    pub fn is_falsy(&self) -> bool {
        use ExpressionValue::*;
        match self {
            String(string) => string == "",
            Bool(b) => !b,
            Number(float) => nearly_zero(float),
            List(list) => list.is_empty(),
        }
    }

    pub fn is_truthy(&self) -> bool {
        !self.is_falsy()
    }

    pub fn and(self, other: ExpressionValue) -> ExpressionValue {
        if self.is_truthy() {
            other
        } else {
            self
        }
    }

    pub fn or(self, other: ExpressionValue) -> ExpressionValue {
        if self.is_truthy() {
            self
        } else {
            other
        }
    }
}

fn nearly_zero(number: &f64) -> bool {
    if number > &0.0 {
        return false;
    }
    if number < &0.0 {
        return false;
    }
    true
}

#[test]
fn test_nearly_zero() {
    assert!(nearly_zero(&0.0));
}

#[test]
fn test_not_nearly_zero() {
    assert!(!nearly_zero(&1.5e-12));
    assert!(!nearly_zero(&-1.5e-12));
}

#[cfg(test)]
mod tests {
    mod iter_variables {
        use crate::Expression;

        #[test]
        fn simple_expression() {
            let parsed = Expression::parse(r#"true | false"#).unwrap();
            let result: Vec<String> = parsed.iter_variables().collect();
            assert_eq!(result, ["true", "false"]);
        }

        #[test]
        fn advanced_expression() {
            let parsed = Expression::parse(
                r#"
                upper(
                    concat(
                        1234, 
                        "_", 
                        contains(
                            "test", 
                            something
                        ) or trim(user_input, "_"),
                        "_", 
                        true
                    )
                )
            "#,
            )
            .unwrap();
            let result: Vec<String> = parsed.iter_variables().collect();
            assert_eq!(result, ["something", "user_input", "true"]);
        }

        #[test]
        fn simple_expression_without_defaults() {
            let parsed = Expression::parse(r#"true | false | test"#).unwrap();
            let result: Vec<String> = parsed.iter_variables_without_defaults().collect();
            assert_eq!(result, ["test"]);
        }
    }

    mod or {
        use crate::{Expression, ExpressionValue, Variables};

        #[test]
        fn operator_true() {
            let parsed = Expression::parse(r#"true | false"#).unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok(true.into()), result);
        }

        #[test]
        fn operator_false() {
            let parsed = Expression::parse(r#"false | false"#).unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok(false.into()), result);
        }

        #[test]
        fn operator_false_string() {
            let parsed = Expression::parse(r#"false | "test""#).unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok("test".into()), result);
        }

        #[test]
        fn operator_true_string() {
            let parsed = Expression::parse(r#"true | "test""#).unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok(true.into()), result);
        }

        #[test]
        fn operator_false_number_string() {
            let parsed = Expression::parse(r#"0 | "test""#).unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok("test".into()), result);
        }

        #[test]
        fn operator_true_number_string() {
            let parsed = Expression::parse(r#"1 | "test""#).unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok(1.0f64.into()), result);
        }

        #[test]
        fn operator_string_string() {
            let parsed = Expression::parse(r#""test" | "other""#).unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok("test".into()), result);
        }

        #[test]
        fn operator_vec_string() {
            use Expression::Value;
            use ExpressionValue::{List, Number};

            let parsed = Expression::parse(r#"[1] | "other""#).unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok(List(vec![Value(Number(1.0))])), result);
        }

        #[test]
        fn operator_empty_vec_string() {
            let parsed = Expression::parse(r#"[] | "other""#).unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok("other".into()), result);
        }
    }

    mod and {
        use crate::{Expression, ExpressionValue, Variables};

        #[test]
        fn operator_false() {
            let parsed = Expression::parse(r#"true & false"#).unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok(false.into()), result);
        }

        #[test]
        fn operator_false_2() {
            let parsed = Expression::parse(r#"false & false"#).unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok(false.into()), result);
        }

        #[test]
        fn operator_true() {
            let parsed = Expression::parse(r#"true & true"#).unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok(true.into()), result);
        }

        #[test]
        fn operator_number_false() {
            let parsed = Expression::parse(r#"5 & false"#).unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok(false.into()), result);
        }

        #[test]
        fn operator_number_true() {
            let parsed = Expression::parse(r#"1 & true"#).unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok(true.into()), result);
        }

        #[test]
        fn operator_false_string() {
            let parsed = Expression::parse(r#"false & "test""#).unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok(false.into()), result);
        }

        #[test]
        fn operator_true_string() {
            let parsed = Expression::parse(r#"true & "test""#).unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok("test".into()), result);
        }

        #[test]
        fn operator_string_string() {
            let parsed = Expression::parse(r#""test" & "other""#).unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok("other".into()), result);
        }

        #[test]
        fn operator_string_string_2() {
            let parsed = Expression::parse(r#"false & "other""#).unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok(false.into()), result);
        }

        #[test]
        fn operator_vec_string() {
            let parsed = Expression::parse(r#"[1] & "other""#).unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok("other".into()), result);
        }

        #[test]
        fn operator_empty_vec_string() {
            use ExpressionValue::List;
            let parsed = Expression::parse(r#"[] & "other""#).unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok(List(vec![])), result);
        }
    }

    mod number {
        use crate::{Expression, Variables};

        #[test]
        fn simple_addition() {
            let parsed = Expression::parse("1 + 2").unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok(3.into()), result);
        }

        #[test]
        fn simple_subtraction() {
            let parsed = Expression::parse("1 - 2").unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok((-1).into()), result);
        }

        #[test]
        fn simple_multiplication() {
            let parsed = Expression::parse("1 * 2").unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok(2.into()), result);
        }

        #[test]
        fn simple_division() {
            let parsed = Expression::parse("1 / 2").unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok(0.5.into()), result);
        }

        #[test]
        fn simple_power() {
            let parsed = Expression::parse("2^2").unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok(4.into()), result);
        }

        #[test]
        fn cosine() {
            let parsed = Expression::parse("cos(0)").unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok(1.into()), result);
        }

        #[test]
        fn sine() {
            let parsed = Expression::parse("sin(0)").unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok(0.into()), result);
        }

        #[test]
        fn tangent() {
            let parsed = Expression::parse("tan(0)").unwrap();
            let result = Expression::eval(parsed, &Variables::default());
            assert_eq!(Ok(0.into()), result);
        }

        #[test]
        fn sine_invalid_argments() {
            assert!(Expression::parse("sin()").is_err());
            assert!(Expression::parse("sin(1,2,3)").is_err());
        }

        #[test]
        fn combine_functions() {
            let parsed =
                Expression::parse(r#"3*((((1 + 2) / (3**2) + 5) - 2 + 5) * (2 / 4) * 4) / 2.5"#)
                    .unwrap();
            let result = Expression::eval(parsed, &Variables::default())
                .unwrap()
                .as_number()
                .unwrap();
            assert_eq!(20.0, result.round());
        }
    }

    use crate::{Expression, Variables};

    #[test]
    fn equal_operator_true() {
        let parsed = Expression::parse(r#""test" == "test""#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(true.into()), result);
    }

    #[test]
    fn equal_operator_false() {
        let parsed = Expression::parse(r#""test" == "other""#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(false.into()), result);
    }

    #[test]
    fn not_equal_operator_true() {
        let parsed = Expression::parse(r#""test" != "test""#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(false.into()), result);
    }

    #[test]
    fn not_equal_operator_false() {
        let parsed = Expression::parse(r#""test" != "other""#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(true.into()), result);
    }

    #[test]
    fn concat_operator() {
        // let parsed = Expression::parse(r#""test" ++ "test""#).unwrap();
        let parsed = match Expression::parse(r#""test" ++ "test""#) {
            Ok(x) => x,
            Err(e) => {
                println!("{}", e);
                panic!("error")
            }
        };
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok("testtest".into()), result);
    }

    #[test]
    fn concat_function() {
        let parsed = Expression::parse(r#"concat("other", "test")"#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok("othertest".into()), result);
    }

    #[test]
    fn concat_function_multi() {
        let parsed = Expression::parse(r#"concat("1", 2, "3", "4")"#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok("1234".into()), result);
    }

    #[test]
    fn concat_function_variables() {
        let parsed = Expression::parse(r#"concat(test, "-", other, third, "!!")"#).unwrap();

        let mut vars = Variables::default();
        vars.insert("test", "1");
        vars.insert("other", "test");
        vars.insert("third", "3456");

        let result = Expression::eval(parsed, &vars);
        assert_eq!(Ok("1-test3456!!".into()), result);
    }

    #[test]
    fn concat_function_one() {
        let parsed = Expression::parse(r#"concat("test")"#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok("test".into()), result);
    }

    #[test]
    fn concat_function_list() {
        let parsed = Expression::parse(r#"[1, 4, 5] ++ [2, 3]"#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(
            Ok(vec![1.into(), 4.into(), 5.into(), 2.into(), 3.into()].into()),
            result
        );
    }

    #[test]
    fn upper() {
        let parsed = Expression::parse(r#"upper("test")"#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok("TEST".into()), result);
    }

    #[test]
    fn trim() {
        let parsed = Expression::parse(r#"trim("..test...............", ".")"#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok("test".into()), result);
    }

    #[test]
    fn contains_true() {
        let parsed = Expression::parse(r#"contains("test", "test")"#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(true.into()), result);
    }

    #[test]
    fn contains_false() {
        let parsed = Expression::parse(r#"contains("test", "something")"#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(false.into()), result);
    }

    #[test]
    fn if_truthy() {
        let parsed = Expression::parse(r#"if("test", "left", "right")"#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok("left".into()), result);
    }

    #[test]
    fn if_falsy() {
        let parsed = Expression::parse(r#"if([], "left", "right")"#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok("right".into()), result);
    }

    #[test]
    fn combine_functions() {
        let parsed = Expression::parse(
            r#"
            upper(
                concat(
                    1234, 
                    "_", 
                    contains(
                        "test", 
                        "something"
                    ) or trim("__testing", "_"),
                    "_", 
                    true
                )
            )
        "#,
        )
        .unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok("1234_TESTING_TRUE".into()), result);
    }

    #[test]
    fn compile_simple() {
        let parsed = Expression::parse(r#"trim("..test...............", ".")"#).unwrap();
        let compiled = Expression::compile(parsed);
        assert_eq!("\"test\"", compiled.unwrap().to_string());

        let parsed = Expression::parse(r#"trim("..test...............", a)"#).unwrap();
        let compiled = Expression::compile(parsed).unwrap();
        assert_eq!("trim(\"..test...............\", a)", compiled.to_string());
    }

    #[test]
    fn compile_medium() {
        let parsed = Expression::parse(
            r#"
            upper(
                concat(
                    1234, 
                    "_", 
                    contains(
                        "test", 
                        "something"
                    ) or trim("__testing", "_"),
                    if("", "!", "_"), 
                    all([1], true, 12, "oke")
                )
            )
        "#,
        )
        .unwrap();
        let compiled = Expression::compile(parsed).unwrap();
        assert_eq!("\"1234_TESTING_TRUE\"", compiled.to_string());

        let parsed = Expression::parse(
            r#"
            upper(
                concat(
                    1234, 
                    "_", 
                    contains(
                        "test", 
                        "something"
                    ) or trim("__testing", a),
                    if("", "!", "_"), 
                    all([1], true, 12, "oke")
                )
            )
        "#,
        )
        .unwrap();
        let compiled = Expression::compile(parsed).unwrap();
        assert_eq!(
            "upper(concat(1234, \"_\", (false or trim(\"__testing\", a)), \"_\", true))",
            compiled.to_string()
        );
    }

    #[test]
    fn compile_random() {
        let parsed = Expression::parse("random()").unwrap();

        let compiled = Expression::compile(parsed).unwrap();
        assert_eq!("random(0, 1)", compiled.to_string());
    }

    #[test]
    fn compile_calculation() {
        let parsed =
            Expression::parse("1 + 2 + 3 + 4 + sin(pi) + e ^ 2 + abc * 8 - e ^ pi / 2 - abc")
                .unwrap();
        assert_eq!(
            "((((((((1 + 2) + 3) + 4) + sin(pi)) + (e ^ 2)) + (abc * 8)) - ((e ^ pi) / 2)) - abc)",
            parsed.to_string()
        );
        let compiled = Expression::compile(parsed).unwrap();
        assert_eq!(
            "(((17.38905609893065 + (abc * 8)) - 11.57034631638963) - abc)",
            compiled.to_string()
        );
    }
}
