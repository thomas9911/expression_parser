use crate::statics::{DEFAULT_STRING_VARIABLES, DEFAULT_VARIABLES};
use crate::string_expression::EvalResult;
use crate::{Error, Expression, Variables};

mod functions;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Function {
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

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Function::*;
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

impl Function {
    pub fn eval(operator: Function, vars: &Variables) -> EvalResult {
        use Function::*;

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

    pub fn compile(operator: Function) -> Result<Expression, Error> {
        use Function::*;

        if operator.contains_variables() | operator.cannot_be_pre_evaluated() {
            let funcs = match operator.to_owned() {
                Concat(list) => Concat(Function::compile_list(list)?),
                Sum(list) => Sum(Function::compile_list(list)?),
                Product(list) => Product(Function::compile_list(list)?),
                All(list) => All(Function::compile_list(list)?),
                Any(list) => Any(Function::compile_list(list)?),
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
            Ok(Expression::Value(Function::eval(
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
            Function::Random(_, _) => true,
            _ => false,
        }
    }

    fn contains_variables(&self) -> bool {
        self.iter_variables_without_defaults().count() != 0
    }

    pub fn iter_variables<'a>(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        use Function::*;

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

fn list_to_string(input: &Vec<Expression>) -> Vec<String> {
    input.iter().map(|x| format!("{}", x)).collect()
}
