use crate::grammar::Rule;
use crate::{ExpressionValue, Variables};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use std::collections::HashMap;

lazy_static! {
    pub(crate) static ref DEFAULT_VARIABLES: HashMap<String, ExpressionValue> = {
        let mut m = HashMap::new();
        m.insert(String::from("true"), ExpressionValue::Bool(true));
        m.insert(String::from("false"), ExpressionValue::Bool(false));
        m.insert(String::from("null"), ExpressionValue::Null);
        m.insert(String::from("none"), ExpressionValue::Null);
        m.insert(String::from("nil"), ExpressionValue::Null);
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
    pub(crate) static ref DEFAULT_STRING_VARIABLES: Variables = Variables::default();
    pub(crate) static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Assoc::*;
        use Rule::*;

        PrecClimber::new(vec![
            Operator::new(concat_op, Left),
            Operator::new(equal, Right) | Operator::new(not_equal, Right),
            Operator::new(and, Left) | Operator::new(or, Left),
            Operator::new(add, Left) | Operator::new(subtract, Left),
            Operator::new(multiply, Left) | Operator::new(divide, Left),
            Operator::new(power, Right),
        ])
    };
}
