use crate::Expression;

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
