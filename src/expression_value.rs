use crate::user_function::UserFunction;
use crate::{Closure, Expression, Variables};
use std::collections::{BinaryHeap, HashMap};
use std::iter::FromIterator;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serde", serde(untagged))]
pub enum ExpressionValue {
    Function(UserFunction, Variables),
    String(String),
    Bool(bool),
    Number(f64),
    List(Vec<Expression>),
    Map(ExpressionMap),
    #[cfg_attr(feature = "serde", serde(skip))]
    ExternalFunction(Closure),
    Null,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct ExpressionMap(pub HashMap<String, Expression>);

impl std::fmt::Display for ExpressionValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ExpressionValue::{Bool, ExternalFunction, Function, List, Null, Number};

        match self {
            ExpressionValue::String(x) => write!(f, "\"{}\"", x),
            Bool(x) => write!(f, "{}", x),
            Number(x) => write!(f, "{}", x),
            List(list) => write!(f, "[ {} ]", list_to_string(list).join(", ")),
            ExpressionValue::Map(map) => write!(f, "{}", map),
            Function(func, _) => write!(f, "{}", func),
            ExternalFunction(func) => write!(f, "{}", func),
            Null => write!(f, "null"),
        }
    }
}

impl std::fmt::Display for ExpressionMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let printed_map =
            BinaryHeap::from_iter(self.0.iter().map(|(k, v)| format!("{:?}: {}", k, v)))
                .into_sorted_vec()
                .join(", ");
        write!(f, "{{{}}}", printed_map)
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

impl<T> From<Vec<T>> for ExpressionValue
where
    T: Into<ExpressionValue>,
{
    fn from(input: Vec<T>) -> ExpressionValue {
        let expressions = input
            .into_iter()
            .map(|x| Expression::Value(x.into()))
            .collect();
        ExpressionValue::List(expressions)
    }
}

impl<T> From<HashMap<String, T>> for ExpressionValue
where
    T: Into<ExpressionValue>,
{
    fn from(input: HashMap<String, T>) -> ExpressionValue {
        ExpressionValue::Map(ExpressionMap::from(input))
    }
}

impl From<ExpressionMap> for ExpressionValue {
    fn from(input: ExpressionMap) -> ExpressionValue {
        ExpressionValue::Map(input)
    }
}

impl From<Closure> for ExpressionValue {
    fn from(input: Closure) -> ExpressionValue {
        ExpressionValue::ExternalFunction(input)
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

impl Default for ExpressionValue {
    fn default() -> Self {
        ExpressionValue::Null
    }
}

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

    /// casts value as a map
    pub fn as_map(self) -> Option<ExpressionMap> {
        use ExpressionValue::*;

        match self {
            Map(x) => Some(x),
            _ => None,
        }
    }

    /// casts value as a function
    pub fn as_function(self) -> Option<(UserFunction, Variables)> {
        use ExpressionValue::*;

        match self {
            Function(x, y) => Some((x, y)),
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

    pub fn is_map(&self) -> bool {
        use ExpressionValue::*;

        match self {
            Map(_) => true,
            _ => false,
        }
    }

    pub fn is_function(self) -> bool {
        use ExpressionValue::*;

        match self {
            Function(_, _) => true,
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
            Map(map) => map.0.is_empty(),
            Function(_, _) => false,
            ExternalFunction(_) => false,
            Null => true,
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

    pub fn what_type(&self) -> &'static str {
        use ExpressionValue::*;

        match self {
            String(_) => "string",
            Bool(_) => "boolean",
            Number(_) => "number",
            List(_) => "list",
            Map(_) => "map",
            Function(_, _) => "function",
            ExternalFunction(_) => "function",
            Null => "null",
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

impl ExpressionMap {
    pub fn new() -> ExpressionMap {
        ExpressionMap(HashMap::new())
    }

    pub fn get(&self, key: &str) -> Option<Expression> {
        self.0.get(key).cloned()
    }

    pub fn remove(&mut self, key: &str) -> Option<Expression> {
        self.0.remove(key)
    }

    pub fn insert<T: Into<ExpressionValue>>(&mut self, k: &str, v: T) -> Option<Expression> {
        self.0.insert(String::from(k), Expression::Value(v.into()))
    }
}

impl<T> From<HashMap<String, T>> for ExpressionMap
where
    T: Into<ExpressionValue>,
{
    fn from(input: HashMap<String, T>) -> ExpressionMap {
        let map = HashMap::from_iter(
            input
                .into_iter()
                .map(|(k, v)| (k, Expression::Value(v.into()))),
        );
        ExpressionMap { 0: map }
    }
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

#[test]
fn format_map() {
    let mut data = HashMap::new();
    data.insert(String::from("test"), 1);
    data.insert(String::from("testing"), 2);

    let mut map = ExpressionMap::from(data.clone());
    map.insert("list", vec![1, 2, 3]);
    map.insert("map", data);

    assert_eq!(
        "{\"list\": [ 1, 2, 3 ], \"map\": {\"test\": 1, \"testing\": 2}, \"test\": 1, \"testing\": 2}",
        map.to_string()
    );
}
