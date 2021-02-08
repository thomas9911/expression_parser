use crate::statics::DEFAULT_VARIABLES;
use crate::ExpressionValue;
use std::collections::{BTreeMap, HashMap};

/// Trait for defining where variables are stored
pub trait VariableMap: std::fmt::Debug {
    fn get(&self, key: &str) -> Option<&ExpressionValue>;
    fn insert(&mut self, key: &str, value: ExpressionValue) -> Option<ExpressionValue>;
    fn remove(&mut self, _key: &str) -> Option<ExpressionValue> {
        None
    }
    fn clear(&mut self) {}
}

impl<'a, T> VariableMap for &'a mut T
where
    T: VariableMap,
{
    fn get(&self, key: &str) -> Option<&ExpressionValue> {
        (**self).get(key)
    }

    fn insert(&mut self, key: &str, value: ExpressionValue) -> Option<ExpressionValue> {
        (*self).insert(key, value)
    }

    fn remove(&mut self, key: &str) -> Option<ExpressionValue> {
        (*self).remove(key)
    }

    fn clear(&mut self) {
        (*self).clear()
    }
}

impl<'a, T> VariableMap for &'a T
where
    T: VariableMap,
{
    fn get(&self, key: &str) -> Option<&ExpressionValue> {
        (*self).get(key)
    }

    fn insert(&mut self, _key: &str, _value: ExpressionValue) -> Option<ExpressionValue> {
        panic!("cannot mutate when not defined as mutatable")
    }

    fn remove(&mut self, _key: &str) -> Option<ExpressionValue> {
        panic!("cannot mutate when not defined as mutatable")
    }

    fn clear(&mut self) {
        panic!("cannot mutate when not defined as mutatable")
    }
}

impl VariableMap for HashMap<String, ExpressionValue> {
    fn get(&self, key: &str) -> Option<&ExpressionValue> {
        self.get(key)
    }

    fn insert(&mut self, key: &str, value: ExpressionValue) -> Option<ExpressionValue> {
        self.insert(String::from(key), value)
    }

    fn remove(&mut self, key: &str) -> Option<ExpressionValue> {
        self.remove(key)
    }

    fn clear(&mut self) {
        self.clear()
    }
}

#[derive(Debug, Clone)]
pub struct Variables {
    state: HashMap<String, ExpressionValue>,
}

impl VariableMap for Variables {
    fn get(&self, key: &str) -> Option<&ExpressionValue> {
        self.state.get(key)
    }

    fn insert(&mut self, key: &str, value: ExpressionValue) -> Option<ExpressionValue> {
        self.state.insert(String::from(key), value)
    }

    fn remove(&mut self, key: &str) -> Option<ExpressionValue> {
        self.state.remove(key)
    }

    fn clear(&mut self) {
        self.state.clear()
    }
}

impl Variables {
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

/// Creates a variable map that has two stores.
/// One can only be changed on creation and the other one on runtime.
#[derive(Debug)]
pub struct ScopedVariables<'a> {
    local: HashMap<String, ExpressionValue>,
    // global: HashMap<String, ExpressionValue>,
    global: Box<dyn VariableMap + 'a>,
}

impl<'a> VariableMap for ScopedVariables<'a> {
    fn get(&self, key: &str) -> Option<&ExpressionValue> {
        match self.local.get(key) {
            None => self.global.get(key),
            x => x,
        }
    }

    fn insert(&mut self, key: &str, value: ExpressionValue) -> Option<ExpressionValue> {
        self.local.insert(String::from(key), value.into())
    }

    fn remove(&mut self, key: &str) -> Option<ExpressionValue> {
        self.local.remove(key)
    }

    fn clear(&mut self) {
        self.local.clear()
    }
}

impl<'a> ScopedVariables<'a> {
    pub fn new(variables: Box<dyn VariableMap + 'a>) -> Self {
        Self {
            global: variables,
            local: HashMap::new(),
        }
    }

    pub fn from_iter<T: IntoIterator<Item = (String, ExpressionValue)>>(iter: T) -> Self {
        let mut variables = DEFAULT_VARIABLES.to_owned();
        variables.extend(iter);

        Self {
            global: Box::new(variables),
            local: HashMap::new(),
        }
    }
}

impl<'a> std::default::Default for ScopedVariables<'a> {
    fn default() -> Self {
        Self {
            global: Box::new(DEFAULT_VARIABLES.to_owned()),
            local: HashMap::new(),
        }
    }
}

impl<'a> From<HashMap<String, ExpressionValue>> for ScopedVariables<'a> {
    fn from(state: HashMap<String, ExpressionValue>) -> Self {
        Self::from_iter(state.into_iter())
    }
}

impl<'a> From<BTreeMap<String, ExpressionValue>> for ScopedVariables<'a> {
    fn from(state: BTreeMap<String, ExpressionValue>) -> Self {
        Self::from_iter(state.into_iter())
    }
}

impl<'a> From<Variables> for ScopedVariables<'a> {
    fn from(state: Variables) -> Self {
        Self {
            global: Box::new(state.state),
            local: HashMap::new(),
        }
    }
}
