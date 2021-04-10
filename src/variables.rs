use crate::statics::DEFAULT_VARIABLES;
use crate::ExpressionValue;
use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// Trait for defining where variables are stored
pub trait VariableMap: std::fmt::Debug {
    fn get(&self, key: &str) -> Option<&ExpressionValue>;
    fn get_arc(&self, key: &str) -> Option<Arc<ExpressionValue>> {
        match self.get(key) {
            Some(x) => Some(Arc::from(x.clone())),
            None => None,
        }
    }
    fn insert(&mut self, key: &str, value: ExpressionValue) -> Option<ExpressionValue>;
    fn insert_arc(
        &mut self,
        key: &str,
        value: Arc<ExpressionValue>,
    ) -> Option<Arc<ExpressionValue>> {
        match self.insert(key, (*value).clone()) {
            Some(x) => Some(Arc::new(x)),
            None => None,
        }
    }
    fn remove(&mut self, _key: &str) -> Option<ExpressionValue> {
        None
    }
    fn local(&self) -> Option<Variables> {
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

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
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
    pub fn new() -> Variables {
        Variables {
            state: HashMap::new(),
        }
    }
    pub fn from_iter<T: IntoIterator<Item = (String, ExpressionValue)>>(iter: T) -> Variables {
        let mut variables = DEFAULT_VARIABLES.to_owned();
        variables.extend(iter);

        Variables { state: variables }
    }

    pub fn iter(&self) -> std::collections::hash_map::Iter<'_, String, ExpressionValue> {
        self.state.iter()
    }
}

impl IntoIterator for Variables {
    type Item = (String, ExpressionValue);
    type IntoIter = std::collections::hash_map::IntoIter<String, ExpressionValue>;

    fn into_iter(self) -> Self::IntoIter {
        self.state.into_iter()
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
    global: &'a Box<dyn VariableMap + 'a>,
}

impl<'a> VariableMap for ScopedVariables<'a> {
    fn get(&self, key: &str) -> Option<&ExpressionValue> {
        match self.local.get(key) {
            None => self.global.get(key),
            x => x,
        }
    }

    fn local(&self) -> Option<Variables> {
        Some(Variables::from_iter(self.local.clone()))
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
    pub fn new(variables: &'a Box<dyn VariableMap + 'a>) -> Self {
        Self {
            global: variables,
            local: HashMap::new(),
        }
    }
}
