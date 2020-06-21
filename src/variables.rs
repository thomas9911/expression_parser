use crate::statics::DEFAULT_VARIABLES;
use crate::ExpressionValue;
use std::collections::{BTreeMap, HashMap};

/// Trait for defining where variables are stored
pub trait VariableMap {
    fn get(&self, key: &str) -> Option<&ExpressionValue>;
    fn insert<V: Into<ExpressionValue>>(&mut self, key: &str, value: V) -> Option<ExpressionValue>;
    fn remove(&mut self, _key: &str) -> Option<ExpressionValue> {
        None
    }
    fn clear(&mut self) {}
}

#[derive(Debug, Clone)]
pub struct Variables {
    state: HashMap<String, ExpressionValue>,
}

impl VariableMap for Variables {
    fn get(&self, key: &str) -> Option<&ExpressionValue> {
        self.state.get(key)
    }

    fn insert<V>(&mut self, key: &str, value: V) -> Option<ExpressionValue>
    where
        V: Into<ExpressionValue>,
    {
        self.state.insert(String::from(key), value.into())
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

impl From<ScopedVariables> for Variables {
    fn from(state: ScopedVariables) -> Variables {
        Variables {
            state: state.global,
        }
    }
}

/// Creates a variable map that has two stores.
/// One can only be changed on creation and the other one on runtime.
pub struct ScopedVariables {
    local: HashMap<String, ExpressionValue>,
    global: HashMap<String, ExpressionValue>,
}

impl VariableMap for ScopedVariables {
    fn get(&self, key: &str) -> Option<&ExpressionValue> {
        match self.local.get(key) {
            None => self.global.get(key),
            x => x,
        }
    }

    fn insert<V>(&mut self, key: &str, value: V) -> Option<ExpressionValue>
    where
        V: Into<ExpressionValue>,
    {
        self.local.insert(String::from(key), value.into())
    }

    fn remove(&mut self, key: &str) -> Option<ExpressionValue> {
        self.local.remove(key)
    }

    fn clear(&mut self) {
        self.local.clear()
    }
}

impl ScopedVariables {
    pub fn from_iter<T: IntoIterator<Item = (String, ExpressionValue)>>(iter: T) -> Self {
        let mut variables = DEFAULT_VARIABLES.to_owned();
        variables.extend(iter);

        Self {
            global: variables,
            local: HashMap::new(),
        }
    }
}

impl std::default::Default for ScopedVariables {
    fn default() -> Self {
        Self {
            global: DEFAULT_VARIABLES.to_owned(),
            local: HashMap::new(),
        }
    }
}

impl From<HashMap<String, ExpressionValue>> for ScopedVariables {
    fn from(state: HashMap<String, ExpressionValue>) -> Self {
        Self::from_iter(state.into_iter())
    }
}

impl From<BTreeMap<String, ExpressionValue>> for ScopedVariables {
    fn from(state: BTreeMap<String, ExpressionValue>) -> Self {
        Self::from_iter(state.into_iter())
    }
}

impl From<Variables> for ScopedVariables {
    fn from(state: Variables) -> Self {
        Self {
            global: state.state,
            local: HashMap::new(),
        }
    }
}
