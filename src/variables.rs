use crate::statics::DEFAULT_VARIABLES;
use crate::ExpressionValue;
use std::collections::{BTreeMap, HashMap};

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
