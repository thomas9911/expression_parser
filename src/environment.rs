use crate::statics::DEFAULT_VARIABLES;
use crate::VariableMap;

pub trait Env<'a>: std::fmt::Debug {
    fn variables(&self) -> &Box<dyn VariableMap + 'a>;
    fn variables_mut(&mut self) -> &mut Box<dyn VariableMap + 'a>;
    fn logger(&mut self) -> Arc<RwLock<dyn std::io::Write + 'a>>;
}

impl<'a, T> Env<'a> for &mut T
where
    T: Env<'a>,
{
    fn variables(&self) -> &Box<dyn VariableMap + 'a> {
        (**self).variables()
    }

    fn variables_mut(&mut self) -> &mut Box<dyn VariableMap + 'a> {
        (*self).variables_mut()
    }

    fn logger(&mut self) -> Arc<RwLock<dyn std::io::Write + 'a>> {
        (*self).logger()
    }
}

impl<'a, T> Env<'a> for Box<T>
where
    T: Env<'a>,
{
    fn variables(&self) -> &Box<dyn VariableMap + 'a> {
        (**self).variables()
    }

    fn variables_mut(&mut self) -> &mut Box<dyn VariableMap + 'a> {
        (**self).variables_mut()
    }

    fn logger(&mut self) -> Arc<RwLock<dyn std::io::Write + 'a>> {
        (**self).logger()
    }
}

impl<'a, T> Env<'a> for &T
where
    T: Env<'a>,
{
    fn variables(&self) -> &Box<dyn VariableMap + 'a> {
        (*self).variables()
    }

    fn variables_mut(&mut self) -> &mut Box<dyn VariableMap + 'a> {
        panic!("cannot mutate when not defined as mutatable")
    }

    fn logger(&mut self) -> Arc<RwLock<dyn std::io::Write + 'a>> {
        panic!("cannot mutate when not defined as mutatable")
    }
}

use std::sync::{Arc, RwLock};

/// Environment holding all the connections to side effects.
pub struct Environment<'a> {
    pub(crate) variables: Box<dyn VariableMap + 'a>,
    pub(crate) logger: Arc<RwLock<dyn std::io::Write + 'a>>,
}

impl std::fmt::Debug for Environment<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Environment")
            .field("variables", &self.variables)
            .field("logger", &"<logger>")
            .finish()
    }
}

pub struct EnvironmentBuilder<'a> {
    variables: Option<Box<dyn VariableMap + 'a>>,
    logger: Option<Box<dyn std::io::Write + 'a>>,
}

impl<'a> Environment<'a> {
    pub fn builder() -> EnvironmentBuilder<'a> {
        EnvironmentBuilder::default()
    }
}

impl<'a> Env<'a> for Environment<'a> {
    fn variables(&self) -> &Box<dyn VariableMap + 'a> {
        &self.variables
    }
    fn variables_mut(&mut self) -> &mut Box<dyn VariableMap + 'a> {
        &mut self.variables
    }

    fn logger(&mut self) -> Arc<RwLock<dyn std::io::Write + 'a>> {
        self.logger.clone()
    }
}

impl<'a> Default for Environment<'a> {
    fn default() -> Self {
        EnvironmentBuilder::default().build()
    }
}

impl<'a> EnvironmentBuilder<'a> {
    pub fn build(self) -> Environment<'a> {
        let variables = self
            .variables
            .unwrap_or(Box::new(DEFAULT_VARIABLES.to_owned()));
        let logger = self.logger.unwrap_or(Box::new(std::io::stdout()));
        Environment {
            variables,
            logger: Arc::new(RwLock::new(logger)),
        }
    }

    pub fn with_variables(
        mut self,
        variables: Box<dyn VariableMap + 'a>,
    ) -> EnvironmentBuilder<'a> {
        self.variables = Some(variables);
        self
    }

    pub fn with_logger(mut self, logger: Box<dyn std::io::Write + 'a>) -> EnvironmentBuilder<'a> {
        self.logger = Some(logger);
        self
    }
}

impl<'a> Default for EnvironmentBuilder<'a> {
    fn default() -> Self {
        EnvironmentBuilder {
            variables: None,
            logger: None,
        }
    }
}

#[test]
fn builder_default_test() {
    let builder = Environment::builder();
    let expected = EnvironmentBuilder::default();

    // we can't compare boxed traits, so just check if both variables are none
    assert!(builder.variables.is_none());
    assert!(expected.variables.is_none());
}

#[test]
fn env_default_test() {
    use crate::ExpressionValue;

    let env = Environment::builder().build();
    let expected = Environment::default();

    assert_eq!(
        Some(&ExpressionValue::from(true)),
        env.variables().get("true")
    );
    assert_eq!(
        Some(&ExpressionValue::from(true)),
        expected.variables().get("true")
    );
}

#[test]
fn with_variables_test() {
    use std::collections::HashMap;
    let vars = HashMap::new();

    let builder = Environment::builder().with_variables(Box::new(vars));

    assert!(builder.variables.is_some());
}

#[test]
fn build_test() {
    use crate::ExpressionValue;
    use std::collections::HashMap;

    let mut vars = HashMap::new();
    vars.insert(String::from("test"), 1.into());

    let env = Environment::builder()
        .with_variables(Box::new(vars))
        .build();

    assert_eq!(Some(&ExpressionValue::from(1)), env.variables().get("test"));
}

#[test]
fn mutable_variables() {
    use std::collections::HashMap;

    let mut vars = HashMap::new();
    vars.insert(String::from("test"), 1.into());

    let mut env = Environment::builder()
        .with_variables(Box::new(vars))
        .build();

    env.variables_mut().insert("ok", 1.into());

    assert_eq!(Some(&1.into()), env.variables().get("ok"))
}

#[test]
fn boxed_test_variables() {
    fn fetch_true<'a, E: Env<'a>>(t: E) -> bool {
        t.variables().get("true").unwrap().as_bool().unwrap()
    };

    let env = Environment::default();
    assert!(fetch_true(env));

    let mut env = Environment::default();
    assert!(fetch_true(Box::new(&mut env)));
    assert!(fetch_true(Box::new(&env)));
    assert!(fetch_true(Box::new(env)));
}

#[test]
fn boxed_test_variables_mut() {
    fn set_ok<'a, E: Env<'a>>(mut t: E) {
        t.variables_mut().insert("ok", true.into());
    };

    fn fetch_ok<'a, E: Env<'a>>(mut t: E) -> bool {
        t.variables_mut().get("ok").unwrap().as_bool().unwrap()
    };

    let mut env = Environment::default();
    set_ok(Box::new(&mut env));
    assert!(fetch_ok(Box::new(&mut env)));

    let env = Environment::default();
    let mut boxed_env = Box::new(env);
    set_ok(&mut boxed_env);
    assert!(fetch_ok(&mut boxed_env));
}
