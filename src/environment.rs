use crate::statics::DEFAULT_VARIABLES;
use crate::VariableMap;

pub trait Env<'a>: std::fmt::Debug {
    fn variables(&self) -> &Box<dyn VariableMap + 'a>;
    fn variables_mut(&mut self) -> &mut Box<dyn VariableMap + 'a>;
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
}

#[derive(Debug)]
/// Environment holding all the connections to side effects. 
pub struct Environment<'a> {
    variables: Box<dyn VariableMap + 'a>,
}

#[derive(Debug)]
pub struct ScopedEnvironment<'a, 'b> {
    pub(crate) original: Box<&'b dyn Env<'a>>,
    pub(crate) local: Box<dyn VariableMap + 'b>,
}

#[derive(Debug)]
pub struct EnvironmentBuilder<'a> {
    variables: Option<Box<dyn VariableMap + 'a>>,
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
}

impl<'a> Default for Environment<'a> {
    fn default() -> Self {
        EnvironmentBuilder::default().build()
    }
}

impl<'a, 'b> Env<'b> for ScopedEnvironment<'a, 'b> {
    fn variables(&self) -> &Box<dyn VariableMap + 'b> {
        &self.local
    }
    fn variables_mut(&mut self) -> &mut Box<dyn VariableMap + 'b> {
        &mut self.local
    }
}

impl<'a> EnvironmentBuilder<'a> {
    pub fn build(self) -> Environment<'a> {
        let variables = self
            .variables
            .unwrap_or(Box::new(DEFAULT_VARIABLES.to_owned()));

        Environment { variables }
    }

    pub fn with_variables(
        mut self,
        variables: Box<dyn VariableMap + 'a>,
    ) -> EnvironmentBuilder<'a> {
        self.variables = Some(variables);
        self
    }
}

impl<'a> Default for EnvironmentBuilder<'a> {
    fn default() -> Self {
        EnvironmentBuilder { variables: None }
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
