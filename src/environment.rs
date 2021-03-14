use std::sync::Arc;

use crate::statics::DEFAULT_VARIABLES;
use crate::{ScopedVariables, ScopedVariablesArc, VariableMap};

#[derive(Debug)]
pub struct Environment<'a> {
    pub variables: Box<dyn VariableMap + 'a>,
    pub global: Option<Box<&'a Environment<'a>>>,
}

#[derive(Debug)]
pub struct EnvironmentBuilder<'a> {
    variables: Option<Box<dyn VariableMap + 'a>>,
    global: Option<Box<&'a Environment<'a>>>,
}

impl<'a> Environment<'a> {
    pub fn builder() -> EnvironmentBuilder<'a> {
        EnvironmentBuilder::default()
    }

    pub fn vars(&'a self) -> Box<dyn VariableMap + 'a> {
        match self.global {
            Some(ref x) => x.vars(),
            None => self.variables,
        }
    }

    // pub fn new_scoped(&self) -> Environment<'a> {

    //     let context = ScopedVariablesArc::new(Arc::new(self.variables));

    //     Environment::builder()
    //         .with_variables(Box::new(context))
    //         .build()
    // }

    // pub fn new_scoped(&self) -> Environment<'a> {

    // }
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

        Environment {
            variables,
            global: self.global,
        }
    }

    pub fn with_variables(
        mut self,
        variables: Box<dyn VariableMap + 'a>,
    ) -> EnvironmentBuilder<'a> {
        self.variables = Some(variables);
        self
    }

    pub fn with_global(mut self, global: &'a Environment<'a>) -> EnvironmentBuilder<'a> {
        self.global = Some(Box::new(global));
        self
    }
}

impl<'a> Default for EnvironmentBuilder<'a> {
    fn default() -> Self {
        EnvironmentBuilder {
            variables: None,
            global: None,
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
        env.variables.get("true")
    );
    assert_eq!(
        Some(&ExpressionValue::from(true)),
        expected.variables.get("true")
    );
}

#[test]
fn with_variables_test() {
    use std::collections::HashMap;
    let env = HashMap::new();

    let builder = Environment::builder().with_variables(Box::new(env));

    assert!(builder.variables.is_some());
}

#[test]
fn build_test() {
    use crate::ExpressionValue;
    use std::collections::HashMap;

    let mut env = HashMap::new();
    env.insert(String::from("test"), 1.into());

    let env = Environment::builder().with_variables(Box::new(env)).build();

    assert_eq!(Some(&ExpressionValue::from(1)), env.variables.get("test"));
}
