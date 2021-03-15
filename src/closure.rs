use crate::{Env, Error, ExpressionValue};
use std::sync::Arc;

type Fp = Arc<
    Box<
        dyn Fn(Vec<ExpressionValue>, &mut dyn Env<'_>) -> Result<ExpressionValue, Error>
            + Send
            + Sync,
    >,
>;

#[derive(Clone)]
pub struct Closure {
    pub input_variables: Vec<String>,
    pub(crate) function: Fp,
}

impl std::fmt::Debug for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Closure")
            .field("function", &"<function>")
            .finish()
    }
}

impl PartialEq for Closure {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.function, &other.function)
    }
}

impl std::fmt::Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{{} => {}}}",
            self.input_variables.join(", "),
            "<function>"
        )
    }
}

impl Closure {
    pub fn new(input_variables: Vec<String>, function: Fp) -> Closure {
        Closure {
            input_variables,
            function,
        }
    }
}
