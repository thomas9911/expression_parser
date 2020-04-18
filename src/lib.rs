//! ## A sort of calculator in Rust using Pest.
//!
//! Take a look at the calculator example:
//! ```sh
//! cargo run --example calculator 1 + 12
//! ```
//!
//! ### library usage
//!
//! Simple example
//! ```
//! # use pest::error::Error;
//! use expression_parser::{Expr, Variables};
//! # use expression_parser::expression::Rule;
//!
//! # fn main() -> Result<(), Error<Rule>> {
//! let parsed = Expr::parse("1 + 5 - 2")?;
//! let result = Expr::eval(parsed, &Variables::default());
//!
//! assert_eq!(Some(4.0), result);
//! # Ok(())
//! # }
//! ```
//!
//! Another example
//! ```
//! # use pest::error::Error;
//! use expression_parser::{Expr, Variables};
//! # use expression_parser::expression::Rule;
//!
//! # fn main() -> Result<(), Error<Rule>> {
//! let parsed = Expr::parse("e ^ (1 + 5 - 2)")?;
//! let result = Expr::eval(parsed, &Variables::default());
//!
//! assert_eq!(Some(std::f64::consts::E.powf(4.0)), result);
//! # Ok(())
//! # }
//! ```
//!
//! Use build-in variables and functions
//! ```
//! # use pest::error::Error;
//! use expression_parser::{Expr, Variables};
//! # use expression_parser::expression::Rule;
//!
//! # fn main() -> Result<(), Error<Rule>> {
//! let parsed = Expr::parse("sin(e) + 1")?;
//! let result = Expr::eval(parsed, &Variables::default());
//!
//! assert_eq!(Some(std::f64::consts::E.sin() + 1.0), result);
//! # Ok(())
//! # }
//! ```
//!
//! Use your own variables
//! ```
//! # use pest::error::Error;
//! use expression_parser::{Expr, Variables};
//! # use expression_parser::expression::Rule;
//!
//! # fn main() -> Result<(), Error<Rule>> {
//! let parsed = Expr::parse("x + y + z")?;
//!
//! let mut vars = std::collections::HashMap::new();
//! vars.insert(String::from("x"), 3.0);
//! vars.insert(String::from("y"), 3.0);
//! vars.insert(String::from("z"), 10.0);
//!
//! let result = Expr::eval(parsed.clone(), &vars.into());
//!
//! assert_eq!(Some(16.0), result);
//!
//! let mut vars = Variables::default();
//! vars.insert("x", 3.0);
//! vars.insert("y", 3.0);
//! vars.insert("z", 10.0);
//!
//! let result = Expr::eval(parsed, &vars);
//! assert_eq!(Some(16.0), result);
//! # Ok(())
//! # }
//! ```
//!
//! Simple String example
//! ```
//! # use pest::error::Error;
//! use expression_parser::{StringExpr, StringVariables, ExpressionValue};
//! # use expression_parser::string_expression::Rule;
//!
//! # fn main() -> Result<(), Error<Rule>> {
//! let parsed = StringExpr::parse(r#"concat("1", "2", "3", "4")"#)?;
//! let result = StringExpr::eval(parsed, &StringVariables::default());
//!
//! assert_eq!(Some(ExpressionValue::from("1234")), result);
//! assert_eq!("1234", result.unwrap().to_string());
//! # Ok(())
//! # }
//! ```
//!
#![recursion_limit = "1024"]

#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;

pub mod expression;
pub mod string_expression;
pub use expression::{Expr, Ops, Variables};
pub use string_expression::{ExpressionValue, Functions, StringExpr, StringVariables};
