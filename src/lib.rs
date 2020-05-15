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
//! use expression_parser::{Expression, Variables};
//! # use expression_parser::grammar::Rule;
//!
//! # fn main() -> Result<(), Error<Rule>> {
//! let parsed = Expression::parse("1 + 5 - 2")?;
//! let result = Expression::eval(parsed, &Variables::default());
//!
//! assert_eq!(Ok(4.0.into()), result);
//! # Ok(())
//! # }
//! ```
//!
//! Another example
//! ```
//! # use pest::error::Error;
//! use expression_parser::{Expression, Variables};
//! # use expression_parser::grammar::Rule;
//!
//! # fn main() -> Result<(), Error<Rule>> {
//! let parsed = Expression::parse("e ^ (1 + 5 - 2)")?;
//! let result = Expression::eval(parsed, &Variables::default());
//!
//! assert_eq!(Ok(std::f64::consts::E.powf(4.0).into()), result);
//! # Ok(())
//! # }
//! ```
//!
//! Use build-in variables and functions
//! ```
//! # use pest::error::Error;
//! use expression_parser::{Expression, Variables};
//! # use expression_parser::grammar::Rule;
//!
//! # fn main() -> Result<(), Error<Rule>> {
//! let parsed = Expression::parse("sin(e) + 1")?;
//! let result = Expression::eval(parsed, &Variables::default());
//!
//! assert_eq!(Ok((std::f64::consts::E.sin() + 1.0).into()), result);
//! # Ok(())
//! # }
//! ```
//!
//! Use your own variables
//! ```
//! # use pest::error::Error;
//! use expression_parser::{Expression, Variables};
//! # use expression_parser::grammar::Rule;
//!
//! # fn main() -> Result<(), Error<Rule>> {
//! let parsed = Expression::parse("x + y + z")?;
//!
//! let mut vars = std::collections::HashMap::new();
//! vars.insert(String::from("x"), 3.0.into());
//! vars.insert(String::from("y"), 3.0.into());
//! vars.insert(String::from("z"), 10.0.into());
//!
//! let result = Expression::eval(parsed.clone(), &vars.into());
//!
//! assert_eq!(Ok(16.0.into()), result);
//!
//! let mut vars = Variables::default();
//! vars.insert("x", 3.0);
//! vars.insert("y", 3.0);
//! vars.insert("z", 10.0);
//!
//! let result = Expression::eval(parsed, &vars);
//! assert_eq!(Ok(16.0.into()), result);
//! # Ok(())
//! # }
//! ```
//!
//! Simple String example
//! ```
//! # use pest::error::Error;
//! use expression_parser::{Expression, Variables, ExpressionValue};
//! # use expression_parser::grammar::Rule;
//!
//! # fn main() -> Result<(), Error<Rule>> {
//! let parsed = Expression::parse(r#"concat("1", "2", "3", "4")"#)?;
//! let result = Expression::eval(parsed, &Variables::default());
//!
//! assert_eq!(Ok(ExpressionValue::from("1234")), result);
//! assert_eq!("\"1234\"", result.unwrap().to_string());
//! # Ok(())
//! # }
//! ```
//!
#![recursion_limit = "1024"]

#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;

pub mod error;
pub mod expression_value;
pub mod function;
pub mod grammar;
pub mod statics;
pub mod string_expression;
pub mod variables;

pub use error::Error;
pub use expression_value::ExpressionValue;
pub use function::Function;
pub use string_expression::Expression;
pub use variables::Variables;
