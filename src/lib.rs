//! ## A sort of calculator in Rust using Pest.
//!
//! Take a look at the calculator example:
//! ```sh
//! cargo run --example calculator 1 + 12
//! ```
//!
//! Or the expression example:
//! ```sh
//! cargo run --example calculator 1 + 12
//! ```
//!
//! Or even better use the REPL:
//! ```sh
//! cargo run --example repl
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
//! use expression_parser::{Expression, Variables, VariableMap};
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
//! let result = Expression::eval(parsed.clone(), &Variables::from(vars));
//!
//! assert_eq!(Ok(16.0.into()), result);
//!
//! let mut vars = Variables::default();
//! vars.insert("x", 3.0);
//! vars.insert("y", 3.0);
//! vars.insert("z", 10.0);
//!
//! let result = Expression::eval(parsed, &Variables::from(vars));
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
//! Multi-line example with variable assigment
//! ```
//! # use pest::error::Error;
//! use expression_parser::{ExpressionFile, Variables, ExpressionValue};
//! # use expression_parser::grammar::Rule;
//!
//! # fn main() -> Result<(), Error<Rule>> {
//! let input = r#"
//!     a = [1, 2, 3];
//!     b = [3, 2, 1];
//!     c = concat(a, b);
//!     d = concat(b, a);
//!     concat(c, [4,4], d);
//! "#;
//! let file = ExpressionFile::parse(input)?;
//! let evaluated = ExpressionFile::eval(file, &mut Variables::default());
//! assert_eq!(
//!     ExpressionValue::from(vec![
//!         1, 2, 3, 3, 2, 1, 4,
//!         4, 3, 2, 1, 1, 2, 3
//!     ]),
//!     evaluated.unwrap()
//! );
//! # Ok(())
//! # }
//! ```
//!
#![recursion_limit = "1024"]

#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;
extern crate strum;
#[macro_use]
extern crate strum_macros;

pub mod assignment;
pub mod error;
pub mod expression_value;
pub mod file;
pub mod function;
pub mod grammar;
pub mod statics;
pub mod string_expression;
pub mod user_function;
pub mod variables;

pub use assignment::{Assignment, Unassignment};
pub use error::Error;
pub use expression_value::{ExpressionMap, ExpressionValue};
pub use file::ExpressionFile;
pub use function::Function;
pub use string_expression::Expression;
pub use user_function::UserFunction;
pub use variables::{VariableMap, Variables};
