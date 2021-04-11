//! # expression_parser
//!
//! TODO: think of a new name.
//!
//! [Github Pages](https://thomas9911.github.io/expression_parser/)
//!
//! ## Why
//!
//! - Library first
//! - JSON support (copy paste your json, should be valid code)
//! - Immutable
//! - Compiled code externaly saveable (using serde)
//! - No external calls build-in (you can add those yourself if you want)
//!
//! ## Non Goals
//!
//! - Speed, speed is nice but not a goal
//!
//! ## Examples
//!
//! Take a look at the expression example:
//!
//! ```sh
//! cargo run --example expression 1 + 12
//! ```
//!
//! Or even better use the REPL:
//!
//! ```sh
//! cargo run --example repl
//! ```
//!
//! For syntax check the [examples page](https://thomas9911.github.io/expression_parser/chapter_5.html) and the rest of the [Github Pages](https://thomas9911.github.io/expression_parser/)
//!
//! ## library usage
//!
//! Simple example:
//!
//! ```
//! # use expression_parser::Error;
//! use expression_parser::{Environment, ExpressionFile};
//!
//! # fn main() -> Result<(), Error> {
//! let result = ExpressionFile::run("1 + 5 - 2",  &mut Environment::default());
//!
//! assert_eq!(Ok(4.0.into()), result);
//! # Ok(())
//! # }
//! ```
//!
//! Example with variable assignment:
//!
//! ```
//! # use expression_parser::Error;
//! use expression_parser::{Environment, ExpressionFile, ExpressionValue};
//!
//! # fn main() -> Result<(), Error> {
//! let input = r#"
//!     a = [1, 2, 3];
//!     b = [3, 2, 1];
//!     c = concat(a, b);
//!     d = concat(b, a);
//!     concat(c, [4,4], d);
//! "#;
//! let file = ExpressionFile::parse(input)?;
//! let evaluated = ExpressionFile::eval(file, &mut Environment::default());
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
//! For better examples take a look at [the library usage page](https://thomas9911.github.io/expression_parser/chapter_6.html)
// #![recursion_limit = "1024"]

#![deny(rust_2018_idioms)]

#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate strum_macros;

pub mod arc_utils;
pub mod closure;
pub mod environment;
pub mod error;
pub mod expression_value;
pub mod file;
pub mod function;
pub mod grammar;
pub mod import;
pub mod statics;
pub mod string_expression;
pub mod user_function;
pub mod variables;

pub use closure::Closure;
pub use environment::{Env, Environment, EnvironmentBuilder};
pub use error::{Error, ErrorCodes};
pub use expression_value::{ExpressionMap, ExpressionValue};
pub use file::assignment::{Assignment, Unassignment};
pub use file::import::Import;
pub use file::ExpressionFile;
pub use function::Function;
pub use import::{
    CollectionImporter, FileImporter, ImportFetch, Importer, MapImporter, NullImporter,
    SingleCollectionImporter,
};
pub use string_expression::Expression;
pub use user_function::UserFunction;
pub use variables::{ScopedVariables, VariableMap, Variables};
