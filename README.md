# expression_parser

### A sort of calculator in Rust using Pest.

Take a look at the calculator example:
```sh
cargo run --example calculator 1 + 12
```

#### library usage

Simple example
```rust
use expression_parser::{Expr, Variables};

let parsed = Expr::parse("1 + 5 - 2")?;
let result = Expr::eval(parsed, &Variables::default());

assert_eq!(Some(4.0), result);
```

Another example
```rust
use expression_parser::{Expr, Variables};

let parsed = Expr::parse("e ^ (1 + 5 - 2)")?;
let result = Expr::eval(parsed, &Variables::default());

assert_eq!(Some(std::f64::consts::E.powf(4.0)), result);
```

Use build-in variables and functions
```rust
use expression_parser::{Expr, Variables};

let parsed = Expr::parse("sin(e) + 1")?;
let result = Expr::eval(parsed, &Variables::default());

assert_eq!(Some(std::f64::consts::E.sin() + 1.0), result);
```

Use your own variables
```rust
use expression_parser::{Expr, Variables};

let parsed = Expr::parse("x + y + z")?;

let mut vars = std::collections::HashMap::new();
vars.insert(String::from("x"), 3.0);
vars.insert(String::from("y"), 3.0);
vars.insert(String::from("z"), 10.0);

let result = Expr::eval(parsed.clone(), &vars.into());

assert_eq!(Some(16.0), result);

let mut vars = Variables::default();
vars.insert("x", 3.0);
vars.insert("y", 3.0);
vars.insert("z", 10.0);

let result = Expr::eval(parsed, &vars);
assert_eq!(Some(16.0), result);
```

Simple String example
```rust
use expression_parser::{StringExpr, StringVariables};

let parsed = StringExpr::parse(r#"concat("1", "2", "3", "4")"#)?;
let result = StringExpr::eval(parsed, &StringVariables::default());

assert_eq!(Some(String::from("1234")), result);
```

