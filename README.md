# expression_parser


TODO: think of a new name.

[Github Pages](https://thomas9911.github.io/expression_parser/)

### Why
- Library first
- JSON support (copy paste your json, should be valid code)
- Immutable
- Compiled code externaly saveable (using serde)
- No external calls build-in (you can add those yourself if you want)
  - Currently `print` is implemented using `println!`, this should change in the future.


Take a look at the calculator example:
```sh
cargo run --example calculator 1 + 12
```

Or the expression example:
```sh
cargo run --example calculator 1 + 12
```

Or even better use the REPL:
```sh
cargo run --example repl
```

#### library usage

Simple example
```rust
use expression_parser::{Environment, Expression};

let parsed = Expression::parse("1 + 5 - 2")?;
let result = Expression::eval(parsed, &mut Environment::default());

assert_eq!(Ok(4.0.into()), result);
```

Another example
```rust
use expression_parser::{Environment, Expression};

let parsed = Expression::parse("e ^ (1 + 5 - 2)")?;
let result = Expression::eval(parsed, &mut Environment::default());

assert_eq!(Ok(std::f64::consts::E.powf(4.0).into()), result);
```

Use build-in variables and functions
```rust
use expression_parser::{Environment, Expression};

let parsed = Expression::parse("sin(e) + 1")?;
let result = Expression::eval(parsed, &mut Environment::default());

assert_eq!(Ok((std::f64::consts::E.sin() + 1.0).into()), result);
```

Use your own variables
```rust
use expression_parser::{Env, Environment, Expression, Variables, VariableMap};

let parsed = Expression::parse("x + y + z")?;

let mut vars = std::collections::HashMap::new();
vars.insert(String::from("x"), 3.0.into());
vars.insert(String::from("y"), 3.0.into());
vars.insert(String::from("z"), 10.0.into());

let mut env = Environment::builder()
            .with_variables(Box::new(vars))
            .build();

let result = Expression::eval(parsed.clone(), &mut env);

assert_eq!(Ok(16.0.into()), result);

let mut vars = Variables::default();
vars.insert("x", 3.0.into());
vars.insert("y", 3.0.into());
vars.insert("z", 10.0.into());

let mut env = Environment::builder()
            .with_variables(Box::new(vars))
            .build();

let result = Expression::eval(parsed, &mut env);
assert_eq!(Ok(16.0.into()), result);
```

Simple String example
```rust
use expression_parser::{Environment, Expression, ExpressionValue};

let parsed = Expression::parse(r#"concat("1", "2", "3", "4")"#)?;
let result = Expression::eval(parsed, &mut Environment::default());

assert_eq!(Ok(ExpressionValue::from("1234")), result);
assert_eq!("\"1234\"", result.unwrap().to_string());
```

Multi-line example with variable assigment
```rust
use expression_parser::{Environment, ExpressionFile, ExpressionValue};

let input = r#"
    a = [1, 2, 3];
    b = [3, 2, 1];
    c = concat(a, b);
    d = concat(b, a);
    concat(c, [4,4], d);
"#;
let file = ExpressionFile::parse(input)?;
let evaluated = ExpressionFile::eval(file, &mut Environment::default());
assert_eq!(
    ExpressionValue::from(vec![
        1, 2, 3, 3, 2, 1, 4,
        4, 3, 2, 1, 1, 2, 3
    ]),
    evaluated.unwrap()
);
```

