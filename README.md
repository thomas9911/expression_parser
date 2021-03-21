# expression_parser


TODO: think of a new name.

[Github Pages](https://thomas9911.github.io/expression_parser/)

### Why
- Library first
- JSON support (copy paste your json, should be valid code)
- Immutable
- Compiled code externaly saveable (using serde)
- No external calls build-in (you can add those yourself if you want)

### Non Goals
- Speed, speed is nice but not a goal

### Examples

Take a look at the expression example:

```sh
cargo run --example expression 1 + 12
```

Or even better use the REPL:

```sh
cargo run --example repl
```

For syntax check the [examples page](https://thomas9911.github.io/expression_parser/chapter_5.html) and the rest of the [Github Pages](https://thomas9911.github.io/expression_parser/)

### library usage

Simple example:

```rust
use expression_parser::{Environment, ExpressionFile};

let result = ExpressionFile::run("1 + 5 - 2",  &mut Environment::default());

assert_eq!(Ok(4.0.into()), result);
```

Example with variable assignment:

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

For better examples take a look at [the library usage page](https://thomas9911.github.io/expression_parser/chapter_6.html)

