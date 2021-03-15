use expression_parser::{Environment, Expression, Variables};

fn main() {
    let parsed = Expression::parse("1 + 5 - 2").unwrap();

    println!("{:?}", parsed); // 'ast'
    println!("{:?}", Expression::eval(parsed, &Environment::default())); // evaluated expression

    let parsed = Expression::parse("1 + test").unwrap();

    let variables = Variables::from_iter(vec![(String::from("test"), 1.2.into())]);
    let env = Environment::builder()
        .with_variables(Box::new(variables))
        .build();

    println!("{:?}", parsed); // 'ast'
    println!("{:?}", Expression::eval(parsed, &env)); // evaluated expression
}
