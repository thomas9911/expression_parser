use expression_parser::{Expression, Variables};

fn main() {
    let parsed = Expression::parse("1 + 5 - 2").unwrap();

    println!("{:?}", parsed); // 'ast'
    println!("{:?}", Expression::eval(parsed, &Variables::default())); // evaluated expression

    let parsed = Expression::parse("1 + test").unwrap();

    let variables = Variables::from_iter(vec![(String::from("test"), 1.2.into())]);

    println!("{:?}", parsed); // 'ast'
    println!("{:?}", Expression::eval(parsed, &variables)); // evaluated expression
}
