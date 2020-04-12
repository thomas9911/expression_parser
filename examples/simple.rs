use expression_parser::{Expr, Variables};
// use std::collections::HashMap;

fn main() {
    let parsed = Expr::parse("1 + 5 - 2").unwrap();

    println!("{:?}", parsed); // 'ast'
    println!("{:?}", Expr::eval(parsed, &Variables::default())); // evaluated expression

    let parsed = Expr::parse("1 + test").unwrap();

    let variables = Variables::from_iter(vec![(String::from("test"), 1.2)]);

    println!("{:?}", parsed); // 'ast'
    println!("{:?}", Expr::eval(parsed, &variables)); // evaluated expression
}
