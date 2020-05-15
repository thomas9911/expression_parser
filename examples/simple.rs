use expression_parser::{StringExpr, StringVariables};
// use std::collections::HashMap;

fn main() {
    let parsed = StringExpr::parse("1 + 5 - 2").unwrap();

    println!("{:?}", parsed); // 'ast'
    println!("{:?}", StringExpr::eval(parsed, &StringVariables::default())); // evaluated expression

    let parsed = StringExpr::parse("1 + test").unwrap();

    let variables = StringVariables::from_iter(vec![(String::from("test"), 1.2.into())]);

    println!("{:?}", parsed); // 'ast'
    println!("{:?}", StringExpr::eval(parsed, &variables)); // evaluated expression
}
