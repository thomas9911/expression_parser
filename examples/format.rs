use expression_parser::{Expression, Variables};

fn main() {
    let original = "1 + 5 - 2 + (12 - 2) + 123";
    let parsed = Expression::parse(original).unwrap();
    let parsed2 = Expression::parse(&parsed.to_string()).unwrap();
    println!("{}", original);
    println!("{}", parsed2);
    println!("are they equivalent? {:?}", parsed == parsed2);
    println!("just rust: {}", 1 + 5 - 2 + (12 - 2) + 123);
    println!(
        "parsed: {}",
        Expression::eval(parsed, &Variables::default()).unwrap()
    );
    println!(
        "parsed twice: {}",
        Expression::eval(parsed2, &Variables::default()).unwrap()
    );

    println!("=================================================");
    let original = "concat(1,2,3,4,5)";
    let parsed = Expression::parse(original).unwrap();
    println!("{}", parsed);
}
