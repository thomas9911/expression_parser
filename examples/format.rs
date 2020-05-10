use expression_parser::{StringExpr, StringVariables};

fn main() {
    let original = "1 + 5 - 2 + (12 - 2) + 123";
    let parsed = StringExpr::parse(original).unwrap();
    let parsed2 = StringExpr::parse(&parsed.to_string()).unwrap();
    println!("{}", original);
    println!("{}", parsed2);
    println!("are they equivalent? {:?}", parsed == parsed2);
    println!("just rust: {}", 1 + 5 - 2 + (12 - 2) + 123);
    println!(
        "parsed: {}",
        StringExpr::eval(parsed, &StringVariables::default()).unwrap()
    );
    println!(
        "parsed twice: {}",
        StringExpr::eval(parsed2, &StringVariables::default()).unwrap()
    );

    println!("=================================================");
    let original = "concat(1,2,3,4,5)";
    let parsed = StringExpr::parse(original).unwrap();
    println!("{}", parsed);
}
