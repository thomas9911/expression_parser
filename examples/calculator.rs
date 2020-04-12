use expression_parser::{Expr, Variables};

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();

    let parsed = match Expr::parse(args.join(" ").as_ref()) {
        Ok(x) => x,
        Err(e) => return println!("Invalid expression{}", e),
    };

    println!("{:?}", parsed); // 'ast'
    match Expr::eval(parsed, &Variables::default()) {
        Some(x) => println!("{}", x), // evaluated expression
        _ => println!("Expression contains invalid variables"),
    }
}
