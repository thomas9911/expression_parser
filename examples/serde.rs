use expression_parser::Expr;
use serde_json::to_string_pretty;

fn main() {
    let parsed = Expr::parse("1 + (5 - 2 * e) / pi + tan(e*pi)").unwrap();

    println!("{}", to_string_pretty(&parsed).unwrap()); // 'ast'
}
