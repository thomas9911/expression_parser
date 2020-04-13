use expression_parser::{Expr, StringExpr};
use serde_json::to_string_pretty;

fn main() {
    let parsed = Expr::parse("1 + (5 - 2 * e) / pi + tan(e*pi)").unwrap();

    println!("{}", to_string_pretty(&parsed).unwrap()); // 'ast'

    let parsed = StringExpr::parse(
        "
        upper(
            concat(
                \"1234\", 
                \"_\", 
                contains(
                    \"test\", 
                    \"something\"
                ) or trim(\"__testing\", \"_\"),
                \"_\", 
                true
            )
        )
    ",
    )
    .unwrap();

    println!("{}", to_string_pretty(&parsed).unwrap()); // 'ast'
}
