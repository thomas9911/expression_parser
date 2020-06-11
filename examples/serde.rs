use expression_parser::ExpressionFile;
use serde_json::to_string_pretty;

fn main() {
    let parsed = ExpressionFile::parse("1 + (5 - 2 * e) / pi + tan(e*pi)").unwrap();

    println!("{}", to_string_pretty(&parsed).unwrap()); // 'ast'

    let parsed = ExpressionFile::parse(
        r#"
        upper(
            concat(
                "1234", 
                "_", 
                contains(
                    "test", 
                    "something"
                ) or trim("__testing", "_"),
                "_", 
                true
            )
        )
    "#,
    )
    .unwrap();

    println!("{}", to_string_pretty(&parsed).unwrap()); // 'ast'
}
