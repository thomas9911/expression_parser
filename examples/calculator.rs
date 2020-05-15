use expression_parser::{Expression, Variables};

const HELP_TEXT: &'static str = "Calculator example
Usage: calculator [OPTIONS] [ARGS ..]

OPTIONS:
    --help -h       prints help
    
ARGS:
    help            prints help
    ARGS            parse args and evaluate expression

ENV:
    DEBUG=TRUE      prints ast of the expression
";

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();

    match print_help(args.get(0).unwrap_or(&String::from("help"))) {
        false => (),
        true => return,
    };

    let parsed = match Expression::parse(args.join(" ").as_ref()) {
        Ok(x) => x,
        Err(e) => return println!("Invalid expression{}", e),
    };
    if parse_debug_env_var() {
        println!("{:?}", parsed); // 'ast'
    }
    match Expression::eval(parsed, &Variables::default()) {
        Ok(x) => println!("{}", x), // evaluated expression
        _ => println!("Expression contains invalid variables"),
    }
}

fn parse_debug_env_var() -> bool {
    std::env::var("DEBUG")
        .unwrap_or("false".to_string())
        .to_lowercase()
        .parse::<bool>()
        .unwrap_or(false)
}

fn print_help(first_arg: &str) -> bool {
    if ["--help", "-h", "help"].contains(&first_arg) {
        println!("{}", HELP_TEXT);
        true
    } else {
        false
    }
}
