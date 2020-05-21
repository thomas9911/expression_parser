use expression_parser::{ExpressionFile, ExpressionValue, Variables};
use std::io::{self, Error, StdoutLock, Write};

const HELP_TEXT: &'static str = "Expression interactive example
Usage: repl [OPTIONS] [ARGS ..]

To exit the program type ctrl c, 'exit' or 'quit'

OPTIONS:
--help -h       prints help

ARGS:
help            prints help

ENV:
    DEBUG=TRUE      prints ast of the expression

Examples:
    > sin(2)/cos(2) == tan(2)
    true

    > 1 + 2 * 3
    7

    > contains(\"testing\", \"test\")
    true

    > text = \"testing testing\"
    > contains(text, \"test\")
    true
    > upper(concat(text, \" \", text))
    TESTING TESTING TESTING TESTING
";

fn main() -> Result<(), Error> {
    let args: Vec<String> = std::env::args().skip(1).collect();

    match print_help(args.get(0).unwrap_or(&String::from(""))) {
        false => (),
        true => return Ok(()),
    };

    let mut vars = Variables::default();

    let is_debug = parse_debug_env_var();

    let mut buffer = String::new();
    let stdout = io::stdout();
    let mut handle = stdout.lock();

    loop {
        print_cursor(&mut handle)?;
        match io::stdin().read_line(&mut buffer) {
            Ok(0) | Err(_) => break,
            Ok(_) => (),
        };
        buffer = buffer.trim_end().to_string();
        if ["quit", "exit"].contains(&buffer.as_ref()) {
            break;
        }
        if !print_help(&buffer) {
            do_expression(buffer, &mut vars, is_debug);
        }

        buffer = String::new();
    }

    Ok(println!("\nGood bye"))
}

fn print_cursor(handle: &mut StdoutLock) -> Result<(), Error> {
    handle.write_all(b">  ")?;
    handle.flush()
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

fn do_expression(buffer: String, vars: &mut Variables, is_debug: bool) {
    let parsed = match ExpressionFile::parse(buffer.as_ref()) {
        Ok(x) => x,
        Err(e) => return println!("Invalid expression{}", e),
    };
    if is_debug {
        println!("{:?}", parsed); // 'ast'
    }
    match ExpressionFile::eval(parsed, vars) {
        Ok(x) => {
            match x {
                ExpressionValue::Null => (),
                _ => println!("{}", x), // evaluated expression
            }
        }
        Err(e) => println!("{}", e),
    }
}
