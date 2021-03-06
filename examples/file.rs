use expression_parser::{Environment, ExpressionFile, FileImporter};

fn print_help() {
    print!(
        r#"Runs FILE as a script file.
    
    Usage: program FILE
    "#
    );
}

fn main() {
    let mut args = std::env::args().skip(1);

    let filename = match args.next() {
        Some(x)
            if [
                String::from("help"),
                String::from("-h"),
                String::from("--help"),
            ]
            .contains(&x) =>
        {
            return print_help()
        }
        Some(x) => x,
        None => return print_help(),
    };

    let script = match std::fs::read_to_string(filename) {
        Ok(x) => x,
        Err(e) => return println!("{}", e),
    };

    let parsed = match ExpressionFile::parse(&script) {
        Ok(x) => x,
        Err(e) => return println!("{}", e),
    };

    let mut env = Environment::builder()
        .with_importer(Box::new(FileImporter))
        .build();

    match ExpressionFile::eval(parsed, &mut env) {
        Ok(x) => return println!("{}", x),
        Err(e) => return println!("{}", e),
    };
}
