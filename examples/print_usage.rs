use expression_parser::function::{functions, FunctionName};
use expression_parser::{Environment, Expression, Function};

use strum::IntoEnumIterator;

fn main() {
    function_doc_tests()
}

fn function_doc_tests() {
    let mut functions: Vec<_> = Function::iter()
        .map(|x| FunctionName::from(x).to_string())
        .collect();
    functions.sort_unstable();

    for function in functions {
        let mut env = Environment::default();
        let help_text = functions::help(Expression::Var(function.clone()).into(), &mut env);
        let mut splitted_name_parts: Vec<String> =
            function.split('_').map(|x| x.to_owned()).collect();
        splitted_name_parts
            .get_mut(0)
            .unwrap()
            .get_mut(0..1)
            .unwrap()
            .make_ascii_uppercase();
        let function_name = splitted_name_parts.join(" ");

        println!(
            "## {}\n\n{}\n",
            function_name,
            help_text.unwrap().to_string().trim_matches('"')
        );
    }
}
