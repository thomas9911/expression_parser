// use expression_parser::{ExpressionFile, ExpressionValue, Function};
use expression_parser::function::{functions, FunctionName};
use expression_parser::{Expression, Function, Variables};

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
        let vars = Variables::default();
        let a = functions::help(Expression::Var(function.clone()), &vars);
        println!(
            "## {}\n\n{}\n",
            function,
            a.unwrap().to_string().trim_matches('"')
        );
    }
}
