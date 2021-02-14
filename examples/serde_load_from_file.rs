use expression_parser::{ExpressionFile, Variables};
use serde_json::{from_slice, to_writer_pretty};

use std::fs::{read, read_to_string, File};

const SCRIPT_DATA: &'static str = "examples/serde_load_from_file/script.txt";
const JSON_DATA: &'static str = "examples/serde_load_from_file/script.json";

fn main() {
    let script = read_to_string(SCRIPT_DATA).unwrap();
    let parsed = match ExpressionFile::parse(&script) {
        Ok(x) => x,
        Err(e) => return println!("{}", e),
    };

    // apply compilation if it is implemented
    // // let compiled = ExpressionFile::compile(parsed);
    let compiled = parsed;

    let f = File::create(JSON_DATA).unwrap();
    to_writer_pretty(f, &compiled).unwrap();

    let direct_output = ExpressionFile::eval(compiled, &mut Variables::default()).unwrap();

    println!("output: {}", direct_output);

    // from_reader is a slower function for serde_json, so this is good enough.
    let data = read(JSON_DATA).unwrap();
    let loaded: ExpressionFile = from_slice(&data).unwrap();

    let loaded_output = ExpressionFile::eval(loaded, &mut Variables::default()).unwrap();

    println!("output: {}", loaded_output);
    println!("same output? {}", loaded_output == direct_output);
}
