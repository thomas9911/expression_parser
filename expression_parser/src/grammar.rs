use pest::error::{Error as PestError, ErrorVariant};
use pest::iterators::{Pair, Pairs};
use pest::Span;

#[derive(Parser)]
#[grammar = "string_expression.pest"]
pub struct ExpressionessionParser;

pub fn create_string(
    pair: Pair<'_, Rule>,
) -> Result<std::string::String, snailquote::UnescapeError> {
    snailquote::unescape(&make_string(pair.into_inner()))
}

fn make_string(string: Pairs<'_, Rule>) -> String {
    let mut buffer = String::from("\"");
    for pair in string {
        match pair.as_rule() {
            Rule::unicode_code => buffer.push_str(&format!(r"\u{{{}}}", pair.as_str())),
            _ => buffer.push_str(pair.as_str()),
        }
    }
    buffer.push('"');
    buffer
}

pub fn make_pest_error(span: Span<'_>, message: String) -> PestError<Rule> {
    let variant = ErrorVariant::<Rule>::CustomError { message: message };
    PestError::new_from_span(variant, span)
}
