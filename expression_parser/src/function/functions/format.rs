// use pest::error::Error as PestError;
use pest::Parser;

use crate::Error;

#[derive(Parser)]
#[grammar = "function/functions/format.pest"]
pub struct FormatGrammar;

#[derive(Debug, PartialEq)]
pub enum Part<'a> {
    String(&'a str),
    Replace(usize),
}

struct FormatIter<'a> {
    pairs: pest::iterators::Pairs<'a, Rule>,
    index_counter: usize,
    manual: bool,
    automatic: bool,
}

impl<'a> FormatIter<'a> {
    pub fn new(template: &'a str) -> Result<FormatIter<'a>, Error> {
        Ok(FormatIter {
            pairs: FormatGrammar::parse(Rule::template, template)?,
            index_counter: 0,
            manual: false,
            automatic: false,
        })
    }

    fn line_replace(&mut self, line: pest::iterators::Pair<'_, Rule>) -> Result<usize, Error> {
        let index = line.as_str().parse::<usize>().ok();
        let replace = if (self.manual || self.automatic) == false {
            self.manual = index.is_some();
            self.automatic = index.is_none();
            index.unwrap_or(self.index_counter)
        } else if self.manual {
            match index {
                Some(x) => x,
                None => return Err(Error::new_static(
                    "cannot switch from automatic field numbering to manual field specification",
                )),
            }
        } else if self.automatic {
            match index {
                Some(_) => return Err(Error::new_static(
                    "cannot switch from automatic field numbering to manual field specification",
                )),
                None => {
                    self.index_counter += 1;
                    self.index_counter
                }
            }
        } else {
            unreachable!()
        };

        Ok(replace)
    }
}

impl<'a> Iterator for FormatIter<'a> {
    type Item = Result<Part<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(line) = self.pairs.next() {
            match line.as_rule() {
                Rule::just_string => {
                    let part = Part::String(line.as_str());
                    Some(Ok(part))
                }
                Rule::replace => match self.line_replace(line) {
                    Ok(replace) => Some(Ok(Part::Replace(replace))),
                    Err(e) => Some(Err(e)),
                },
                Rule::EOI => None,
                _ => unreachable!(),
            }
        } else {
            None
        }
    }
}

fn apply_format<T: std::fmt::Display>(iter: FormatIter<'_>, vars: Vec<T>) -> Result<String, Error> {
    let mut out = String::new();

    for res in iter {
        match res? {
            Part::String(x) => out.push_str(x),
            Part::Replace(index) => {
                let var = vars.get(index).ok_or(Error::new(format!(
                    "argument on position {} not found",
                    index
                )))?;
                out.push_str(var.to_string().as_str())
            }
        }
    }

    Ok(out)
}

pub fn format<T: std::fmt::Display>(template: &str, vars: Vec<T>) -> Result<String, Error> {
    let iter = FormatIter::new(template)?;
    apply_format(iter, vars)
}

#[test]
fn format_automatic() {
    let result = format("abcdefg {} testing {} {} test {}", vec![1, 2, 3, 4]).unwrap();
    assert_eq!("abcdefg 1 testing 2 3 test 4", result);
}

#[test]
fn format_manual() {
    let result = format("abcdefg {3} testing {2} {1} test {0}", vec![1, 2, 3, 4]).unwrap();
    assert_eq!("abcdefg 4 testing 3 2 test 1", result);
}

#[test]
fn format_using_args_multiple_times() {
    let result = format("abcdefg {0} testing {0} {0} test {0}", vec![1]).unwrap();
    assert_eq!("abcdefg 1 testing 1 1 test 1", result);
}

#[test]
fn format_mix_types() {
    let result = format("abcdefg {0} testing {} {0} test {}", vec![1]);

    assert!(result.is_err())
}

#[test]
fn format_using_non_existsing_argument() {
    let result = format("testing {10}", vec![1]);

    assert!(result.is_err())
}

#[test]
fn format_invalid_template() {
    let result = format("testing {{}", vec![1]);

    assert!(result.is_err())
}

#[test]
fn format_named_template() {
    let result = format("testing {keys}", vec![1]);

    assert!(result.is_err())
}
