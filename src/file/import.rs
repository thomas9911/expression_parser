use pest::error::Error as PestError;
use pest::iterators::Pairs;
use std::sync::Arc;

use crate::grammar::{create_string, make_pest_error, Rule};
use crate::{Env, Error};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

pub type ImportParseResult = Result<Import, PestError<Rule>>;
pub type EvalResult = Result<(), Error>;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Import {
    imported: Vec<(String, String)>,
    from: String,
}

impl std::fmt::Display for Import {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let a: Vec<_> = self
            .imported
            .iter()
            .map(|(a, b)| format!("{}: {}", a, b))
            .collect();
        write!(f, "import {{ {} }} from \"{}\"", a.join(", "), self.from)
    }
}

impl Import {
    pub fn eval<'a, 'b, V: Env<'a>>(import: Self, env: &'b mut V) -> EvalResult {
        let data = vec![(import.from, import.imported)];
        env.import(&data)?;
        Ok(())
    }

    pub fn eval_rc<'a, 'b, V: Env<'a>>(import: Arc<Self>, env: &'b mut V) -> EvalResult {
        let xd = (*import).clone();
        let data = vec![(xd.from, xd.imported)];
        env.import(&data)?;
        Ok(())
    }
}

pub fn parse_import(mut import_statement: Pairs<'_, Rule>) -> ImportParseResult {
    let to_fetch = import_statement
        .next()
        .expect("grammar of import is incorrect");

    let pair = import_statement
        .next()
        .expect("grammar of import is incorrect");

    let from = match create_string(pair.clone()) {
        Ok(x) => Ok(x),
        Err(e) => Err(make_pest_error(pair.as_span(), e.to_string())),
    }?;

    let imported = parse_variable_map(to_fetch.into_inner());

    Ok(Import { imported, from })
}

fn parse_variable_map(variable_map: Pairs<'_, Rule>) -> Vec<(String, String)> {
    variable_map
        .map(|item| parse_assign(item.into_inner()))
        .collect()
}

fn parse_assign(mut assign: Pairs<'_, Rule>) -> (String, String) {
    let first = assign
        .next()
        .expect("grammar of variablemap is incorrect")
        .as_str();
    let second = match assign.next() {
        Some(x) => x.as_str(),
        None => first,
    }
    .to_string();

    (first.to_string(), second)
}

#[test]
fn var_importing() {
    let import = crate::ExpressionFile::parse(
        "import { test: test, variable: my_var, exported } from \"testing\"",
    )
    .unwrap()
    .lines
    .remove(0);

    let expected = crate::file::ExpressionLine::Import(Arc::new(Import {
        from: String::from("testing"),
        imported: vec![
            (String::from("test"), String::from("test")),
            (String::from("variable"), String::from("my_var")),
            (String::from("exported"), String::from("exported")),
        ],
    }));

    assert_eq!(import, Arc::new(expected));
}

#[test]
fn import_display() {
    let import = crate::ExpressionFile::parse(
        "import { test: test, variable: my_var, exported } from \"testing\"",
    )
    .unwrap()
    .to_string();

    let expected = String::from(
        "import { test: test, variable: my_var, exported: exported } from \"testing\"",
    );

    assert_eq!(import, expected);
}
