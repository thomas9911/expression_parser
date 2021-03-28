use std::collections::HashMap;
use std::fs::read_to_string;

use crate::{Error, ErrorCodes, Variables};

pub trait Importer {
    fn fetch<'a>(&'a self, from: &str) -> Result<ImportFetch<'a>, Error>;
}

#[derive(Debug, PartialEq)]
pub enum ImportFetch<'a> {
    /// result is already evaluated, and a reference to a `Variables`
    Evaluated(&'a Variables),
    /// result is already evaluated, and a owned `Variables`
    EvaluatedOwned(Variables),
    /// result still needs to be evaluated
    Text(String),
}

#[derive(Debug, PartialEq)]
/// Importer that imports from files
pub struct FileImporter;

impl Importer for FileImporter {
    fn fetch<'a>(&'a self, from: &str) -> Result<ImportFetch<'a>, Error> {
        let file_text = read_to_string(from)?;
        Ok(ImportFetch::Text(file_text))
    }
}

#[derive(Debug, PartialEq)]
/// Importer that always returns error on import
pub struct NullImporter;

impl Importer for NullImporter {
    fn fetch<'a>(&'a self, _from: &str) -> Result<ImportFetch<'a>, Error> {
        Err(Error::new_code(ErrorCodes::IMPORT_ERROR))
    }
}

#[derive(Debug, PartialEq)]
/// Importer that always returns the same map
pub struct MapImporter {
    data: Variables,
}

impl Importer for MapImporter {
    fn fetch<'a>(&'a self, _from: &str) -> Result<ImportFetch<'a>, Error> {
        Ok(ImportFetch::Evaluated(&self.data))
    }
}

impl MapImporter {
    pub fn new(variables: Variables) -> Self {
        MapImporter { data: variables }
    }
}

#[derive(Debug, PartialEq)]
/// Importer that uses the given HashMap to resolve the import
pub struct CollectionImporter {
    data: HashMap<String, Variables>,
}

impl Importer for CollectionImporter {
    fn fetch<'a>(&'a self, from: &str) -> Result<ImportFetch<'a>, Error> {
        match self.data.get(from) {
            None => Err(Error::new(format!("file '{}' not found", from))),
            Some(variables) => Ok(ImportFetch::Evaluated(variables)),
        }
    }
}

impl CollectionImporter {
    pub fn new(variables: HashMap<String, Variables>) -> Self {
        CollectionImporter { data: variables }
    }
}
