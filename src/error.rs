use crate::function::functions::format::Rule as FormatRule;
use crate::grammar::Rule;
use pest::error::Error as PestError;

#[derive(Debug, PartialEq)]
pub enum Error {
    Static(&'static str),
    String(String),
    Code(ErrorCodes),
    Parse(PestError<Rule>),
    Empty,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Static(info) => write!(f, "{}", info),
            Error::String(info) => write!(f, "{}", info),
            Error::Parse(info) => write!(f, "{}", info),
            Error::Code(info) => write!(f, "{}", info),
            Error::Empty => write!(f, "Unable to evaluate expression"),
        }
    }
}

impl Error {
    pub fn new(info: String) -> Self {
        Error::String(info)
    }

    pub fn new_static(info: &'static str) -> Self {
        Error::Static(info)
    }

    pub fn new_code(info: ErrorCodes) -> Self {
        Error::Code(info)
    }

    pub fn empty() -> Self {
        Error::Empty
    }
}

impl From<PestError<Rule>> for Error {
    fn from(error: PestError<Rule>) -> Error {
        Error::Parse(error)
    }
}

impl From<PestError<FormatRule>> for Error {
    fn from(error: PestError<FormatRule>) -> Error {
        Error::new(format!("{}", error))
    }
}

impl From<ErrorCodes> for Error {
    fn from(error: ErrorCodes) -> Error {
        Error::new_code(error)
    }
}

#[derive(Debug, PartialEq)]
pub enum ErrorCodes {
    STACKOVERFLOW,
}

impl std::fmt::Display for ErrorCodes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
