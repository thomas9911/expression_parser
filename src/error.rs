use crate::function::functions::format::Rule as FormatRule;
use crate::grammar::Rule;
use pest::error::Error as PestError;

#[derive(Debug, PartialEq)]
pub struct Error {
    pub static_info: Option<&'static str>,
    pub info: Option<String>,
    pub code: Option<ErrorCodes>,
}

#[derive(Debug, PartialEq)]
pub enum ErrorCodes {
    STACKOVERFLOW,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(info) = self.static_info {
            return write!(f, "{}", info);
        }
        if let Some(ref info) = self.info {
            return write!(f, "{}", info);
        }
        if let Some(ref info) = self.code {
            return write!(f, "{:?}", info);
        }
        write!(f, "Unable to evaluate expression")
    }
}

impl Error {
    pub fn new(info: String) -> Self {
        Error {
            info: Some(info),
            static_info: None,
            code: None,
        }
    }

    pub fn new_static(info: &'static str) -> Self {
        Error {
            static_info: Some(info),
            info: None,
            code: None,
        }
    }

    pub fn new_code(info: ErrorCodes) -> Self {
        Error {
            static_info: None,
            info: None,
            code: Some(info),
        }
    }

    pub fn empty() -> Self {
        Error {
            info: None,
            static_info: None,
            code: None,
        }
    }
}

impl From<PestError<Rule>> for Error {
    fn from(error: PestError<Rule>) -> Error {
        Error::new(format!("{}", error))
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
