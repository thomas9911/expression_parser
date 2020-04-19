// use crate::ExpressionValue;

#[derive(Debug, PartialEq)]
pub struct Error {
    info: Option<String>,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.info {
            Some(ref x) => write!(f, "{}", x),
            None => write!(f, "Unable to evaluate expression"),
        }
    }
}

// impl From<Option<ExpressionValue>> for Error {
//     fn from(error: Option<ExpressionValue>) -> Self {
//         Error {
//             info: Some(String::new()),
//         }
//     }
// }

// impl From<std::option::NoneError> for Error {
//     fn from(_: std::option::NoneError) -> Self {
//         Error::empty()
//     }
// }

impl Error {
    pub fn new(info: String) -> Self {
        Error { info: Some(info) }
    }

    pub fn empty() -> Self {
        Error { info: None }
    }
}
