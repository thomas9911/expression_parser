#[derive(Debug, PartialEq)]
pub struct Error {
    pub static_info: Option<&'static str>,
    pub info: Option<String>,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.static_info {
            Some(x) => write!(f, "{}", x),
            None => match self.info {
                Some(ref x) => write!(f, "{}", x),
                None => write!(f, "Unable to evaluate expression"),
            },
        }
    }
}

impl Error {
    pub fn new(info: String) -> Self {
        Error {
            info: Some(info),
            static_info: None,
        }
    }

    pub fn new_static(info: &'static str) -> Self {
        Error {
            static_info: Some(info),
            info: None,
        }
    }

    pub fn empty() -> Self {
        Error {
            info: None,
            static_info: None,
        }
    }
}
