use crate::statics::DEFAULT_VARIABLES;
use crate::string_expression::EvalResult;
use crate::{Error, Expression, VariableMap, Variables};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

mod functions;

#[derive(Debug, Clone, PartialEq, EnumString, EnumMessage, EnumIter, EnumDiscriminants)]
#[strum(serialize_all = "snake_case")]
#[strum_discriminants(derive(EnumString))]
#[strum_discriminants(
    name(FunctionName),
    strum(serialize_all = "snake_case"),
    derive(Display, IntoStaticStr)
)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Function {
    #[strum(message = "Combines two or more lists into one")]
    Concat(Vec<Expression>),
    #[strum(message = "Sums up the arguments")]
    Sum(Vec<Expression>),
    #[strum(message = "Calculates the product of the arguments")]
    Product(Vec<Expression>),
    #[strum(message = "Checks if all arguments are truthy")]
    All(Vec<Expression>),
    #[strum(message = "Checks if any arguments are truthy")]
    Any(Vec<Expression>),
    #[strum(message = "Compares if the two arguments are equal")]
    Equal(Expression, Expression),
    #[strum(message = "Compares if the two arguments are not equal")]
    NotEqual(Expression, Expression),
    #[strum(
        message = "If the first argument is truthy returns the second argument, otherwise returns the first argument"
    )]
    And(Expression, Expression),
    #[strum(
        message = "If the first argument is truthy returns the first argument, otherwise returns the second argument"
    )]
    Or(Expression, Expression),
    #[strum(
        message = "Removes the characters at the start and end of the first argument, second argument is an optional argument that contains the character to remove, defaults to ' '"
    )]
    Trim(Expression, Expression),
    #[strum(message = "Checks if the seconds arguement is in the first argument")]
    Contains(Expression, Expression),
    #[strum(
        message = "Combine the first argument into a string with the second argument as is seperator"
    )]
    Join(Expression, Expression),
    #[strum(
        message = "If the first argument is truthy returns the second argument, otherwise returns the third argument"
    )]
    If(Expression, Expression, Expression),
    #[strum(message = "Converts input to uppercase")]
    Upper(Expression),
    #[strum(message = "Converts input to lowercase")]
    Lower(Expression),
    #[strum(message = "Adds the two arguments together")]
    Add(Expression, Expression),
    #[strum(message = "Subtracts the second argument from the first argument")]
    Sub(Expression, Expression),
    #[strum(message = "Multiplies the two arguments together")]
    Mul(Expression, Expression),
    #[strum(message = "Divides the second argument from the first argument")]
    Div(Expression, Expression),
    #[strum(message = "Raises the first argument to the second argument")]
    Pow(Expression, Expression),
    #[strum(message = "Calculates the cosine of the number")]
    Cos(Expression),
    #[strum(message = "Calculates the sine of the number")]
    Sin(Expression),
    #[strum(message = "Calculates the tangent of the number")]
    Tan(Expression),
    #[strum(
        message = "Generate a random number. Defaults to a number between 0 and 1, but the range can be set as argument"
    )]
    Random(Expression, Expression),
    #[strum(message = "Returns the unix timestamp of the current time")]
    Now(),
    #[strum(message = "Prints value")]
    Print(Expression),
    #[strum(message = "Prints help message")]
    Help(Expression),
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Function::*;

        let func_name = FunctionName::from(self);
        match self {
            // functions
            Concat(list) | Sum(list) | Product(list) | All(list) | Any(list) => {
                write!(f, "{}({})", func_name, list_to_string(list).join(", "))
            }
            If(lhs, mdl, rhs) => write!(f, "{}({}, {}, {})", func_name, lhs, mdl, rhs),
            Trim(lhs, rhs) | Contains(lhs, rhs) | Join(lhs, rhs) | Random(lhs, rhs) => {
                write!(f, "{}({}, {})", func_name, lhs, rhs)
            }
            Upper(lhs) | Lower(lhs) | Cos(lhs) | Sin(lhs) | Tan(lhs) | Print(lhs) | Help(lhs) => {
                write!(f, "{}({})", func_name, lhs)
            }
            Now() => write!(f, "{}()", func_name),

            // infixes
            Equal(lhs, rhs) => write!(f, "({} == {})", lhs, rhs),
            NotEqual(lhs, rhs) => write!(f, "({} != {})", lhs, rhs),
            And(lhs, rhs) => write!(f, "({} and {})", lhs, rhs),
            Or(lhs, rhs) => write!(f, "({} or {})", lhs, rhs),
            Add(lhs, rhs) => write!(f, "({} + {})", lhs, rhs),
            Sub(lhs, rhs) => write!(f, "({} - {})", lhs, rhs),
            Mul(lhs, rhs) => write!(f, "({} * {})", lhs, rhs),
            Div(lhs, rhs) => write!(f, "({} / {})", lhs, rhs),
            Pow(lhs, rhs) => write!(f, "({} ^ {})", lhs, rhs),
        }
    }
}

impl Function {
    pub fn eval<V: VariableMap>(operator: Function, vars: &V) -> EvalResult {
        use Function::*;

        match operator {
            Concat(list) => functions::concat(list, vars),
            Sum(list) => functions::sum(list, vars),
            Product(list) => functions::product(list, vars),
            All(list) => functions::all(list, vars),
            Any(list) => functions::any(list, vars),
            Trim(lhs, rhs) => functions::trim(lhs, rhs, vars),
            Equal(lhs, rhs) => functions::equal(lhs, rhs, vars),
            NotEqual(lhs, rhs) => functions::not_equal(lhs, rhs, vars),
            And(lhs, rhs) => functions::and(lhs, rhs, vars),
            Or(lhs, rhs) => functions::or(lhs, rhs, vars),
            Contains(lhs, rhs) => functions::contains(lhs, rhs, vars),
            Join(lhs, rhs) => functions::join(lhs, rhs, vars),
            If(lhs, mdl, rhs) => functions::if_function(lhs, mdl, rhs, vars),
            Upper(lhs) => functions::upper(lhs, vars),
            Lower(lhs) => functions::lower(lhs, vars),
            Add(lhs, rhs) => functions::add(lhs, rhs, vars),
            Sub(lhs, rhs) => functions::sub(lhs, rhs, vars),
            Mul(lhs, rhs) => functions::mul(lhs, rhs, vars),
            Div(lhs, rhs) => functions::div(lhs, rhs, vars),
            Pow(lhs, rhs) => functions::pow(lhs, rhs, vars),
            Cos(lhs) => functions::cos(lhs, vars),
            Sin(lhs) => functions::sin(lhs, vars),
            Tan(lhs) => functions::tan(lhs, vars),
            Random(lhs, rhs) => functions::random(lhs, rhs, vars),
            Now() => functions::now(vars),
            Print(lhs) => functions::print(lhs, vars),
            Help(lhs) => functions::help(lhs, vars),
        }
    }

    pub fn compile(operator: Function) -> Result<Expression, Error> {
        use Expression as E;
        use Expression::*;
        use Function::*;

        if operator.contains_variables() | operator.cannot_be_pre_evaluated() {
            let funcs = match operator.to_owned() {
                Concat(list) => Concat(Function::compile_list(list)?),
                Sum(list) => Sum(Function::compile_list(list)?),
                Product(list) => Product(Function::compile_list(list)?),
                All(list) => All(Function::compile_list(list)?),
                Any(list) => Any(Function::compile_list(list)?),
                Trim(lhs, rhs) => Trim(E::compile(lhs)?, E::compile(rhs)?),
                Equal(lhs, rhs) => Equal(E::compile(lhs)?, E::compile(rhs)?),
                NotEqual(lhs, rhs) => NotEqual(E::compile(lhs)?, E::compile(rhs)?),
                And(lhs, rhs) => And(E::compile(lhs)?, E::compile(rhs)?),
                Or(lhs, rhs) => Or(E::compile(lhs)?, E::compile(rhs)?),
                Contains(lhs, rhs) => Contains(E::compile(lhs)?, E::compile(rhs)?),
                Join(lhs, rhs) => Join(E::compile(lhs)?, E::compile(rhs)?),
                If(lhs, mdl, rhs) => If(E::compile(lhs)?, E::compile(mdl)?, E::compile(rhs)?),
                Upper(lhs) => Upper(E::compile(lhs)?),
                Lower(lhs) => Lower(E::compile(lhs)?),
                Add(lhs, rhs) => Add(E::compile(lhs)?, E::compile(rhs)?),
                Sub(lhs, rhs) => Sub(E::compile(lhs)?, E::compile(rhs)?),
                Mul(lhs, rhs) => Mul(E::compile(lhs)?, E::compile(rhs)?),
                Div(lhs, rhs) => Div(E::compile(lhs)?, E::compile(rhs)?),
                Pow(lhs, rhs) => Pow(E::compile(lhs)?, E::compile(rhs)?),
                Cos(lhs) => Cos(E::compile(lhs)?),
                Sin(lhs) => Sin(E::compile(lhs)?),
                Tan(lhs) => Tan(E::compile(lhs)?),
                Random(lhs, rhs) => Random(E::compile(lhs)?, E::compile(rhs)?),
                Now() => Now(),
                Print(lhs) => Print(E::compile(lhs)?),
                Help(lhs) => Help(E::compile(lhs)?),
            };

            Ok(Expr(Box::new(funcs)))
        } else {
            Ok(Value(Function::eval(operator, &Variables::default())?))
        }
    }

    fn compile_list(list: Vec<Expression>) -> Result<Vec<Expression>, Error> {
        list.into_iter().try_fold(Vec::new(), |mut acc, x| {
            acc.push(Expression::compile(x)?);
            Ok(acc)
        })
    }

    fn cannot_be_pre_evaluated(&self) -> bool {
        use Function::*;
        match self {
            Random(_, _) | Now() | Print(_) => true,
            _ => false,
        }
    }

    fn contains_variables(&self) -> bool {
        self.iter_variables_without_defaults().count() != 0
    }

    pub fn iter_variables<'a>(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        use Function::*;

        match self {
            Trim(lhs, rhs)
            | Equal(lhs, rhs)
            | NotEqual(lhs, rhs)
            | And(lhs, rhs)
            | Or(lhs, rhs)
            | Contains(lhs, rhs)
            | Join(lhs, rhs)
            | Add(lhs, rhs)
            | Sub(lhs, rhs)
            | Mul(lhs, rhs)
            | Div(lhs, rhs)
            | Pow(lhs, rhs)
            | Random(lhs, rhs) => Box::new(lhs.iter_variables().chain(rhs.iter_variables())),

            Lower(lhs) | Upper(lhs) | Cos(lhs) | Sin(lhs) | Tan(lhs) | Print(lhs) | Help(lhs) => {
                Box::new(lhs.iter_variables())
            }

            If(lhs, mdl, rhs) => Box::new(
                lhs.iter_variables()
                    .chain(mdl.iter_variables())
                    .chain(rhs.iter_variables()),
            ),

            Concat(list) | Product(list) | Sum(list) | All(list) | Any(list) => {
                Box::new(list.iter().flat_map(|x| x.iter_variables()))
            }

            Now() => Box::new(std::iter::empty::<String>()),
        }
    }

    pub fn iter_variables_without_defaults<'a>(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
        Box::new(
            self.iter_variables()
                .filter(|x| !DEFAULT_VARIABLES.contains_key(x)),
        )
    }

    pub fn is_infix(&self) -> bool {
        self.inflix_symbol().is_some()
    }

    pub fn inflix_symbol(&self) -> Option<&'static str> {
        use FunctionName::*;
        match FunctionName::from(self) {
            Equal => Some("=="),
            NotEqual => Some("!="),
            And => Some("and"),
            Or => Some("or"),
            Add => Some("+"),
            Sub => Some("-"),
            Mul => Some("*"),
            Div => Some("/"),
            Pow => Some("^"),
            _ => None,
        }
    }

    pub fn help() -> String {
        use strum::{EnumMessage, IntoEnumIterator};

        Function::iter().fold(String::from("\n"), |mut acc, item| {
            let func_name = item
                .inflix_symbol()
                .unwrap_or(FunctionName::from(&item).into());
            acc.push_str(&format!(
                "{}:\n\t{}\n",
                func_name,
                item.get_message().unwrap_or_default()
            ));
            acc
        })
    }

    pub fn get_usage(&self) -> &'static str {
        use FunctionName::*;
        match FunctionName::from(self) {
            Equal => {
                r#"
    first = 1 == 1; 
    second = [1,2,3] == [1,2,3]; 
    third = {"test": true} == {"test": true}; 
    first and second and third"#
            }
            NotEqual => {
                r#"
    first = 1 != 2; 
    second = [1,2,3] != [3,2,1]; 
    third = {"test": true} != {"test": false}; 
    first and second and third"#
            }
            And => {
                r#"
    first = true and true;
    second = 1 and true;
    third = [1,2,3] and true;
    fourth = 0 and 1234 == 0;
    first and second and third and fourth"#
            }
            Or => {
                r#"
    first = false or true;
    second = 0.0 or true;
    third = [] or true;
    first and second and third"#
            }
            All => {
                r#"
    first = all([1, true, [1,2,3], {"test": true}]);
    first"#
            }
            Any => {
                r#"
    first = any([1, false, [1,2,3], {"test": true}]);
    second = any([0.0, true, [], {}]);
    first and second"#
            }
            Add => {
                r#"
    first = 1 + 1 == 2;
    second = 123 + 321 == 444;
    third = -321 + 321 == 0;
    first and second and third"#
            }
            Sub => {
                r#"
    first = 1 - 1 == 0;
    second = 421 - 321 == 100;
    third = -123 - 321 == -444;
    first and second and third"#
            }
            Mul => {
                r#"
    first = 1 * 1 == 1;
    second = 150 * 0 == 0;
    third = -5 * -5 == 25;
    first and second and third"#
            }
            Div => {
                r#"
    first = 1 / 5 == 0.2;
    second = 50 / 50 == 1;
    third = 150 / 6 == 25;
    first and second and third"#
            }
            Pow => {
                r#"
    first = 1 ** 1 == 1;
    second = 2 ^ 3 == 8;
    third = 16 ** 0.5 == 4;
    first and second and third"#
            }
            Trim => {
                r#"
    first = trim("__Testing_Test__", "_") == "Testing_Test";
    second = trim("A sentence.\n\n\n\n\n", "\n") == "A sentence.";
    first and second"#
            }
            Join => {
                r#"
    first = join(["1", "2", "3"], ", ") == "1, 2, 3";
    second = join(["test", "testing", "test"], "-") == "test-testing-test";
    first and second"#
            }
            If => {
                r#"
    first = if(1 == 1, "success", "failed") == "success";
    
    one = "1";
    two = "2";
    some_test = e != pi;
    second = if(some_test, one, two) == one;
    first and second"#
            }
            Random => {
                r#"
    // random() returns a random number between 0 and 1
    first = random() != random();
    second = random(-1, 1) != random(2, 5);
    third = random(-5) != random(5);
    first and second and third"#
            }
            Concat => {
                r#"
    first = concat([1,2], [3,4], [5,6]) == [1,2,3,4,5,6];
    second = concat("test", "-", "test") == "test-test";
    first and second"#
            }
            Sum => {
                r#"
    first = sum(1,2,3,4,5) == 15;
    first"#
            }
            Product => {
                r#"
    first = product(1,2,3,4,5) == 120;
    first"#
            }
            Lower => {
                r#"
    first = lower("TESTING") == "testing";
    first"#
            }
            Upper => {
                r#"
    first = upper("testing") == "TESTING";
    first"#
            }
            Contains => {
                r#"
    first = contains("testing", "test");
    second = contains([1,2,3,4], 3);
    first and second"#
            }
            Sin => {
                r#"
    first = sin(0) == 0;
    first"#
            }
            Cos => {
                r#"
    first = cos(0) == 1;
    first"#
            }
            Tan => {
                r#"
    first = tan(0) == 0;
    first"#
            }
            Help => {
                r#"
    // prints general help
    help();

    // prints help for specific function
    help(contains);

    // print all the functions
    help(functions);
    true"#
            }
            Now => {
                r#"
    now();
    true"#
            }
            Print => "true",
        }
    }
}

fn list_to_string(input: &Vec<Expression>) -> Vec<String> {
    input.iter().map(|x| format!("{}", x)).collect()
}

#[test]
fn function_names_test() {
    use std::str::FromStr;

    let t = FunctionName::from_str("now").unwrap();
    let r = Function::Now();
    let p = FunctionName::from(&r);

    assert_eq!(t, p);

    assert_eq!("now", p.to_string());
    assert_eq!("now()", r.to_string());
}

// #[test]
// fn xd() {
//     use std::str::FromStr;
//     use strum::EnumMessage;

//     let t = Function::from_str("now").unwrap();
//     let r = Function::Now();
//     let p = FunctionName::from(&r);

//     println!("{:?}", t.get_message());

//     assert!(false)
// }

#[test]
fn function_doc_tests() {
    use crate::{ExpressionFile, ExpressionValue};
    use strum::IntoEnumIterator;

    for function in Function::iter() {
        let val = ExpressionFile::eval(
            ExpressionFile::parse(function.get_usage()).unwrap(),
            &mut Variables::default(),
        )
        .unwrap();

        // assert_eq!(val, ExpressionValue::Bool(true))
        if val != ExpressionValue::Bool(true) {
            panic!("usage test for \"{}\" fails", FunctionName::from(function))
        }
    }
}
