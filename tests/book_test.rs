use expression_parser::{Error, ExpressionFile, ExpressionValue, Variables};

type Result = std::result::Result<ExpressionValue, Error>;

fn running_default(input: &str) -> Result {
    ExpressionFile::run(input, &mut Variables::default())
}

mod chapter_1 {
    use super::*;

    #[test]
    fn numbers() {
        assert!(running_default(
            r#"
// ANCHOR: chapter_1_numbers
123;
12.345;
1.23e6; // 1230000
1.23e-3; // 0.00123
// ANCHOR_END: chapter_1_numbers
        "#
        )
        .is_ok());
    }

    #[test]
    fn string() {
        assert!(running_default(
            r#"
// ANCHOR: chapter_1_strings
"Text";
"More text";
// unicode rust
"ok\u{00e9}"; // prints oké
// also json unicode
"ok\u00e9"; // prints oké as well
"prints\t\tescaped\n\"characters\"\\";
// prints         escaped
// "characters"\
// ANCHOR_END: chapter_1_strings
        "#
        )
        .is_ok());
    }

    #[test]
    fn boolean() {
        assert!(running_default(
            r#"
// ANCHOR: chapter_1_booleans
true;
false;
// ANCHOR_END: chapter_1_booleans
        "#
        )
        .is_ok());
    }

    #[test]
    fn null() {
        assert!(running_default(
            r#"
// ANCHOR: chapter_1_null
null;
// ANCHOR_END: chapter_1_null
        "#
        )
        .is_ok());
    }

    #[test]
    fn lists() {
        assert!(running_default(
            r#"
// ANCHOR: chapter_1_lists
[ 1, 2, 3 ];
["test", "test"];
// also mixed type lists
[1, "test", false];
// ANCHOR_END: chapter_1_lists
        "#
        )
        .is_ok());
    }

    #[test]
    fn maps() {
        assert!(running_default(
            r#"
// ANCHOR: chapter_1_maps
// just your regular json
{
    "test": 123,
    "list": [1,2,3],
    "map": {
        "nested": true
    }
}
// ANCHOR_END: chapter_1_maps
        "#
        )
        .is_ok());
    }

    #[test]
    fn function() {
        assert!(running_default(
            r#"
// ANCHOR: chapter_1_function
// simple example
{x => x + x}
// ANCHOR_END: chapter_1_function
        "#
        )
        .is_ok());
    }
}

mod chapter_2 {
    use super::*;

    #[test]
    fn assignment() {
        assert!(running_default(
            r#"
// ANCHOR: chapter_2_assignment
a = 1;
b = 2;
c = a + b;
// ANCHOR_END: chapter_2_assignment
        "#
        )
        .is_ok());
    }

    #[test]
    fn unassignment() {
        assert!(running_default(
            r#"
// ANCHOR: chapter_2_unassignment
a = 1;
unset a;
// ANCHOR_END: chapter_2_unassignment
        "#
        )
        .is_ok());
    }
}

mod chapter_4 {
    use super::*;

    #[test]
    fn simple() {
        assert_eq!(
            Ok(6.into()),
            running_default(
                r#"
// ANCHOR: chapter_4_simple
my_func = {x, y =>
    x + y
};
my_func.(2, 4) // returns 6
// ANCHOR_END: chapter_4_simple
        "#
            )
        );
    }

    #[test]
    fn globals() {
        assert_eq!(
            Ok(19.into()),
            running_default(
                r#"
// ANCHOR: chapter_4_globals
GLOBAL = 15;
my_func = {x =>
    GLOBAL + x
};
my_func.(4) // returns 19
// ANCHOR_END: chapter_4_globals
        "#
            )
        );
    }

    #[test]
    fn curry() {
        assert_eq!(
            Ok(16.into()),
            running_default(
                r#"
// ANCHOR: chapter_4_curry
factory = {x =>
    { y => x + y }
};
generated_function = factory.(4);
generated_function.(12) // returns 16
// ANCHOR_END: chapter_4_curry
        "#
            )
        );
    }
}

mod chapter_5 {
    #[test]
    fn simple() {
        // ANCHOR: chapter_5_simple
        use expression_parser::{ExpressionFile, Variables};

        let input = r#"
        a = 1 + 1;
        a + 5        
        "#;

        // ofcourse handle this better in you code
        let parsed_expression = ExpressionFile::parse(input).unwrap();

        // you can now decide what to do with the expression

        // we will just evaluate it here with default variables.

        let mut vars = Variables::default();
        let output = ExpressionFile::eval(parsed_expression, &mut vars).unwrap();
        assert_eq!(output, 7.into());
        // ANCHOR_END: chapter_5_simple
    }

    #[test]
    fn extra_vars() {
        // ANCHOR: chapter_5_extra_vars
        use expression_parser::{ExpressionFile, VariableMap, Variables};

        let input = r#"
        5 * DATA     
        "#;

        let parsed_expression = ExpressionFile::parse(input).unwrap();

        let mut vars = Variables::default();
        vars.insert("DATA", 1234.into());

        let output = ExpressionFile::eval(parsed_expression, &mut vars).unwrap();
        assert_eq!(output, 6170.into());
        // ANCHOR_END: chapter_5_extra_vars
    }

    #[test]
    fn extra_functions() {
        // ANCHOR: chapter_5_extra_functions
        use expression_parser::{
            Closure, Error, ExpressionFile, ExpressionValue, VariableMap, Variables,
        };
        use std::sync::Arc;

        let mut vars = Variables::new();

        // a `Closure` struct is just a container for holding the function.
        // `new` takes a list of the arguments used (only for debugging purposes)
        // and an `Arc` with a `Box`ed function with two arguments.
        // the first is a list containing all the arguments given by the user. These need to be validated yourself.
        // the second argument is a ScopedVariables struct that acts like a Map that contains all the variables used above the function call.
        // this can be used for instance for config options.
        // the return value is a `Result<ExpressionValue, Error>`
        let closure = Closure::new(
            vec!["x".to_string(), "y".to_string()],
            Arc::new(Box::new(|vars, _| {
                /// validate the arguments or return an error
                fn validate_number(x: Option<&ExpressionValue>) -> Result<f64, Error> {
                    x.ok_or(Error::new_static("missing arguments"))?
                        .as_number()
                        .ok_or(Error::new_static("argument not a number"))
                }

                let x = validate_number(vars.get(0))?;
                let y = validate_number(vars.get(1))?;
                let result = x * y;

                // the `into` casts the rust value into a `ExpressionValue` enum
                Ok(result.into())
            })),
        );
        vars.insert("external_func", closure.into());

        let script = r#"
        external_func.(6, 2)
        "#;

        let parsed = ExpressionFile::parse(script).unwrap();
        let result = ExpressionFile::eval(parsed, &mut vars);
        assert_eq!(result, Ok(12.into()))
        // ANCHOR_END: chapter_5_extra_functions
    }

    #[test]
    fn extra_functions2() {
        // ANCHOR: chapter_5_extra_functions2
        use expression_parser::{
            Closure, Error, Expression, ExpressionFile, VariableMap, Variables,
        };
        use std::sync::Arc;

        let mut vars = Variables::new();

        let closure = Closure::new(
            vec!["map".to_string(), "key".to_string()],
            Arc::new(Box::new(|vars, context| {
                let map = vars
                    .get(0)
                    .ok_or(Error::new_static("expect a map as the first argument"))?
                    // probably do something more smart than just clone
                    .clone()
                    .as_map()
                    .ok_or(Error::new_static("expect a map as the first argument"))?;
                let key = vars
                    .get(1)
                    .ok_or(Error::new_static("expect a key as the second argument"))?
                    .as_string()
                    .ok_or(Error::new_static("expect a key as the second argument"))?;

                // get access to the underlying HashMap
                let result = map.0.get(&key).ok_or(Error::new_static("key not found"))?;

                // the result is an expression, these can include variables ect.
                // We can just match on the value or eval the Expression with the current context.
                let result = Expression::eval(result.clone(), &context)?;

                Ok(result)
            })),
        );
        vars.insert("map_get", closure.into());

        let script = r#"
        map_get.(
            {"test": "some_value"},
            "test"
        )
        "#;

        let result = ExpressionFile::run(script, &mut vars);
        assert_eq!(result, Ok("some_value".into()));

        let script = r#"
        text = "some_variable";
        map_get.(
            {"test": text},
            "test"
        )
        "#;

        let result = ExpressionFile::run(script, &mut vars);
        assert_eq!(result, Ok("some_variable".into()));
        // ANCHOR_END: chapter_5_extra_functions2
    }
}
