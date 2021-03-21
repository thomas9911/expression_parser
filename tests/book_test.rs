use expression_parser::{Environment, Error, ExpressionFile, ExpressionValue};

type Result = std::result::Result<ExpressionValue, Error>;

fn running_default(input: &str) -> Result {
    ExpressionFile::run(input, &mut Environment::default())
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

    #[test]
    fn recursion() {
        assert_eq!(
            Ok(40320.into()),
            running_default(
                r#"
// ANCHOR: chapter_4_recursion
factorial = {n => 
    if((n == 0) or (n == 1), 1, n * factorial.(n-1))
};
factorial.(8) // returns 40320
// ANCHOR_END: chapter_4_recursion
        "#
            )
        );
    }
}

mod chapter_6 {
    #[test]
    fn simple() {
        // ANCHOR: chapter_6_simple
        use expression_parser::{Environment, ExpressionFile};

        let input = r#"
        a = 1 + 1;
        a + 5        
        "#;

        // ofcourse handle this better in you code
        let parsed_expression = ExpressionFile::parse(input).unwrap();

        // you can now decide what to do with the expression

        // we will just evaluate it here with default variables.

        let mut vars = Environment::default();
        let output = ExpressionFile::eval(parsed_expression, &mut vars).unwrap();
        assert_eq!(output, 7.into());
        // ANCHOR_END: chapter_6_simple
    }

    #[test]
    fn extra_vars() {
        // ANCHOR: chapter_6_extra_vars
        use expression_parser::{Env, Environment, ExpressionFile};

        let input = r#"
        5 * DATA     
        "#;

        let parsed_expression = ExpressionFile::parse(input).unwrap();

        let mut env = Environment::default();
        env.variables_mut().insert("DATA", 1234.into());

        let output = ExpressionFile::eval(parsed_expression, &mut env).unwrap();
        assert_eq!(output, 6170.into());
        // ANCHOR_END: chapter_6_extra_vars
    }

    #[test]
    fn extra_functions() {
        // ANCHOR: chapter_6_extra_functions
        use expression_parser::{
            Closure, Env, Environment, Error, ExpressionFile, ExpressionValue,
        };
        use std::sync::Arc;

        let mut env = Environment::default();

        // a `Closure` struct is just a container for holding the function.
        // `new` takes a list of the arguments used (only for debugging purposes)
        // and an `Arc` with a `Box`ed function with two arguments.
        // the first is a list containing all the arguments given by the user. These need to be validated yourself.
        // the second argument is a `Environment` struct that has methods to access variables outside the function and side effects.
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
        env.variables_mut().insert("external_func", closure.into());

        let script = r#"
        external_func.(6, 2)
        "#;

        let parsed = ExpressionFile::parse(script).unwrap();
        let result = ExpressionFile::eval(parsed, &mut env);
        assert_eq!(result, Ok(12.into()))
        // ANCHOR_END: chapter_6_extra_functions
    }

    #[test]
    fn extra_functions2() {
        // ANCHOR: chapter_6_extra_functions2
        use expression_parser::{Closure, Env, Environment, Error, Expression, ExpressionFile};
        use std::sync::Arc;

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
                let result = Expression::eval(result.clone(), &mut Box::new(context))?;

                Ok(result)
            })),
        );

        let mut env = Environment::default();
        env.variables_mut().insert("map_get", closure.into());

        let script = r#"
        map_get.(
            {"test": "some_value"},
            "test"
        )
        "#;

        let result = ExpressionFile::run(script, &mut env);
        assert_eq!(result, Ok("some_value".into()));

        let script = r#"
        text = "some_variable";
        map_get.(
            {"test": text},
            "test"
        )
        "#;

        let result = ExpressionFile::run(script, &mut env);
        assert_eq!(result, Ok("some_variable".into()));
        // ANCHOR_END: chapter_6_extra_functions2
    }

    #[test]
    fn own_variables() {
        // ANCHOR: chapter_6_own_variables
        use expression_parser::{Environment, ExpressionFile, ExpressionValue};
        use std::collections::HashMap;

        // if you like python keywords better than the names I picked
        let mut my_variables = HashMap::new();
        my_variables.insert(String::from("True"), ExpressionValue::Bool(true));
        my_variables.insert(String::from("False"), ExpressionValue::Bool(false));
        my_variables.insert(String::from("None"), ExpressionValue::Null);

        // `Environment::builder` lets you configure your environment yourself.
        let mut env = Environment::builder()
            .with_variables(Box::new(my_variables))
            .build();

        let input = r#"
            a = True and True;
            b = False or True;
            c = None or True;
            all(a, b, c)
        "#;

        let parsed_expression = ExpressionFile::parse(input).unwrap();

        let output = ExpressionFile::eval(parsed_expression, &mut env).unwrap();
        assert_eq!(output, true.into());
        // ANCHOR_END: chapter_6_own_variables
    }

    #[test]
    fn extend_variables() {
        // ANCHOR: chapter_6_extend_variables
        use expression_parser::{Environment, ExpressionFile, VariableMap, Variables};

        // `Variables::default` returns a `VariableMap` with all the default variables
        let mut my_variables = Variables::default();
        // so this allready exists in the variables.
        assert!(my_variables.insert("true", true.into()).is_some());
        my_variables.insert("DATA", vec![1, 5, 1].into());
        my_variables.insert("SOME", true.into());

        // `Environment::builder` lets you configure your environment yourself.
        let mut env = Environment::builder()
            .with_variables(Box::new(my_variables))
            .build();

        let input = r#"
            // returns DATA because SOME is true 
            if(SOME, DATA, error("data not found"))
        "#;

        let parsed_expression = ExpressionFile::parse(input).unwrap();

        let output = ExpressionFile::eval(parsed_expression, &mut env).unwrap();
        assert_eq!(output, vec![1, 5, 1].into());
        // ANCHOR_END: chapter_6_extend_variables
    }
}
