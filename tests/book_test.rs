use expression_parser::{
    Error, Expression, ExpressionFile, ExpressionMap, ExpressionValue, VariableMap, Variables,
};

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
        use expression_parser::{ExpressionFile, Variables, VariableMap};

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
        use std::sync::Arc;
        use expression_parser::{Closure, Error, ExpressionFile, ExpressionValue, Variables, VariableMap};

        let mut vars = Variables::new();
        let closure = Closure::new(
            vec!["x".to_string(), "y".to_string()],
            Arc::new(Box::new(|vars, _| {
                fn unwrap_number(x: Option<&ExpressionValue>) -> Result<f64, Error> {
                    x.ok_or(Error::new_static("missing arguments"))?
                        .as_number()
                        .ok_or(Error::new_static("argument not a number"))
                }

                let x = unwrap_number(vars.get(0))?;
                let y = unwrap_number(vars.get(1))?;
                let result = x * y;

                Ok(result.into())
            })),
        );
        vars.insert("external_func", closure.into());

        let script = r#"
        call(external_func, 6, 2)
        "#;

        let parsed = ExpressionFile::parse(script).unwrap();
        let result = ExpressionFile::eval(parsed, &mut vars);
        assert_eq!(result, Ok(12.into()))
        // ANCHOR_END: chapter_5_extra_functions
    }
}
