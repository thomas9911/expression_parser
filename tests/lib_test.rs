use expression_parser::VariableMap;

mod iter_variables {
    use expression_parser::Expression;

    #[test]
    fn simple_expression() {
        let parsed = Expression::parse(r#"true | false"#).unwrap();
        let result: Vec<String> = parsed.iter_variables().collect();
        assert_eq!(result, ["true", "false"]);
    }

    #[test]
    fn advanced_expression() {
        let parsed = Expression::parse(
            r#"
            upper(
                concat(
                    1234, 
                    "_", 
                    contains(
                        "test", 
                        something
                    ) or trim(user_input, "_"),
                    "_", 
                    true
                )
            )
        "#,
        )
        .unwrap();
        let result: Vec<String> = parsed.iter_variables().collect();
        assert_eq!(result, ["something", "user_input", "true"]);
    }

    #[test]
    fn simple_expression_without_defaults() {
        let parsed = Expression::parse(r#"true | false | test"#).unwrap();
        let result: Vec<String> = parsed.iter_variables_without_defaults().collect();
        assert_eq!(result, ["test"]);
    }
}

mod or {
    use expression_parser::{Expression, ExpressionValue, Variables};

    #[test]
    fn operator_true() {
        let parsed = Expression::parse(r#"true | false"#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(true.into()), result);
    }

    #[test]
    fn operator_false() {
        let parsed = Expression::parse(r#"false | false"#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(false.into()), result);
    }

    #[test]
    fn operator_false_string() {
        let parsed = Expression::parse(r#"false | "test""#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok("test".into()), result);
    }

    #[test]
    fn operator_true_string() {
        let parsed = Expression::parse(r#"true | "test""#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(true.into()), result);
    }

    #[test]
    fn operator_false_number_string() {
        let parsed = Expression::parse(r#"0 | "test""#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok("test".into()), result);
    }

    #[test]
    fn operator_true_number_string() {
        let parsed = Expression::parse(r#"1 | "test""#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(1.0f64.into()), result);
    }

    #[test]
    fn operator_string_string() {
        let parsed = Expression::parse(r#""test" | "other""#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok("test".into()), result);
    }

    #[test]
    fn operator_vec_string() {
        use Expression::Value;
        use ExpressionValue::{List, Number};

        let parsed = Expression::parse(r#"[1] | "other""#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(List(vec![Value(Number(1.0))])), result);
    }

    #[test]
    fn operator_empty_vec_string() {
        let parsed = Expression::parse(r#"[] | "other""#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok("other".into()), result);
    }
}

mod and {
    use expression_parser::{Expression, ExpressionValue, Variables};

    #[test]
    fn operator_false() {
        let parsed = Expression::parse(r#"true & false"#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(false.into()), result);
    }

    #[test]
    fn operator_false_2() {
        let parsed = Expression::parse(r#"false & false"#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(false.into()), result);
    }

    #[test]
    fn operator_true() {
        let parsed = Expression::parse(r#"true & true"#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(true.into()), result);
    }

    #[test]
    fn operator_number_false() {
        let parsed = Expression::parse(r#"5 & false"#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(false.into()), result);
    }

    #[test]
    fn operator_number_true() {
        let parsed = Expression::parse(r#"1 & true"#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(true.into()), result);
    }

    #[test]
    fn operator_false_string() {
        let parsed = Expression::parse(r#"false & "test""#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(false.into()), result);
    }

    #[test]
    fn operator_true_string() {
        let parsed = Expression::parse(r#"true & "test""#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok("test".into()), result);
    }

    #[test]
    fn operator_string_string() {
        let parsed = Expression::parse(r#""test" & "other""#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok("other".into()), result);
    }

    #[test]
    fn operator_string_string_2() {
        let parsed = Expression::parse(r#"false & "other""#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(false.into()), result);
    }

    #[test]
    fn operator_vec_string() {
        let parsed = Expression::parse(r#"[1] & "other""#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok("other".into()), result);
    }

    #[test]
    fn operator_empty_vec_string() {
        use ExpressionValue::List;
        let parsed = Expression::parse(r#"[] & "other""#).unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(List(vec![])), result);
    }
}

mod number {
    use expression_parser::{Expression, Variables};

    #[test]
    fn simple_addition() {
        let parsed = Expression::parse("1 + 2").unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(3.into()), result);
    }

    #[test]
    fn simple_subtraction() {
        let parsed = Expression::parse("1 - 2").unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok((-1).into()), result);
    }

    #[test]
    fn simple_multiplication() {
        let parsed = Expression::parse("1 * 2").unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(2.into()), result);
    }

    #[test]
    fn simple_division() {
        let parsed = Expression::parse("1 / 2").unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(0.5.into()), result);
    }

    #[test]
    fn simple_power() {
        let parsed = Expression::parse("2^2").unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(4.into()), result);
    }

    #[test]
    fn cosine() {
        let parsed = Expression::parse("cos(0)").unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(1.into()), result);
    }

    #[test]
    fn sine() {
        let parsed = Expression::parse("sin(0)").unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(0.into()), result);
    }

    #[test]
    fn tangent() {
        let parsed = Expression::parse("tan(0)").unwrap();
        let result = Expression::eval(parsed, &Variables::default());
        assert_eq!(Ok(0.into()), result);
    }

    #[test]
    fn sine_invalid_argments() {
        assert!(Expression::parse("sin()").is_err());
        assert!(Expression::parse("sin(1,2,3)").is_err());
    }

    #[test]
    fn combine_functions() {
        let parsed =
            Expression::parse(r#"3*((((1 + 2) / (3**2) + 5) - 2 + 5) * (2 / 4) * 4) / 2.5"#)
                .unwrap();
        let result = Expression::eval(parsed, &Variables::default())
            .unwrap()
            .as_number()
            .unwrap();
        assert_eq!(20.0, result.round());
    }
}

use expression_parser::{Expression, ExpressionMap, ExpressionValue, Variables};
use std::collections::HashMap;

#[test]
fn equal_operator_true() {
    let parsed = Expression::parse(r#""test" == "test""#).unwrap();
    let result = Expression::eval(parsed, &Variables::default());
    assert_eq!(Ok(true.into()), result);
}

#[test]
fn equal_operator_false() {
    let parsed = Expression::parse(r#""test" == "other""#).unwrap();
    let result = Expression::eval(parsed, &Variables::default());
    assert_eq!(Ok(false.into()), result);
}

#[test]
fn not_equal_operator_true() {
    let parsed = Expression::parse(r#""test" != "test""#).unwrap();
    let result = Expression::eval(parsed, &Variables::default());
    assert_eq!(Ok(false.into()), result);
}

#[test]
fn not_equal_operator_false() {
    let parsed = Expression::parse(r#""test" != "other""#).unwrap();
    let result = Expression::eval(parsed, &Variables::default());
    assert_eq!(Ok(true.into()), result);
}

#[test]
fn concat_operator() {
    // let parsed = Expression::parse(r#""test" ++ "test""#).unwrap();
    let parsed = match Expression::parse(r#""test" ++ "test""#) {
        Ok(x) => x,
        Err(e) => {
            println!("{}", e);
            panic!("error")
        }
    };
    let result = Expression::eval(parsed, &Variables::default());
    assert_eq!(Ok("testtest".into()), result);
}

#[test]
fn concat_function() {
    let parsed = Expression::parse(r#"concat("other", "test")"#).unwrap();
    let result = Expression::eval(parsed, &Variables::default());
    assert_eq!(Ok("othertest".into()), result);
}

#[test]
fn concat_function_multi() {
    let parsed = Expression::parse(r#"concat("1", 2, "3", "4")"#).unwrap();
    let result = Expression::eval(parsed, &Variables::default());
    assert_eq!(Ok("1234".into()), result);
}

#[test]
fn concat_function_variables() {
    let parsed = Expression::parse(r#"concat(test, "-", other, third, "!!")"#).unwrap();

    let mut vars = Variables::default();
    vars.insert("test", "1".into());
    vars.insert("other", "test".into());
    vars.insert("third", "3456".into());

    let result = Expression::eval(parsed, &vars);
    assert_eq!(Ok("1-test3456!!".into()), result);
}

#[test]
fn concat_function_one() {
    let parsed = Expression::parse(r#"concat("test")"#).unwrap();
    let result = Expression::eval(parsed, &Variables::default());
    assert_eq!(Ok("test".into()), result);
}

#[test]
fn concat_function_list() {
    let parsed = Expression::parse(r#"[1, 4, 5] ++ [2, 3]"#).unwrap();
    let result = Expression::eval(parsed, &Variables::default());
    assert_eq!(Ok(vec![1, 4, 5, 2, 3].into()), result);
}

#[test]
fn upper() {
    let parsed = Expression::parse(r#"upper("test")"#).unwrap();
    let result = Expression::eval(parsed, &Variables::default());
    assert_eq!(Ok("TEST".into()), result);
}

#[test]
fn trim() {
    let parsed = Expression::parse(r#"trim("..test...............", ".")"#).unwrap();
    let result = Expression::eval(parsed, &Variables::default());
    assert_eq!(Ok("test".into()), result);
}

#[test]
fn contains_true() {
    let parsed = Expression::parse(r#"contains("test", "test")"#).unwrap();
    let result = Expression::eval(parsed, &Variables::default());
    assert_eq!(Ok(true.into()), result);
}

#[test]
fn contains_false() {
    let parsed = Expression::parse(r#"contains("test", "something")"#).unwrap();
    let result = Expression::eval(parsed, &Variables::default());
    assert_eq!(Ok(false.into()), result);
}

#[test]
fn if_truthy() {
    let parsed = Expression::parse(r#"if("test", "left", "right")"#).unwrap();
    let result = Expression::eval(parsed, &Variables::default());
    assert_eq!(Ok("left".into()), result);
}

#[test]
fn if_falsy() {
    let parsed = Expression::parse(r#"if([], "left", "right")"#).unwrap();
    let result = Expression::eval(parsed, &Variables::default());
    assert_eq!(Ok("right".into()), result);
}

#[test]
fn map_parse() {
    let parsed = Expression::parse("{\"list\": [ 1, 2, 3 ], \"map\": {\"test\": 1, \"testing\": 2}, \"test\": 1, \"testing\": 2}").unwrap();

    let mut data = HashMap::new();
    data.insert(String::from("test"), 1);
    data.insert(String::from("testing"), 2);

    let mut map = ExpressionMap::from(data.clone());
    map.insert("list", vec![1, 2, 3]);
    map.insert("map", data);

    assert_eq!(parsed, Expression::Value(ExpressionValue::Map(map)));
}

#[test]
fn map_evalutate_functions() {
    let parsed =
        Expression::parse(r#"{"testing": sum(1,2,3,4) + 15, "test":  {"testing": sum(1,2,3,4)}}"#)
            .unwrap();
    let expected = Expression::parse(r#"{"testing": 25, "test":  {"testing": 10}}"#).unwrap();
    let result = Expression::eval(parsed, &Variables::default()).unwrap();
    assert_eq!(expected, Expression::Value(result));
}

#[test]
fn combine_functions() {
    let parsed = Expression::parse(
        r#"
        upper(
            concat(
                1234, 
                "_", 
                contains(
                    "test", 
                    "something"
                ) or trim("__testing", "_"),
                "_", 
                true
            )
        )
    "#,
    )
    .unwrap();
    let result = Expression::eval(parsed, &Variables::default());
    assert_eq!(Ok("1234_TESTING_TRUE".into()), result);
}

#[test]
fn parse_variable_correctly() {
    let parsed = Expression::parse("true and false").unwrap();
    let result = Expression::eval(parsed, &Variables::default());

    assert_eq!(Ok(false.into()), result);
}

#[test]
fn compile_simple() {
    let parsed = Expression::parse(r#"trim("..test...............", ".")"#).unwrap();
    let compiled = Expression::compile(parsed);
    assert_eq!("\"test\"", compiled.unwrap().to_string());

    let parsed = Expression::parse(r#"trim("..test...............", a)"#).unwrap();
    let compiled = Expression::compile(parsed).unwrap();
    assert_eq!("trim(\"..test...............\", a)", compiled.to_string());
}

#[test]
fn compile_medium() {
    let parsed = Expression::parse(
        r#"
        upper(
            concat(
                1234, 
                "_", 
                contains(
                    "test", 
                    "something"
                ) or trim("__testing", "_"),
                if("", "!", "_"), 
                all([1], true, 12, "oke")
            )
        )
    "#,
    )
    .unwrap();
    let compiled = Expression::compile(parsed).unwrap();
    assert_eq!("\"1234_TESTING_TRUE\"", compiled.to_string());

    let parsed = Expression::parse(
        r#"
        upper(
            concat(
                1234, 
                "_", 
                contains(
                    "test", 
                    "something"
                ) or trim("__testing", a),
                if("", "!", "_"), 
                all([1], true, 12, "oke")
            )
        )
    "#,
    )
    .unwrap();
    let compiled = Expression::compile(parsed).unwrap();
    assert_eq!(
        "upper(concat(1234, \"_\", (false or trim(\"__testing\", a)), \"_\", true))",
        compiled.to_string()
    );
}

#[test]
fn compile_random() {
    let parsed = Expression::parse("random()").unwrap();

    let compiled = Expression::compile(parsed).unwrap();
    assert_eq!("random(0, 1)", compiled.to_string());
}

#[test]
fn compile_calculation() {
    let parsed =
        Expression::parse("1 + 2 + 3 + 4 + sin(pi) + e ^ 2 + abc * 8 - e ^ pi / 2 - abc").unwrap();
    assert_eq!(
        "((((((((1 + 2) + 3) + 4) + sin(pi)) + (e ^ 2)) + (abc * 8)) - ((e ^ pi) / 2)) - abc)",
        parsed.to_string()
    );
    let compiled = Expression::compile(parsed).unwrap();
    assert_eq!(
        "(((17.38905609893065 + (abc * 8)) - 11.570346316389632) - abc)",
        compiled.to_string()
    );
}

#[test]
fn call_test() {
    let script = "call({x => x + 1}, 2)";
    let parsed = Expression::parse(script).unwrap();

    let result = Expression::eval(parsed, &Variables::default());

    assert_eq!(result, Ok(3.into()))
}
