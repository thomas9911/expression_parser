use expression_parser::{
    Env, Environment, Expression, ExpressionFile, ExpressionMap, ExpressionValue,
};
use std::collections::HashMap;

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
    use expression_parser::{Environment, Expression, ExpressionValue};

    #[test]
    fn operator_true() {
        let parsed = Expression::parse(r#"true | false"#).unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok(true.into()), result);
    }

    #[test]
    fn operator_false() {
        let parsed = Expression::parse(r#"false | false"#).unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok(false.into()), result);
    }

    #[test]
    fn operator_false_string() {
        let parsed = Expression::parse(r#"false | "test""#).unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok("test".into()), result);
    }

    #[test]
    fn operator_true_string() {
        let parsed = Expression::parse(r#"true | "test""#).unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok(true.into()), result);
    }

    #[test]
    fn operator_false_number_string() {
        let parsed = Expression::parse(r#"0 | "test""#).unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok("test".into()), result);
    }

    #[test]
    fn operator_true_number_string() {
        let parsed = Expression::parse(r#"1 | "test""#).unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok(1.0f64.into()), result);
    }

    #[test]
    fn operator_string_string() {
        let parsed = Expression::parse(r#""test" | "other""#).unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok("test".into()), result);
    }

    #[test]
    fn operator_vec_string() {
        use Expression::Value;
        use ExpressionValue::{List, Number};

        let parsed = Expression::parse(r#"[1] | "other""#).unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok(List(vec![Value(Number(1.0))])), result);
    }

    #[test]
    fn operator_empty_vec_string() {
        let parsed = Expression::parse(r#"[] | "other""#).unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok("other".into()), result);
    }
}

mod and {
    use expression_parser::{Environment, Expression, ExpressionValue};

    #[test]
    fn operator_false() {
        let parsed = Expression::parse(r#"true & false"#).unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok(false.into()), result);
    }

    #[test]
    fn operator_false_2() {
        let parsed = Expression::parse(r#"false & false"#).unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok(false.into()), result);
    }

    #[test]
    fn operator_true() {
        let parsed = Expression::parse(r#"true & true"#).unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok(true.into()), result);
    }

    #[test]
    fn operator_number_false() {
        let parsed = Expression::parse(r#"5 & false"#).unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok(false.into()), result);
    }

    #[test]
    fn operator_number_true() {
        let parsed = Expression::parse(r#"1 & true"#).unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok(true.into()), result);
    }

    #[test]
    fn operator_false_string() {
        let parsed = Expression::parse(r#"false & "test""#).unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok(false.into()), result);
    }

    #[test]
    fn operator_true_string() {
        let parsed = Expression::parse(r#"true & "test""#).unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok("test".into()), result);
    }

    #[test]
    fn operator_string_string() {
        let parsed = Expression::parse(r#""test" & "other""#).unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok("other".into()), result);
    }

    #[test]
    fn operator_string_string_2() {
        let parsed = Expression::parse(r#"false & "other""#).unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok(false.into()), result);
    }

    #[test]
    fn operator_vec_string() {
        let parsed = Expression::parse(r#"[1] & "other""#).unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok("other".into()), result);
    }

    #[test]
    fn operator_empty_vec_string() {
        use ExpressionValue::List;
        let parsed = Expression::parse(r#"[] & "other""#).unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok(List(vec![])), result);
    }
}

mod number {
    use expression_parser::{Environment, Expression};

    #[test]
    fn simple_addition() {
        let parsed = Expression::parse("1 + 2").unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok(3.into()), result);
    }

    #[test]
    fn simple_subtraction() {
        let parsed = Expression::parse("1 - 2").unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok((-1).into()), result);
    }

    #[test]
    fn simple_multiplication() {
        let parsed = Expression::parse("1 * 2").unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok(2.into()), result);
    }

    #[test]
    fn simple_division() {
        let parsed = Expression::parse("1 / 2").unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok(0.5.into()), result);
    }

    #[test]
    fn simple_power() {
        let parsed = Expression::parse("2^2").unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok(4.into()), result);
    }

    #[test]
    fn cosine() {
        let parsed = Expression::parse("cos(0)").unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok(1.into()), result);
    }

    #[test]
    fn sine() {
        let parsed = Expression::parse("sin(0)").unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(Ok(0.into()), result);
    }

    #[test]
    fn tangent() {
        let parsed = Expression::parse("tan(0)").unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
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
        let result = Expression::eval(parsed, &mut Environment::default())
            .unwrap()
            .as_number()
            .unwrap();
        assert_eq!(20.0, result.round());
    }
}

#[test]
fn equal_operator_true() {
    let parsed = Expression::parse(r#""test" == "test""#).unwrap();
    let result = Expression::eval(parsed, &mut Environment::default());
    assert_eq!(Ok(true.into()), result);
}

#[test]
fn equal_operator_false() {
    let parsed = Expression::parse(r#""test" == "other""#).unwrap();
    let result = Expression::eval(parsed, &mut Environment::default());
    assert_eq!(Ok(false.into()), result);
}

#[test]
fn not_equal_operator_true() {
    let parsed = Expression::parse(r#""test" != "test""#).unwrap();
    let result = Expression::eval(parsed, &mut Environment::default());
    assert_eq!(Ok(false.into()), result);
}

#[test]
fn not_equal_operator_false() {
    let parsed = Expression::parse(r#""test" != "other""#).unwrap();
    let result = Expression::eval(parsed, &mut Environment::default());
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
    let result = Expression::eval(parsed, &mut Environment::default());
    assert_eq!(Ok("testtest".into()), result);
}

#[test]
fn concat_function() {
    let parsed = Expression::parse(r#"concat("other", "test")"#).unwrap();
    let result = Expression::eval(parsed, &mut Environment::default());
    assert_eq!(Ok("othertest".into()), result);
}

#[test]
fn concat_function_multi() {
    let parsed = Expression::parse(r#"concat("1", 2, "3", "4")"#).unwrap();
    let result = Expression::eval(parsed, &mut Environment::default());
    assert_eq!(Ok("1234".into()), result);
}

#[test]
fn concat_function_variables() {
    let parsed = Expression::parse(r#"concat(test, "-", other, third, "!!")"#).unwrap();

    let mut env = Environment::default();
    let vars = env.variables_mut();
    vars.insert("test", "1".into());
    vars.insert("other", "test".into());
    vars.insert("third", "3456".into());

    let result = Expression::eval(parsed, &mut env);
    assert_eq!(Ok("1-test3456!!".into()), result);
}

#[test]
fn concat_function_one() {
    let parsed = Expression::parse(r#"concat("test")"#).unwrap();
    let result = Expression::eval(parsed, &mut Environment::default());
    assert_eq!(Ok("test".into()), result);
}

#[test]
fn concat_function_list() {
    let parsed = Expression::parse(r#"[1, 4, 5] ++ [2, 3]"#).unwrap();
    let result = Expression::eval(parsed, &mut Environment::default());
    assert_eq!(Ok(vec![1, 4, 5, 2, 3].into()), result);
}

#[test]
fn upper() {
    let parsed = Expression::parse(r#"upper("test")"#).unwrap();
    let result = Expression::eval(parsed, &mut Environment::default());
    assert_eq!(Ok("TEST".into()), result);
}

#[test]
fn trim() {
    let parsed = Expression::parse(r#"trim("..test...............", ".")"#).unwrap();
    let result = Expression::eval(parsed, &mut Environment::default());
    assert_eq!(Ok("test".into()), result);
}

#[test]
fn contains_true() {
    let parsed = Expression::parse(r#"contains("test", "test")"#).unwrap();
    let result = Expression::eval(parsed, &mut Environment::default());
    assert_eq!(Ok(true.into()), result);
}

#[test]
fn contains_false() {
    let parsed = Expression::parse(r#"contains("test", "something")"#).unwrap();
    let result = Expression::eval(parsed, &mut Environment::default());
    assert_eq!(Ok(false.into()), result);
}

#[test]
fn if_truthy() {
    let parsed = Expression::parse(r#"if("test", "left", "right")"#).unwrap();
    let result = Expression::eval(parsed, &mut Environment::default());
    assert_eq!(Ok("left".into()), result);
}

#[test]
fn if_falsy() {
    let parsed = Expression::parse(r#"if([], "left", "right")"#).unwrap();
    let result = Expression::eval(parsed, &mut Environment::default());
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
    let result = Expression::eval(parsed, &mut Environment::default()).unwrap();
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
    let result = Expression::eval(parsed, &mut Environment::default());
    assert_eq!(Ok("1234_TESTING_TRUE".into()), result);
}

#[test]
fn parse_variable_correctly() {
    let parsed = Expression::parse("true and false").unwrap();
    let result = Expression::eval(parsed, &mut Environment::default());

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

mod function {
    use expression_parser::{Environment, Expression, ExpressionFile};

    #[test]
    fn simple() {
        let script = "call({x => x + 1}, 2)";
        let parsed = Expression::parse(script).unwrap();

        let result = Expression::eval(parsed, &mut Environment::default());

        assert_eq!(result, Ok(3.into()))
    }

    #[test]
    fn dot_operator_function() {
        let script = "{x, y => x + y + 1}.(2, 3)";

        let result = ExpressionFile::run(script, &mut Environment::default());

        assert_eq!(result, Ok(6.into()))
    }

    #[test]
    fn dot_operator_variable() {
        let script = "a = {x, y => x + y + 1}; a.(2, 3)";

        let result = ExpressionFile::run(script, &mut Environment::default());

        assert_eq!(result, Ok(6.into()))
    }

    #[test]
    fn dot_operator_variable_chain_not_allowed() {
        // this is invalid syntax for now.
        let script = "a = {x, y => {=> x + y + 1}}; a.(2, 3).()";

        let result = ExpressionFile::run(script, &mut Environment::default());

        assert!(result.is_err())
    }

    #[test]
    fn dot_operator_built_ins() {
        let script = r#"
        a = {x, y => 
            {
                "test": {z => x * y + z}
            }
        };

        get(a.(2, 3), "test").(4);"#;

        let result = ExpressionFile::run(script, &mut Environment::default());
        assert_eq!(result, Ok(10.into()))
    }

    #[test]
    fn invalid_operator_variable() {
        let script = "(2 + 1).(2, 3)";
        assert!(ExpressionFile::parse(script).is_err())
    }

    #[test]
    fn dot_operator_complex_arguments() {
        let script = "a = {x, y => x + y + 1}; a.((2 * 3), 5 - 9)";

        let result = ExpressionFile::run(script, &mut Environment::default());

        assert_eq!(result, Ok(3.into()))
    }

    #[test]
    fn two_args() {
        let script = "call({x, y => x + y + 1}, 2, 4)";
        let parsed = Expression::parse(script).unwrap();

        let result = Expression::eval(parsed, &mut Environment::default());

        assert_eq!(result, Ok(7.into()))
    }

    #[test]
    fn nested() {
        let script = "call({x, y => 
            call({s => 
                s * 2
            }, x + y)
        }, 2, 4)";

        let result = ExpressionFile::run(script, &mut Environment::default());

        assert_eq!(result, Ok(12.into()))
    }

    #[test]
    fn nested_script() {
        let script = r#"
        func = {x, y, z => 
            (x + y) * z
        };
        t = 5;
        r = 4;
        t = call(func, t, r, 12);
        t + 1
        "#;

        let result = ExpressionFile::run(script, &mut Environment::default());

        assert_eq!(result, Ok(109.into()))
    }

    #[test]
    fn string() {
        let script = r#"call({x => join([ x, x ], ",")}, "test")"#;
        let parsed = Expression::parse(script).unwrap();
        let result = Expression::eval(parsed, &mut Environment::default());
        assert_eq!(result, Ok("test,test".into()))
    }

    #[test]
    fn context() {
        let script = r#"
        x = 5;
        y = 4;

        func = {z =>
            # x and y are available because they are defined before 
            (x + y) * z
        };

        call(func, 12)
        "#;

        let parsed = ExpressionFile::parse(script).unwrap();
        let result = ExpressionFile::eval(parsed, &mut Environment::default());
        assert_eq!(result, Ok(108.into()))
    }

    #[test]
    fn currying() {
        let script = r#"
        factory = { x => 
            { y => x * y }
        };

        times_two = call(factory, 2);
        times_three = call(factory, 3);

        answer1 = call(times_two, 5);
        answer2 = call(times_three, 5);

        answer1 + answer2
        "#;

        let parsed = ExpressionFile::parse(script).unwrap();
        let result = ExpressionFile::eval(parsed, &mut Environment::default());
        assert_eq!(result, Ok(25.into()))
    }

    #[test]
    fn nested_currying() {
        use expression_parser::{Env, Environment};

        let script = r#"
        call(call(call({ x => 
            { => { => x } }
        }, 2)))
        "#;

        let mut env = Environment::default();
        env.variables_mut().insert("top", 1.into());

        let parsed = ExpressionFile::parse(script).unwrap();
        let result = ExpressionFile::eval(parsed, &mut env);
        assert_eq!(result, Ok(2.into()))
    }

    #[test]
    fn variable_scoping() {
        let script = r#"
        x = 123;
        
        func = { => 
            x
        };

        x = 5;

        # should be 123
        func.()
        "#;

        let mut vars = Environment::default();

        let parsed = ExpressionFile::parse(script).unwrap();
        let result = ExpressionFile::eval(parsed, &mut vars);
        assert_eq!(result, Ok(123.into()))
    }

    #[test]
    fn local_overrides_global() {
        let script = r#"
        x = 123;
        
        func = {x =>  x};

        # should be 5
        func.(5)
        "#;

        let mut vars = Environment::default();

        let parsed = ExpressionFile::parse(script).unwrap();
        let result = ExpressionFile::eval(parsed, &mut vars);
        assert_eq!(result, Ok(5.into()))
    }
}

mod closure {
    use expression_parser::{Closure, Env, Environment, Error, ExpressionFile, ExpressionValue};
    use std::sync::Arc;

    #[test]
    fn simple_test() {
        let mut vars = Environment::default();
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
        vars.variables_mut().insert("external_func", closure.into());

        let script = r#"
        call(external_func, 6, 2)
        "#;

        let parsed = ExpressionFile::parse(script).unwrap();
        let result = ExpressionFile::eval(parsed, &mut vars);
        assert_eq!(result, Ok(12.into()))
    }

    #[test]
    fn with_context_test() {
        let mut env = Environment::default();
        let closure = Closure::new(
            vec!["y".to_string()],
            Arc::new(Box::new(|vars, context| {
                fn unwrap_number(x: Option<&ExpressionValue>) -> Result<f64, Error> {
                    x.ok_or(Error::new_static("missing arguments"))?
                        .as_number()
                        .ok_or(Error::new_static("argument not a number"))
                }
                let cfg = context
                    .variables()
                    .get("MY_CONFIG")
                    .unwrap_or(&ExpressionValue::from("default"))
                    .as_string()
                    .unwrap_or(String::from("default"));
                let x = unwrap_number(context.variables().get("x"))?;
                let y = unwrap_number(vars.get(0))?;
                let result = format!("{}{}", cfg, x * y);

                Ok(result.into())
            })),
        );
        env.variables_mut().insert("external_func", closure.into());

        let script = r#"
        MY_CONFIG="testing-test-testing";
        x = 123;
        call(external_func, 2)
        "#;

        let parsed = ExpressionFile::parse(script).unwrap();
        let result = ExpressionFile::eval(parsed, &mut env);
        assert_eq!(result, Ok("testing-test-testing246".into()))
    }

    fn inner_func(
        args: Vec<ExpressionValue>,
        context: &mut Environment<'_>,
    ) -> Result<ExpressionValue, Error> {
        fn unwrap_number(x: Option<&ExpressionValue>) -> Result<f64, Error> {
            x.ok_or(Error::new_static("missing arguments"))?
                .as_number()
                .ok_or(Error::new_static("argument not a number"))
        }
        let cfg = context
            .variables()
            .get("MY_CONFIG")
            .unwrap_or(&ExpressionValue::from("default"))
            .as_string()
            .unwrap_or(String::from("default"));
        let x = unwrap_number(context.variables().get("x"))?;
        let y = unwrap_number(args.get(0))?;
        let result = format!("{}{}", cfg, x * y);

        Ok(result.into())
    }

    #[test]
    fn seperate_function_test() {
        let mut env = Environment::default();
        let closure = Closure::new(vec!["y".to_string()], Arc::new(Box::new(inner_func)));
        env.variables_mut().insert("external_func", closure.into());

        let script = r#"
        MY_CONFIG="testing-test-testing";
        x = 123;
        external_func.(2)
        "#;

        let parsed = ExpressionFile::parse(script).unwrap();
        let result = ExpressionFile::eval(parsed, &mut env);
        assert_eq!(result, Ok("testing-test-testing246".into()))
    }
}

#[cfg(feature = "serde_example")]
mod serde_tests {
    use expression_parser::{Environment, ExpressionFile};

    #[test]
    fn loading_script_from_json() {
        let script = r#"
        join_by_word = {join_word => 
            join_word = if(type(join_word) == "string", join_word, "default");
            {list => 
                r = concat(list, ["d"]);
                join(r, join_word)
            }
        };
        
        joiner = join_by_word.("<>");
        joiner.(["a", "b", "c"])
        "#;

        let parsed = ExpressionFile::parse(&script).unwrap();
        let json = serde_json::to_string(&parsed).unwrap();

        let direct_output = ExpressionFile::eval(parsed, &mut Environment::default());

        let loaded: ExpressionFile = serde_json::from_str(&json).unwrap();

        let loaded_output = ExpressionFile::eval(loaded, &mut Environment::default());
        assert_eq!(Ok("a<>b<>c<>d".into()), direct_output);
        assert_eq!(direct_output, loaded_output);
    }
}

#[test]
fn class_test() {
    // Test how classes can work.
    let script = r#"
        MyClass = {x => 
            {
                "value": x,
                "calculate": {y => x + y},
                "lazy": {=> 2*x}
            }
        };

        obj = MyClass.(5);
        one = get(obj, "value") == 5;
        two = get(obj, "calculate").(2) == 7;
        three = get(obj, "lazy").() == 10;
        one and two and three;
        "#;

    let output = ExpressionFile::run(script, &mut Environment::default());

    assert_eq!(Ok(true.into()), output);
}

mod recursion_overflow_test {
    use expression_parser::error::ErrorCodes;
    use expression_parser::{Environment, Error, ExpressionFile};

    fn script(amount: usize) -> String {
        format!(
            r#"
        createList = {{list, index => 
            newList = push(list, index);
            newIndex = index+1;
            if(newIndex == {}, newList, {{=> createList.(newList, newIndex)}})
        }};
        
        createList.([], 0);
        "#,
            amount
        )
    }

    #[test]
    fn overflow() {
        // if this does not fail just bump the number below until it overflows
        let script = &script(90);
        let output = ExpressionFile::run(script, &mut Environment::default());

        assert_eq!(output, Err(Error::new_code(ErrorCodes::STACKOVERFLOW)))
    }

    #[test]
    fn works() {
        let script = &script(5);
        let output = ExpressionFile::run(script, &mut Environment::default());

        assert_eq!(output, Ok(vec![0, 1, 2, 3, 4].into()))
    }
}

mod print {
    #[test]
    fn with_vec_writer() {
        use expression_parser::{Environment, ExpressionFile};

        let mut buffer = Vec::new();
        {
            let mut env = Environment::builder()
                .with_logger(Box::new(&mut buffer))
                .build();

            let script = r#"
        print("test");
        print({"test": 1})
        "#;

            ExpressionFile::run(script, &mut env).unwrap();
        }

        let out = String::from_utf8(buffer).unwrap();
        let expected = String::from("\"test\"\n{\"test\": 1}\n");
        assert_eq!(expected, out);
    }

    #[test]
    fn with_cursor_writer_with_bytes_buffer() {
        use expression_parser::{Environment, ExpressionFile};

        let mut data = [0 as u8; 19];
        let mut buffer = std::io::Cursor::new(&mut data[..]);
        {
            let mut env = Environment::builder()
                .with_logger(Box::new(&mut buffer))
                .build();

            let script = r#"
        print("test");
        print({"test": 1})
        "#;

            ExpressionFile::run(script, &mut env).unwrap();
        }

        let out = String::from_utf8_lossy(buffer.into_inner());
        let expected = String::from("\"test\"\n{\"test\": 1}\n");
        assert_eq!(expected, out);
    }
}

mod environment {
    use expression_parser::{
        CollectionImporter, Environment, Error, ErrorCodes, ExpressionFile, MapImporter,
        SingleCollectionImporter, VariableMap, Variables,
    };
    use std::collections::HashMap;

    #[test]
    fn collection_importer() {
        let mut import_collection = HashMap::new();
        let mut import_file = Variables::new();
        import_file.insert("data", vec![1, 2, 3, 4].into());
        import_collection.insert(String::from("test"), import_file);

        let mut import_file = Variables::new();
        import_file.insert("data", vec![9, 8, 7, 6].into());
        import_collection.insert(String::from("another"), import_file);

        let mut env = Environment::builder()
            .with_importer(Box::new(CollectionImporter::new(import_collection)))
            .build();

        let script = r#"
            import { data } from "test";

            push(data, 5)
        "#;

        let result = ExpressionFile::run(script, &mut env);
        assert_eq!(result, Ok(vec![1, 2, 3, 4, 5].into()));
    }

    #[test]
    fn single_collection_importer() {
        let mut import_file = Variables::new();
        import_file.insert("data", vec![9, 8, 7, 6].into());

        let mut env = Environment::builder()
            .with_importer(Box::new(SingleCollectionImporter::new(import_file)))
            .build();

        let script = r#"
            import { data } from "can be anything";

            push(data, 5)
        "#;

        let result = ExpressionFile::run(script, &mut env);
        assert_eq!(result, Ok(vec![9, 8, 7, 6, 5].into()));
    }

    #[test]
    fn map_importer() {
        let mut import_file = HashMap::new();
        import_file.insert(
            String::from("test"),
            String::from(
                r#"
        data = [1, 2, 3, 4];
        more_data = [5, 6, 7, 8];
        // returns list 0 to 4
        even_more_data = range(5);
        "#,
            ),
        );

        let mut env = Environment::builder()
            .with_importer(Box::new(MapImporter::new(import_file)))
            .build();

        let script = r#"
            import { data, more_data, even_more_data: foo } from "test";

            concat(data, more_data, foo)
        "#;

        let result = ExpressionFile::run(script, &mut env);
        assert_eq!(
            result,
            Ok(vec![1, 2, 3, 4, 5, 6, 7, 8, 0, 1, 2, 3, 4].into())
        );
    }

    #[test]
    fn null_importer() {
        let mut env = Environment::default();

        let script = r#"
            import { data } from "can be anything";

            push(data, 5)
        "#;

        let result = ExpressionFile::run(script, &mut env);
        assert_eq!(result, Err(Error::new_code(ErrorCodes::IMPORT_ERROR)));
    }
}
