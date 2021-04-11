use expression_parser::{Expression, ExpressionMap, ExpressionValue};
use std::sync::Arc;

// parse from https://json.org/example.html

#[test]
fn parse_json_one() {
    let json = r#"
    {
        "glossary": {
            "title": "example glossary",
            "GlossDiv": {
                "title": "S",
                "GlossList": {
                    "GlossEntry": {
                        "ID": "SGML",
                        "SortAs": "SGML",
                        "GlossTerm": "Standard Generalized Markup Language",
                        "Acronym": "SGML",
                        "Abbrev": "ISO 8879:1986",
                        "GlossDef": {
                            "para": "A meta-markup language, used to create markup languages such as DocBook.",
                            "GlossSeeAlso": ["GML", "XML"]
                        },
                        "GlossSee": "markup"
                    }
                }
            }
        }
    }
    "#;

    let mut def = ExpressionMap::new();
    def.insert(
        "para",
        "A meta-markup language, used to create markup languages such as DocBook.",
    );
    def.insert("GlossSeeAlso", vec!["GML", "XML"]);

    let mut entry = ExpressionMap::new();
    entry.insert("ID", "SGML");
    entry.insert("SortAs", "SGML");
    entry.insert("GlossTerm", "Standard Generalized Markup Language");
    entry.insert("Acronym", "SGML");
    entry.insert("Abbrev", "ISO 8879:1986");
    entry.insert("GlossSee", "markup");
    entry.insert("GlossDef", def);

    let mut list = ExpressionMap::new();
    list.insert("GlossEntry", entry);

    let mut div = ExpressionMap::new();
    div.insert("title", "S");
    div.insert("GlossList", list);

    let mut glossary = ExpressionMap::new();
    glossary.insert("title", "example glossary");
    glossary.insert("GlossDiv", div);

    let mut main = ExpressionMap::new();
    main.insert("glossary", glossary);

    let expected = Expression::Value(Arc::new(main.into()));
    let parsed = Expression::parse(json).unwrap();

    assert_eq!(parsed, expected);
}

#[test]
fn parse_json_two() {
    let json = r#"
    {"menu": {
        "id": "file",
        "value": "File",
        "popup": {
          "menuitem": [
            {"value": "New", "onclick": "CreateNewDoc()"},
            {"value": "Open", "onclick": "OpenDoc()"},
            {"value": "Close", "onclick": "CloseDoc()"}
          ]
        }
      }}
    "#;

    let mut item1 = ExpressionMap::new();
    item1.insert("value", "New");
    item1.insert("onclick", "CreateNewDoc()");

    let mut item2 = ExpressionMap::new();
    item2.insert("value", "Open");
    item2.insert("onclick", "OpenDoc()");

    let mut item3 = ExpressionMap::new();
    item3.insert("value", "Close");
    item3.insert("onclick", "CloseDoc()");

    let mut popup = ExpressionMap::new();
    popup.insert("menuitem", vec![item1, item2, item3]);

    let mut menu = ExpressionMap::new();
    menu.insert("id", "file");
    menu.insert("value", "File");
    menu.insert("popup", popup);

    let mut main = ExpressionMap::new();
    main.insert("menu", menu);

    let expected = Expression::Value(Arc::new(main.into()));
    let parsed = Expression::parse(json).unwrap();

    assert_eq!(parsed, expected);
}

#[test]
fn parse_json_three() {
    let json = r#"
    {"widget": {
        "debug": "on",
        "window": {
            "title": "Sample Konfabulator Widget",
            "name": "main_window",
            "width": 500,
            "height": 500
        },
        "image": { 
            "src": "Images/Sun.png",
            "name": "sun1",
            "hOffset": 250,
            "vOffset": 250,
            "alignment": "center"
        },
        "text": {
            "data": "Click Here",
            "size": 36,
            "style": "bold",
            "name": "text1",
            "hOffset": 250,
            "vOffset": 100,
            "alignment": "center",
            "onMouseUp": "sun1.opacity = (sun1.opacity / 100) * 90;"
        }
    }}   
    "#;

    let mut window = ExpressionMap::new();
    window.insert("title", "Sample Konfabulator Widget");
    window.insert("name", "main_window");
    window.insert("width", 500);
    window.insert("height", 500);

    let mut image = ExpressionMap::new();
    image.insert("src", "Images/Sun.png");
    image.insert("name", "sun1");
    image.insert("hOffset", 250);
    image.insert("vOffset", 250);
    image.insert("alignment", "center");

    let mut text = ExpressionMap::new();
    text.insert("data", "Click Here");
    text.insert("size", 36);
    text.insert("style", "bold");
    text.insert("name", "text1");
    text.insert("hOffset", 250);
    text.insert("vOffset", 100);
    text.insert("alignment", "center");
    text.insert("onMouseUp", "sun1.opacity = (sun1.opacity / 100) * 90;");

    let mut widget = ExpressionMap::new();
    widget.insert("window", window);
    widget.insert("image", image);
    widget.insert("text", text);
    widget.insert("debug", "on");

    let mut main = ExpressionMap::new();
    main.insert("widget", widget);

    let expected = Expression::Value(Arc::new(main.into()));
    let parsed = Expression::parse(json).unwrap();

    assert_eq!(parsed, expected);
}

#[test]
fn parse_json_five() {
    let json = r#"
    {"menu": {
        "header": "SVG Viewer",
        "items": [
            {"id": "Open"},
            {"id": "OpenNew", "label": "Open New"},
            null,
            {"id": "ZoomIn", "label": "Zoom In"},
            {"id": "ZoomOut", "label": "Zoom Out"},
            {"id": "OriginalView", "label": "Original View"},
            null,
            {"id": "Quality"},
            {"id": "Pause"},
            {"id": "Mute"},
            null
        ]
    }}
    "#;
    let null_var = Expression::Var("null".to_string());

    let mut item1 = ExpressionMap::new();
    item1.insert("id", "Open");

    let mut item2 = ExpressionMap::new();
    item2.insert("id", "OpenNew");
    item2.insert("label", "Open New");

    let item3 = null_var.clone();

    let mut item4 = ExpressionMap::new();
    item4.insert("id", "ZoomIn");
    item4.insert("label", "Zoom In");

    let mut item5 = ExpressionMap::new();
    item5.insert("id", "ZoomOut");
    item5.insert("label", "Zoom Out");

    let mut item6 = ExpressionMap::new();
    item6.insert("id", "OriginalView");
    item6.insert("label", "Original View");

    let item7 = null_var.clone();

    let mut item8 = ExpressionMap::new();
    item8.insert("id", "Quality");

    let mut item9 = ExpressionMap::new();
    item9.insert("id", "Pause");

    let mut item10 = ExpressionMap::new();
    item10.insert("id", "Mute");

    let item11 = null_var.clone();

    let mut menu = ExpressionMap::new();
    menu.insert("header", "SVG Viewer");
    menu.0.insert(
        String::from("items"),
        Arc::new(Expression::Value(
            ExpressionValue::List(vec![
                Expression::from(item1).into(),
                Expression::from(item2).into(),
                item3.into(),
                Expression::from(item4).into(),
                Expression::from(item5).into(),
                Expression::from(item6).into(),
                item7.into(),
                Expression::from(item8).into(),
                Expression::from(item9).into(),
                Expression::from(item10).into(),
                item11.into(),
            ])
            .into(),
        )),
    );

    let mut main = ExpressionMap::new();
    main.insert("menu", menu);

    let expected = Expression::Value(Arc::new(main.into()));
    let parsed = Expression::parse(json).unwrap();

    assert_eq!(parsed, expected);
}
