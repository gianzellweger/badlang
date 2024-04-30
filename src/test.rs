use std::str::FromStr;

use parser as pa;

const OPERATORS: &str = include_str!("../tests/operators.out");
const EQUALITY: &str = include_str!("../tests/equality.out");

#[test]
fn operators() {
    let tokens = pa::parse_file(&std::path::PathBuf::from_str("examples/operators.bl").unwrap()).unwrap();
    let mut buf = vec![];
    let stack = pa::execute_tokens(&tokens, false, &mut buf, None).unwrap();
    assert!(stack.is_empty(), "{stack:#?}");
    assert_eq!(OPERATORS, String::from_utf8(buf.to_vec()).unwrap().as_str());
}

#[test]
fn equality() {
    let tokens = pa::parse_file(&std::path::PathBuf::from_str("examples/equality.bl").unwrap()).unwrap();
    let mut buf = vec![];
    let stack = pa::execute_tokens(&tokens, false, &mut buf, None).unwrap();
    assert!(stack.is_empty(), "{stack:#?}");
    assert_eq!(EQUALITY, String::from_utf8(buf.to_vec()).unwrap().as_str());
}
