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
    let operators = if cfg!(windows) { OPERATORS.replace("\r\n", "\n") } else { OPERATORS.to_string() };
    assert_eq!(operators, String::from_utf8(buf.to_vec()).unwrap().as_str());
}

#[test]
fn equality() {
    let tokens = pa::parse_file(&std::path::PathBuf::from_str("examples/equality.bl").unwrap()).unwrap();
    let mut buf = vec![];
    let stack = pa::execute_tokens(&tokens, false, &mut buf, None).unwrap();
    assert!(stack.is_empty(), "{stack:#?}");
    let equality = if cfg!(windows) { EQUALITY.replace("\r\n", "\n") } else { EQUALITY.to_string() };
    assert_eq!(equality, String::from_utf8(buf.to_vec()).unwrap().as_str());
}
