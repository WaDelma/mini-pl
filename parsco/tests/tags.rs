extern crate parsco;
use parsco::{Parser, tag};

#[derive(Clone, Debug, PartialEq)]
struct A;

#[test]
fn simple_tag_parses() {
    assert_eq!(
        Some((A, "c")),
        tag("ab", A).parse("abc")
    );
}

#[test]
fn simple_tag_doesnt_parse() {
    assert_eq!(
        None,
        tag("ab", A).parse("acb")
    );
}

#[test]
fn unicode() {
    assert_eq!(
        Some((A, "a")),
        tag("áàäåö", A).parse("áàäåöa")
    );
}