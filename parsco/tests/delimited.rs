extern crate parsco;
use parsco::{Parser, tag, preceded, terminated};

#[derive(Clone, Debug, PartialEq)]
struct A;
#[derive(Clone, Debug, PartialEq)]
struct B;

#[test]
fn preceded_parses() {
    assert_eq!(
        Some((B, "c")),
        preceded(tag("a", A), tag("b", B)).parse("abc")
    );
}

#[test]
fn preceded_doesnt_parse() {
    assert_eq!(
        None,
        preceded(tag("a", A), tag("b", B)).parse("acb")
    );
}

#[test]
fn terminated_parses() {
    assert_eq!(
        Some((A, "c")),
        terminated(tag("a", A), tag("b", B)).parse("abc")
    );
}

#[test]
fn terminated_doesnt_parses() {
    assert_eq!(
        None,
        terminated(tag("a", A), tag("b", B)).parse("acb")
    );
}