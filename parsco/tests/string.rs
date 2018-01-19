extern crate parsco;
use parsco::{Parser, ws, tag};

#[derive(Clone, Debug, PartialEq)]
struct A;

#[test]
fn tag_parses() {
    assert_eq!(
        Some((A, "c")),
        tag("ab", A).parse("abc")
    );
}

#[test]
fn tag_doesnt_parse() {
    assert_eq!(
        None,
        tag("ab", A).parse("acb")
    );
}

#[test]
fn tag_unicode() {
    assert_eq!(
        Some((A, "a")),
        tag("áàäåö", A).parse("áàäåöa")
    );
}

#[test]
fn whitespace_parse() {
    assert_eq!(
        Some((A, "c")),
        ws(tag("a", A)).parse("    
        ac")
    );
}

#[test]
fn whitespace_none() {
    assert_eq!(
        Some((A, "c")),
        ws(tag("a", A)).parse("ac")
    );
}

#[test]
fn whitespace_empty() {
    assert_eq!(
        None,
        ws(tag("a", A)).parse("c")
    );
}