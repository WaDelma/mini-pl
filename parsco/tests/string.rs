extern crate parsco;
use parsco::{Parser, ws, tag};

#[test]
fn tag_parses() {
    assert_eq!(
        Some(("ab", "c")),
        tag("ab").parse("abc")
    );
}

#[test]
fn tag_doesnt_parse() {
    assert_eq!(
        None,
        tag("ab").parse("acb")
    );
}

#[test]
fn tag_unicode() {
    assert_eq!(
        Some(("áàäåö", "a")),
        tag("áàäåö").parse("áàäåöa")
    );
}

#[test]
fn whitespace_parse() {
    assert_eq!(
        Some(("a", "c")),
        ws(tag("a")).parse("    
        ac")
    );
}

#[test]
fn whitespace_none() {
    assert_eq!(
        Some(("a", "c")),
        ws(tag("a")).parse("ac")
    );
}

#[test]
fn whitespace_empty() {
    assert_eq!(
        None,
        ws(tag("a")).parse("c")
    );
}