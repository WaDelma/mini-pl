extern crate parsco;
use parsco::{Parser, ws, tag, Err2};

#[test]
fn tag_parses() {
    assert_eq!(
        Ok(("ab", "c")),
        tag("ab").parse("abc")
    );
}

#[test]
fn tag_doesnt_parse() {
    assert_eq!(
        Err(()),
        tag("ab").parse("acb")
    );
}

#[test]
fn tag_unicode() {
    assert_eq!(
        Ok(("áàäåö", "a")),
        tag("áàäåö").parse("áàäåöa")
    );
}

#[test]
fn whitespace_parse() {
    assert_eq!(
        Ok(("a", "c")),
        ws(tag("a")).parse("    
        ac")
    );
}

#[test]
fn whitespace_none() {
    assert_eq!(
        Ok(("a", "c")),
        ws(tag("a")).parse("ac")
    );
}

#[test]
fn whitespace_empty() {
    assert_eq!(
        Err(Err2::V2(())),
        ws(tag("a")).parse("c")
    );
}