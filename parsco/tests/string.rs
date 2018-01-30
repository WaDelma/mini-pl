extern crate parsco;
use parsco::{Parser, ws, tag, Err2};

#[test]
fn tag_parses() {
    assert_eq!(
        Ok(("ab", "c", 3)),
        tag("ab").parse("abc", 0)
    );
}

#[test]
fn tag_doesnt_parse() {
    assert_eq!(
        Err(((), 0..3)),
        tag("ab").parse("acb", 0)
    );
}

#[test]
fn tag_unicode() {
    assert_eq!(
        Ok(("áàäåö", "a", 6)),
        tag("áàäåö").parse("áàäåöa", 0)
    );
}

#[test]
fn whitespace_parse() {
    assert_eq!(
        Ok(("a", "c", 15)),
        ws(tag("a")).parse("    
        ac", 0)
    );
}

#[test]
fn whitespace_none() {
    assert_eq!(
        Ok(("a", "c", 1)),
        ws(tag("a")).parse("ac", 0)
    );
}

#[test]
fn whitespace_empty() {
    assert_eq!(
        Err((Err2::V2(()), 0..1)),
        ws(tag("a")).parse("c", 0)
    );
}