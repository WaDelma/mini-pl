extern crate parsco;
use parsco::{Parser, ws, tag, Err2};

#[test]
fn tag_parses() {
    assert_eq!(
        Ok(("ab", "c", 2)),
        tag("ab").parse("abc")
    );
}

#[test]
fn tag_doesnt_parse() {
    assert_eq!(
        Err(((), 0..1)),
        tag("ab").parse("acb")
    );
}

#[test]
fn tag_unicode() {
    let t = "áàäåö";
    assert_eq!(
        Ok((t, "a", t.len())),
        tag(t).parse("áàäåöa")
    );
}

#[test]
fn whitespace_parse() {
    assert_eq!(
        Ok(("a", "c", 14)),
        ws(tag("a")).parse("    
        ac")
    );
}

#[test]
fn whitespace_none() {
    assert_eq!(
        Ok(("a", "c", 1)),
        ws(tag("a")).parse("ac")
    );
}

#[test]
fn whitespace_empty() {
    assert_eq!(
        Err((Err2::V2(()), 0..0)),
        ws(tag("a")).parse("c")
    );
}