extern crate parsco;
use parsco::{Parser, tag};

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