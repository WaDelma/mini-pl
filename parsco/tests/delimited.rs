extern crate parsco;
use parsco::{Parser, tag, preceded, terminated, Err2};

#[test]
fn preceded_parses() {
    assert_eq!(
        Ok(("b", "c")),
        preceded(tag("a"), tag("b")).parse("abc")
    );
}

#[test]
fn preceded_doesnt_parse() {
    assert_eq!(
        Err(Err2::V2(())),
        preceded(tag("a"), tag("b")).parse("acb")
    );
}

#[test]
fn terminated_parses() {
    assert_eq!(
        Ok(("a", "c")),
        terminated(tag("a"), tag("b")).parse("abc")
    );
}

#[test]
fn terminated_doesnt_parses() {
    assert_eq!(
        Err(Err2::V2(())),
        terminated(tag("a"), tag("b")).parse("acb")
    );
}

#[test]
fn tuple_2_parses() {
    assert_eq!(
        Ok((("a", "b"), "cdef")),
        (tag("a"), tag("b")).parse("abcdef")
    );
}

#[test]
fn tuple_3_parses() {
    assert_eq!(
        Ok((("a", "b", "c"), "def")),
        (tag("a"), tag("b"), tag("c")).parse("abcdef")
    );
}

#[test]
fn tuple_4_parses() {
    assert_eq!(
        Ok((("a", "b", "c", "d"), "ef")),
        (tag("a"), tag("b"), tag("c"), tag("d")).parse("abcdef")
    );
}

#[test]
fn tuple_5_parses() {
    assert_eq!(
        Ok((("a", "b", "c", "d", "e"), "f")),
        (tag("a"), tag("b"), tag("c"), tag("d"), tag("e")).parse("abcdef")
    );
}