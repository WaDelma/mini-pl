extern crate parsco;
use parsco::{Parser, tag, preceded, terminated, Err2};

#[test]
fn preceded_parses() {
    assert_eq!(
        Ok(("b", "c", 2)),
        preceded(tag("a"), tag("b")).parse("abc", 0)
    );
}

#[test]
fn preceded_doesnt_parse() {
    assert_eq!(
        Err((Err2::V2(()), 0..3)),
        preceded(tag("a"), tag("b")).parse("acb", 0)
    );
}

#[test]
fn terminated_parses() {
    assert_eq!(
        Ok(("a", "c", 2)),
        terminated(tag("a"), tag("b")).parse("abc", 0)
    );
}

#[test]
fn terminated_doesnt_parses() {
    assert_eq!(
        Err((Err2::V2(()), 0..3)),
        terminated(tag("a"), tag("b")).parse("acb", 0)
    );
}

#[test]
fn tuple_2_parses() {
    assert_eq!(
        Ok((("a", "b"), "cdef", 3)),
        (tag("a"), tag("b")).parse("abcdef", 0)
    );
}

#[test]
fn tuple_3_parses() {
    assert_eq!(
        Ok((("a", "b", "c"), "def", 4)),
        (tag("a"), tag("b"), tag("c")).parse("abcdef", 0)
    );
}

#[test]
fn tuple_4_parses() {
    assert_eq!(
        Ok((("a", "b", "c", "d"), "ef", 5)),
        (tag("a"), tag("b"), tag("c"), tag("d")).parse("abcdef", 0)
    );
}

#[test]
fn tuple_5_parses() {
    assert_eq!(
        Ok((("a", "b", "c", "d", "e"), "f", 6)),
        (tag("a"), tag("b"), tag("c"), tag("d"), tag("e")).parse("abcdef", 0)
    );
}