extern crate parsco;
use parsco::{Parser, tag, preceded, terminated, Err2};

#[test]
fn preceded_parses() {
    assert_eq!(
        Ok(("b", "c", 2)),
        preceded(tag("a"), tag("b")).parse("abc")
    );
}

#[test]
fn preceded_doesnt_parse() {
    assert_eq!(
        Err((Err2::V2(()), 1..1)),
        preceded(tag("a"), tag("b")).parse("acb")
    );
}

#[test]
fn terminated_parses() {
    assert_eq!(
        Ok(("a", "c", 2)),
        terminated(tag("a"), tag("b")).parse("abc")
    );
}

#[test]
fn terminated_doesnt_parses() {
    assert_eq!(
        Err((Err2::V2(()), 1..1)),
        terminated(tag("a"), tag("b")).parse("acb")
    );
}

#[test]
fn tuple_2_parses() {
    assert_eq!(
        Ok((("a", "b"), "cdef", 2)),
        (tag("a"), tag("b")).parse("abcdef")
    );
}

#[test]
fn tuple_3_parses() {
    assert_eq!(
        Ok((("a", "b", "c"), "def", 3)),
        (tag("a"), tag("b"), tag("c")).parse("abcdef")
    );
}

#[test]
fn tuple_4_parses() {
    assert_eq!(
        Ok((("a", "b", "c", "d"), "ef", 4)),
        (tag("a"), tag("b"), tag("c"), tag("d")).parse("abcdef")
    );
}

#[test]
fn tuple_5_parses() {
    assert_eq!(
        Ok((("a", "b", "c", "d", "e"), "f", 5)),
        (tag("a"), tag("b"), tag("c"), tag("d"), tag("e")).parse("abcdef")
    );
}