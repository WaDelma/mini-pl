extern crate parsco;
use parsco::{Parser, tag, preceded, terminated};

#[test]
fn preceded_parses() {
    assert_eq!(
        Some(("b", "c")),
        preceded(tag("a"), tag("b")).parse("abc")
    );
}

#[test]
fn preceded_doesnt_parse() {
    assert_eq!(
        None,
        preceded(tag("a"), tag("b")).parse("acb")
    );
}

#[test]
fn terminated_parses() {
    assert_eq!(
        Some(("a", "c")),
        terminated(tag("a"), tag("b")).parse("abc")
    );
}

#[test]
fn terminated_doesnt_parses() {
    assert_eq!(
        None,
        terminated(tag("a"), tag("b")).parse("acb")
    );
}

#[test]
fn tuple_2_parses() {
    assert_eq!(
        Some((("a", "b"), "cdef")),
        (tag("a"), tag("b")).parse("abcdef")
    );
}

#[test]
fn tuple_3_parses() {
    assert_eq!(
        Some((("a", "b", "c"), "def")),
        (tag("a"), tag("b"), tag("c")).parse("abcdef")
    );
}

#[test]
fn tuple_4_parses() {
    assert_eq!(
        Some((("a", "b", "c", "d"), "ef")),
        (tag("a"), tag("b"), tag("c"), tag("d")).parse("abcdef")
    );
}

#[test]
fn tuple_5_parses() {
    assert_eq!(
        Some((("a", "b", "c", "d", "e"), "f")),
        (tag("a"), tag("b"), tag("c"), tag("d"), tag("e")).parse("abcdef")
    );
}