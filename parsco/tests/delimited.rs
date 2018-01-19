extern crate parsco;
use parsco::{Parser, tag, preceded, terminated};

#[derive(Clone, Debug, PartialEq)]
struct A;
#[derive(Clone, Debug, PartialEq)]
struct B;
#[derive(Clone, Debug, PartialEq)]
struct C;
#[derive(Clone, Debug, PartialEq)]
struct D;
#[derive(Clone, Debug, PartialEq)]
struct E;
#[derive(Clone, Debug, PartialEq)]
struct F;

#[test]
fn preceded_parses() {
    assert_eq!(
        Some((B, "c")),
        preceded(tag("a", A), tag("b", B)).parse("abc")
    );
}

#[test]
fn preceded_doesnt_parse() {
    assert_eq!(
        None,
        preceded(tag("a", A), tag("b", B)).parse("acb")
    );
}

#[test]
fn terminated_parses() {
    assert_eq!(
        Some((A, "c")),
        terminated(tag("a", A), tag("b", B)).parse("abc")
    );
}

#[test]
fn terminated_doesnt_parses() {
    assert_eq!(
        None,
        terminated(tag("a", A), tag("b", B)).parse("acb")
    );
}

#[test]
fn tuple_2_parses() {
    assert_eq!(
        Some(((A, B), "cdef")),
        (tag("a", A), tag("b", B)).parse("abcdef")
    );
}

#[test]
fn tuple_3_parses() {
    assert_eq!(
        Some(((A, B, C), "def")),
        (tag("a", A), tag("b", B), tag("c", C)).parse("abcdef")
    );
}

#[test]
fn tuple_4_parses() {
    assert_eq!(
        Some(((A, B, C, D), "ef")),
        (tag("a", A), tag("b", B), tag("c", C), tag("d", D)).parse("abcdef")
    );
}

#[test]
fn tuple_5_parses() {
    assert_eq!(
        Some(((A, B, C, D, E), "f")),
        (tag("a", A), tag("b", B), tag("c", C), tag("d", D), tag("e", E)).parse("abcdef")
    );
}