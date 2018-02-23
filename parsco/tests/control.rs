extern crate parsco;
use parsco::{Parser, tag, flat_map};
use parsco::common::Err2;

#[test]
fn flat_map_none_doesnt_parse() {
    assert_eq!(
        Err((Err2::V2(()), 0..2)),
        flat_map(tag("ab"), |_, _, p| Err::<((), _, _), _>(((), 0..p))).parse("abc")
    )
}

#[test]
fn flat_map_some_parses() {
    assert_eq!(
        Ok((0u8, "c", 2)),
        flat_map(tag("ab"), |_, s, p| Ok::<_, ((), _)>((0u8, s, p))).parse("abc")
    )
}