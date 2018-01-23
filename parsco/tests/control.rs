extern crate parsco;
use parsco::{Parser, tag, flat_map};

#[test]
fn flat_map_none_doesnt_parse() {
    assert_eq!(
        None,
        flat_map(tag("ab"), |_| None::<u8>).parse("abc")
    )
}

#[test]
fn flat_map_some_parses() {
    assert_eq!(
        Some((0u8, "c")),
        flat_map(tag("ab"), |_| Some(0u8)).parse("abc")
    )
}