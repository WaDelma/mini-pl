extern crate parsco;
use parsco::{Parser, tag, flat_map, Err2};

#[test]
fn flat_map_none_doesnt_parse() {
    assert_eq!(
        Err((Err2::V2(()), 0..2)),
        flat_map(tag("ab"), |_| None::<u8>).parse("abc")
    )
}

#[test]
fn flat_map_some_parses() {
    assert_eq!(
        Ok((0u8, "c", 2)),
        flat_map(tag("ab"), |_| Some(0u8)).parse("abc")
    )
}