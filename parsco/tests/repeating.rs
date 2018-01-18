extern crate parsco;
use parsco::{Parser, tag, many0, many1, list0};

#[derive(Clone, Debug, PartialEq)]
struct A;

#[test]
fn many0_parses_none() {
    assert_eq!(
        Some((vec![], "ca")),
        many0(tag("a", A)).parse("ca")
    );
}

#[test]
fn many0_parses_one() {
    assert_eq!(
        Some((vec![A], "c")),
        many0(tag("a", A)).parse("ac")
    );
}

#[test]
fn many0_parses_many() {
    assert_eq!(
        Some((vec![A, A, A], "c")),
        many0(tag("a", A)).parse("aaac")
    );
}

#[test]
fn many1_doesnt_parse_none() {
    assert_eq!(
        None,
        many1(tag("a", A)).parse("ca")
    );
}

#[test]
fn many1_parses_one() {
    assert_eq!(
        Some((vec![A], "c")),
        many1(tag("a", A)).parse("ac")
    );
}

#[test]
fn many1_parses_many() {
    assert_eq!(
        Some((vec![A, A, A], "c")),
        many1(tag("a", A)).parse("aaac")
    );
}

#[test]
fn list0_parses_none() {
    assert_eq!(
        Some((vec![], "c,a")),
        list0(",", tag("a", A)).parse("c,a")
    );
}

#[test]
fn list0_parses_one() {
    assert_eq!(
        Some((vec![A], "c")),
        list0(",", tag("a", A)).parse("a,c")
    );
}

#[test]
fn list0_parses_many() {
    assert_eq!(
        Some((vec![A, A, A], "c")),
        list0(",", tag("a", A)).parse("a,a,a,c")
    );
}

// #[test]
// fn list1_doesnt_parse_none() {
//     assert_eq!(
//         None,
//         list1(",", tag("a", A)).parse("c,a")
//     );
// }

// #[test]
// fn list1_parses_one() {
//     assert_eq!(
//         Some((vec![A], "c")),
//         list1(",", tag("a", A)).parse("a,c")
//     );
// }

// #[test]
// fn list1_parses_many() {
//     assert_eq!(
//         Some((vec![A, A, A], "c")),
//         list1(",", tag("a", A)).parse("a,a,a,c")
//     );
// }