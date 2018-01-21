extern crate parsco;
use parsco::{Parser, tag, many0, many1, list0, take_while, take_until};

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

#[test]
fn take_while_zero() {
    assert_eq!(
        None,
        take_while(|c| c == 'a').parse("c")
    );
}

#[test]
fn take_while_one() {
    assert_eq!(
        Some((String::from("a"), "c")),
        take_while(|c| c == 'a').parse("ac")
    );
}

#[test]
fn take_while_many() {
    assert_eq!(
        Some((String::from("aaa"), "c")),
        take_while(|c| c == 'a').parse("aaac")
    );
}

#[test]
fn take_while_end() {
    assert_eq!(
        Some((String::from("a"), "")),
        take_while(|c| c == 'a').parse("a")
    );
}
#[test]
fn take_until_simple() {
    #[derive(Clone, Debug, PartialEq)]
    struct DC;
    assert_eq!(
        Some((("ab", DC), "e")),
        take_until(tag("dc", DC)).parse("abcde")
    );
}
