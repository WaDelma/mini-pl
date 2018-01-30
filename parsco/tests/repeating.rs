extern crate parsco;
use parsco::{Parser, tag, many0, many1, list0, take_while, take_until};

#[test]
fn many0_parses_none() {
    assert_eq!(
        Ok((vec![], "ca", 0)),
        many0(tag("a")).parse("ca", 0)
    );
}

#[test]
fn many0_parses_one() {
    assert_eq!(
        Ok((vec!["a"], "c", 1)),
        many0(tag("a")).parse("ac", 0)
    );
}

#[test]
fn many0_parses_many() {
    assert_eq!(
        Ok((vec!["a", "a", "a"], "c", 4)),
        many0(tag("a")).parse("aaac", 0)
    );
}

#[test]
fn many1_doesnt_parse_none() {
    assert_eq!(
        Err(((), 0..1)),
        many1(tag("a")).parse("ca", 0)
    );
}

#[test]
fn many1_parses_one() {
    assert_eq!(
        Ok((vec!["a"], "c", 1)),
        many1(tag("a")).parse("ac", 0)
    );
}

#[test]
fn many1_parses_many() {
    assert_eq!(
        Ok((vec!["a", "a", "a"], "c", 4)),
        many1(tag("a")).parse("aaac", 0)
    );
}

#[test]
fn list0_parses_none() {
    assert_eq!(
        Ok((vec![], "c,a", 0)),
        list0(",", tag("a")).parse("c,a", 0)
    );
}

#[test]
fn list0_parses_one() {
    assert_eq!(
        Ok((vec!["a"], "c", 3)),
        list0(",", tag("a")).parse("a,c", 0)
    );
}

#[test]
fn list0_parses_many() {
    assert_eq!(
        Ok((vec!["a", "a", "a"], "c", 7)),
        list0(",", tag("a")).parse("a,a,a,c", 0)
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
//         Ok((vec![A], "c")),
//         list1(",", tag("a", A)).parse("a,c")
//     );
// }

// #[test]
// fn list1_parses_many() {
//     assert_eq!(
//         Ok((vec![A, A, A], "c")),
//         list1(",", tag("a", A)).parse("a,a,a,c")
//     );
// }

#[test]
fn take_while_zero() {
    assert_eq!(
        Err(((), 0..1)),
        take_while(|c| c == 'a').parse("c", 0)
    );
}

#[test]
fn take_while_one() {
    assert_eq!(
        Ok((String::from("a"), "c", 1)),
        take_while(|c| c == 'a').parse("ac", 0)
    );
}

#[test]
fn take_while_many() {
    assert_eq!(
        Ok((String::from("aaa"), "c", 4)),
        take_while(|c| c == 'a').parse("aaac", 0)
    );
}

#[test]
fn take_while_end() {
    assert_eq!(
        Ok((String::from("a"), "", 1)),
        take_while(|c| c == 'a').parse("a", 0)
    );
}
#[test]
fn take_until_simple() {
    assert_eq!(
        Ok((("ab", "cd"), "e", 5)),
        take_until(tag("cd")).parse("abcde", 0)
    );
}
