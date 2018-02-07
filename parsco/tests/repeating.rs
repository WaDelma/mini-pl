extern crate parsco;
use parsco::{Parser, tag, many0, many1, list0, take_while, take_until};

#[test]
fn many0_parses_none() {
    assert_eq!(
        Ok((vec![], "ca", 0)),
        many0(tag("a")).parse("ca")
    );
}

#[test]
fn many0_parses_one() {
    assert_eq!(
        Ok((vec!["a"], "c", 1)),
        many0(tag("a")).parse("ac")
    );
}

#[test]
fn many0_parses_many() {
    assert_eq!(
        Ok((vec!["a", "a", "a"], "c", 3)),
        many0(tag("a")).parse("aaac")
    );
}

#[test]
fn many1_doesnt_parse_none() {
    assert_eq!(
        Err(((), 0..0)),
        many1(tag("a")).parse("ca")
    );
}

#[test]
fn many1_parses_one() {
    assert_eq!(
        Ok((vec!["a"], "c", 1)),
        many1(tag("a")).parse("ac")
    );
}

#[test]
fn many1_parses_many() {
    assert_eq!(
        Ok((vec!["a", "a", "a"], "c", 3)),
        many1(tag("a")).parse("aaac")
    );
}

#[test]
fn list0_parses_none() {
    assert_eq!(
        Ok((vec![], "c,a", 0)),
        list0(tag("a"), ",").parse("c,a")
    );
}

#[test]
fn list0_parses_one() {
    assert_eq!(
        Ok((vec!["a"], "c", 2)),
        list0(tag("a"), ",").parse("a,c")
    );
}

#[test]
fn list0_parses_many() {
    assert_eq!(
        Ok((vec!["a", "a", "a"], "c", 6)),
        list0(tag("a"), ",").parse("a,a,a,c")
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
        Err(((), 0..0)),
        take_while(|c| c == 'a').parse("c")
    );
}

#[test]
fn take_while_one() {
    assert_eq!(
        Ok(("a", "c", 1)),
        take_while(|c| c == 'a').parse("ac")
    );
}

#[test]
fn take_while_many() {
    assert_eq!(
        Ok(("aaa", "c", 3)),
        take_while(|c| c == 'a').parse("aaac")
    );
}

#[test]
fn take_while_end() {
    assert_eq!(
        Ok(("a", "", 1)),
        take_while(|c| c == 'a').parse("a")
    );
}
#[test]
fn take_until_simple() {
    assert_eq!(
        Ok((("ab", "cd"), "e", 4)),
        take_until(tag("cd")).parse("abcde")
    );
}
