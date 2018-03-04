//! Parsers that are used to delimit other parsers.

use {Parser, Parseable, Result};

/// Allows prefixing given parser. Used via `parsco::preceded` function.
pub struct Preceded<P1, P2> {
    parser: P1,
    precedator: P2,
}

impl<P1, P2, S> Parser<S> for Preceded<P1, P2>
    where S: Parseable,
          P1: Parser<S>,
          P2: Parser<S>,
{
    type Res = P1::Res;
    type Err = ::common::Err2<P2::Err, (P2::Res, P1::Err)>;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        (&self.precedator, &self.parser).parse(s)
            .map(|((_, r), s, p)| (r, s, p))
    }
}

/// Precedes given parser with another.
/// 
/// # Examples
/// ```rust
/// use parsco::{Parser, preceded, tag, take};
/// 
/// assert_eq!(
///     Ok(("foo", "", 4)),
///     preceded(
///         tag("#"),
///         take(3)
///     ).parse("#foo")
/// );
/// ```
pub fn preceded<P1, P2, S>(precedator: P2, parser: P1) -> Preceded<P1, P2>
    where S: Parseable,
          P1: Parser<S>,
          P2: Parser<S>,
{
    Preceded {
        parser,
        precedator
    }
}

/// Allows postfixing given parser. Used via `parsco::terminated` function.
pub struct Terminated<P1, P2> {
    parser: P1,
    terminator: P2,
}

impl<P1, P2, S> Parser<S> for Terminated<P1, P2>
    where S: Parseable,
          P1: Parser<S>,
          P2: Parser<S>,
{
    type Res = P1::Res;
    type Err = ::common::Err2<P1::Err, (P1::Res, P2::Err)>;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        (&self.parser, &self.terminator).parse(s)
            .map(|((r, _), s, p)|(r, s, p))
    }
}

/// Terminates given parser with another.
/// 
/// # Examples
/// ```rust
/// use parsco::{Parser, terminated, tag, take};
/// 
/// assert_eq!(
///     Ok(("foo", "", 4)),
///     terminated(
///         take(3),
///         tag(";")
///     ).parse("foo;")
/// );
/// ```
pub fn terminated<P1, P2, S>(parser: P1, terminator: P2) -> Terminated<P1, P2>
    where S: Parseable,
          P1: Parser<S>,
          P2: Parser<S>,
{
    Terminated {
        parser,
        terminator
    }
}

/// Allows pre- and postfixing given parser. Used via `parsco:delimited` function.
pub struct Delimited<P1, P2, P3> {
    precedator: P1,
    parser: P2,
    terminator: P3,
}

impl<P1, P2, P3, S> Parser<S> for Delimited<P1, P2, P3>
    where S: Parseable,
          P1: Parser<S>,
          P2: Parser<S>,
          P3: Parser<S>,
{
    type Res = P2::Res;
    type Err = ::common::Err3<P1::Err, (P1::Res, P2::Err), (P1::Res, P2::Res, P3::Err)>;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        (&self.precedator, &self.parser, &self.terminator).parse(s)
            .map(|((_, r, _), s, p)| (r, s, p))
    }
}

/// Delimites given parser with two other parsers.
/// 
/// # Examples
/// ```rust
/// use parsco::{Parser, delimited, tag, take};
/// 
/// assert_eq!(
///     Ok(("foo", "", 5)),
///     delimited(
///         tag("("),
///         take(3),
///         tag(")")
///     ).parse("(foo)")
/// );
/// ```
pub fn delimited<P1, P2, P3, S>(precedator: P1, parser: P2, terminator: P3) -> Delimited<P1, P2, P3>
    where S: Parseable,
          P1: Parser<S>,
          P2: Parser<S>,
          P3: Parser<S>,
{
    Delimited {
        precedator,
        parser,
        terminator,
    }
}

impl<P1, P2, S> Parser<S> for (P1, P2)
    where S: Parseable,
          P1: Parser<S>,
          P2: Parser<S>,
{
    type Res = (P1::Res, P2::Res);
    type Err = ::common::Err2<P1::Err, (P1::Res, P2::Err)>;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        self.0
            .parse(s)
            .map_err(|(e, p)| (::common::Err2::V1(e), p))
            .and_then(|(r1, s, pp)|
                match self.1.parse(s) {
                    Ok((r2, s, p)) => Ok(((r1, r2), s, pp + p)),
                    Err((e, p)) => Err((::common::Err2::V2((r1, e)), (pp + p.start)..(pp + p.end))),
                }
            )
    }
}

impl<P1, P2, P3, S> Parser<S> for (P1, P2, P3)
    where S: Parseable,
          P1: Parser<S>,
          P2: Parser<S>,
          P3: Parser<S>,
{
    type Res = (P1::Res, P2::Res, P3::Res);
    type Err = ::common::Err3<P1::Err, (P1::Res, P2::Err), (P1::Res, P2::Res, P3::Err)>;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        use common::Err2::*;
        use common::Err2 as E2;
        use common::Err3 as E3;
        let (ref p1, ref p2, ref p3) = *self;
        ((p1, p2), p3)
            .parse(s)
            .map_err(|(e, p)| (match e {
                E2::V1(V1(e1)) => E3::V1(e1),
                E2::V1(V2(e2)) => E3::V2(e2),
                E2::V2(((r1, r2), e3)) => E3::V3((r1, r2, e3)),
            }, p))
            .map(|(((r1, r2), r3), s, p)| ((r1, r2, r3), s, p))
    }
}


impl<P1, P2, P3, P4, S> Parser<S> for (P1, P2, P3, P4)
    where S: Parseable,
          P1: Parser<S>,
          P2: Parser<S>,
          P3: Parser<S>,
          P4: Parser<S>,
{
    type Res = (P1::Res, P2::Res, P3::Res, P4::Res);
    type Err = ::common::Err4<P1::Err, (P1::Res, P2::Err), (P1::Res, P2::Res, P3::Err), (P1::Res, P2::Res, P3::Res, P4::Err)>;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        use common::Err3::*;
        use common::Err2 as E2;
        use common::Err4 as E4;
        let (ref p1, ref p2, ref p3, ref p4) = *self;
        ((p1, p2, p3), p4)
            .parse(s)
            .map_err(|(e, p)| (match e {
                E2::V1(V1(e1)) => E4::V1(e1),
                E2::V1(V2(e2)) => E4::V2(e2),
                E2::V1(V3(e3)) => E4::V3(e3),
                E2::V2(((r1, r2, r3), e4)) => E4::V4((r1, r2, r3, e4)),
            }, p))
            .map(|(((r1, r2, r3), r4), s, p)| ((r1, r2, r3, r4), s, p))
    }
}

impl<P1, P2, P3, P4, P5, S> Parser<S> for (P1, P2, P3, P4, P5)
    where S: Parseable,
          P1: Parser<S>,
          P2: Parser<S>,
          P3: Parser<S>,
          P4: Parser<S>,
          P5: Parser<S>,
{
    type Res = (P1::Res, P2::Res, P3::Res, P4::Res, P5::Res);
    type Err = ::common::Err5<P1::Err, (P1::Res, P2::Err), (P1::Res, P2::Res, P3::Err), (P1::Res, P2::Res, P3::Res, P4::Err), (P1::Res, P2::Res, P3::Res, P4::Res, P5::Err)>;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        use common::Err4::*;
        use common::Err2 as E2;
        use common::Err5 as E5;
        let (ref p1, ref p2, ref p3, ref p4, ref p5) = *self;
        ((p1, p2, p3, p4), p5)
            .parse(s)
            .map_err(|(e, p)| (match e {
                E2::V1(V1(e1)) => E5::V1(e1),
                E2::V1(V2(e2)) => E5::V2(e2),
                E2::V1(V3(e3)) => E5::V3(e3),
                E2::V1(V4(e4)) => E5::V4(e4),
                E2::V2(((r1, r2, r3, r4), e5)) => E5::V5((r1, r2, r3, r4, e5)),
            }, p))
            .map(|(((r1, r2, r3, r4), r5), s, p)| ((r1, r2, r3, r4, r5), s, p))
    }
}