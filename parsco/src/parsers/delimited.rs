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