use std::marker::PhantomData;
use std::ops::BitOr;

use {Parser, Parseable, Result, Err2};

pub struct Alt<P1, P2, S> {
    parser: P1,
    rest: P2,
    _marker: PhantomData<S>,
}

impl<P1, P2, P3, S, T> BitOr<P3> for Alt<P1, P2, S>
    where S: Parseable,
          P1: Parser<S, Res=T>,
          P2: Parser<S, Res=T>,
          P3: Parser<S, Res=T>,
{
    type Output = Alt<Self, P3, S>;
    fn bitor(self, lhs: P3) -> Self::Output {
        Alt {
            parser: self,
            rest: lhs,
            _marker: PhantomData,
        }
    }
}

impl<P1, P2, T, S> Parser<S> for Alt<P1, P2, S>
    where S: Parseable,
          P1: Parser<S, Res=T>,
          P2: Parser<S, Res=T>,
{
    type Res = T;
    type Err = P2::Err;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        self.parser.parse(s)
            .or_else(|_|
                self.rest.parse(s)
            )
    }
}

pub struct Empty<P, S>(PhantomData<(P, S)>);

impl<P1, P2, S, T> BitOr<P2> for Empty<P1, S>
    where S: Parseable,
          P2: Parser<S, Res=T>,
{
    type Output = Alt<Self, P2, S>;
    fn bitor(self, lhs: P2) -> Self::Output {
        Alt {
            parser: self,
            rest: lhs,
            _marker: PhantomData,
        }
    }
}

impl<T, S> Parser<S> for Empty<T, S>
    where S: Parseable,
{
    type Res = T;
    type Err = ();
    fn parse(&self, _: S) -> Result<S, Self::Res, Self::Err> {
        Err(((), 0..0))
    }
}

/// Allows to try multiple alternative parsers in a sequence.
/// 
/// # Examples
/// ```rust
/// # use parsco::{Parser, alt, tag};
/// let parser = alt()
///     | tag("foo")
///     | tag("bar");
/// assert_eq!(
///     Ok(("foo", "", 3)),
///     parser.parse("foo")
/// );
/// assert_eq!(
///     Ok(("bar", "", 3)),
///     parser.parse("bar")
/// );
/// ```
pub fn alt<P, S>() -> Empty<P, S> {
    Empty(PhantomData)
}

pub struct Opt<P> {
    parser: P,
}

impl<P, S> Parser<S> for Opt<P>
    where S: Parseable,
          P: Parser<S>,
{
    type Res = Option<P::Res>;
    type Err = ();
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        self.parser.parse(s)
            .map(|(r, s, p)| (Some(r), s, p))
            .or_else(|_| Ok((None, s, 0)))
    }
}

/// Makes input parser optional ignoring it's failure.
/// 
/// # Examples
/// ```rust
/// # use parsco::{Parser, opt, tag};
/// let parser = opt(tag("foo"));
/// assert_eq!(
///     Ok((Some("foo"), "", 3)),
///     parser.parse("foo")
/// );
/// assert_eq!(
///     Ok((None, "bar", 0)),
///     parser.parse("bar")
/// );
/// ```
pub fn opt<P, S>(parser: P) -> Opt<P>
    where S: Parseable,
          P: Parser<S>,
{
    Opt {
        parser
    }
}

pub struct Map<P, F> {
    parser: P,
    map: F
}

impl<P, S, F, T> Parser<S> for Map<P, F>
    where P: Parser<S>,
          S: Parseable,
          F: Fn(P::Res) -> T       
{
    type Res = T;
    type Err = P::Err;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        self.parser.parse(s)
            .map(|(res, s, p)| ((self.map)(res), s, p))
    }
}

/// Maps input parsers output using closure or function.
/// 
/// # Examples
/// ```rust
/// # use parsco::{Parser, map, tag};
/// #[derive(Debug, PartialEq)]
/// struct Foo;
/// assert_eq!(
///     Ok((Foo, "", 3)),
///     map(tag("foo"), |_| Foo).parse("foo")
/// );
/// ```
pub fn map<P, F, S, T>(parser: P, map: F) -> Map<P, F>
    where P: Parser<S>,
          S: Parseable,
          F: Fn(P::Res) -> T
{
    Map {
        parser,
        map
    }
}

pub struct FlatMap<P, F> {
    parser: P,
    map: F
}

impl<P, S, F, T> Parser<S> for FlatMap<P, F>
    where P: Parser<S>,
          S: Parseable,
          F: Fn(P::Res) -> Option<T>
{
    type Res = T;
    type Err = Err2<P::Err, ()>;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        self.parser.parse(s)
            .map_err(|(e, p)| (Err2::V1(e), p))
            .and_then(|(res, s, pp)|
                (self.map)(res)
                    .map(|res| (res, s, pp))
                    .ok_or((Err2::V2(()), 0..pp))
            )
    }
}

/// Flat maps input parsers output using closure or function.
/// 
/// # Examples
/// ```rust
/// # use parsco::{Parser, flat_map, tag};
/// #[derive(Debug, PartialEq)]
/// struct Foo;
/// assert_eq!(
///     Ok((Foo, "", 3)),
///     flat_map(tag("foo"), |_| Some(Foo)).parse("foo")
/// );
/// ```
/// ```rust
/// # use parsco::{Parser, Err2, flat_map, tag};
/// #[derive(Debug, PartialEq)]
/// struct Foo;
/// assert_eq!(
///     Err((Err2::V2(()), 0..3)),
///     flat_map(tag("foo"), |_| None::<u8>).parse("foo")
/// );
/// ```
pub fn flat_map<P, F, S, T>(parser: P, map: F) -> FlatMap<P, F>
    where P: Parser<S>,
          S: Parseable,
          F: Fn(P::Res) -> Option<T>
{
    FlatMap {
        parser,
        map
    }
}

pub struct Eat<P, T> {
    parser: P,
    substitute: T
}

impl<P, S, T> Parser<S> for Eat<P, T>
    where P: Parser<S>,
          S: Parseable,
          T: Clone,
{
    type Res = T;   
    type Err = P::Err;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        self.parser.parse(s)
            .map(|(_, s, pp)| (self.substitute.clone(), s, pp))
    }
}

/// Eats parsers return value and substitutes it with predefined value.
/// 
/// # Examples
/// ```rust
/// # use parsco::{Parser, eat, tag};
/// assert_eq!(
///     Ok(("+", "", 3)),
///     eat(tag("add"), "+").parse("add")
/// );
/// ```
pub fn eat<P, T, S>(parser: P, substitute: T) -> Eat<P, T>
    where P: Parser<S>,
          S: Parseable,
          T: Clone,
{
    Eat {
        parser,
        substitute
    }
}