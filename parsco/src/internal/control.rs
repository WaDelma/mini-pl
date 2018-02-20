use std::marker::PhantomData;
use std::ops::BitOr;

use {Parser, Parseable, Result};
use common::Err2;

/// Allows to try multiple alternative parsers in a sequence. Used via `parsco::alt` function.
/// 
/// Chaining multiple parsers using `BitOr` implementation creates type based list that ends with `Empty`.
/// When parsing it first tries the parser in the head of the list
/// and if it fails it recursively calls parsers in the rest of the list.
/// If none of the parsers succeed, the recursion ends with `Empty` which just fails.
pub struct Alt<P, R, S> {
    parser: P,
    rest: R,
    _marker: PhantomData<S>,
}

impl<P, R, N, S, T> BitOr<N> for Alt<P, R, S>
    where S: Parseable,
          P: Parser<S, Res=T>,
          R: Parser<S, Res=T>,
          N: Parser<S, Res=T>,
{
    type Output = Alt<Self, N, S>;
    fn bitor(self, lhs: N) -> Self::Output {
        Alt {
            parser: self,
            rest: lhs,
            _marker: PhantomData,
        }
    }
}

impl<P, R, T, S> Parser<S> for Alt<P, R, S>
    where S: Parseable,
          P: Parser<S, Res=T>,
          R: Parser<S, Res=T>,
{
    type Res = T;
    type Err = R::Err;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        self.parser.parse(s)
            .or_else(|_|
                self.rest.parse(s)
            )
    }
}

// TODO: Remove P when `!` becomes stable.
/// End of type type based list of `Alt`s. Used via `parsco::alt` function.
pub struct Empty<P, S>(PhantomData<(P, S)>);

impl<P, N, S, T> BitOr<N> for Empty<P, S>
    where S: Parseable,
          N: Parser<S, Res=T>,
{
    type Output = Alt<Self, N, S>;
    fn bitor(self, lhs: N) -> Self::Output {
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

/// Allows turning parsers into optional ones. Used via `parsco::opt` function.
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

/// Allows mapping parser return type to different one. Used via `parsco::map` function.
pub struct Map<P, F> {
    parser: P,
    map: F
}

impl<P, S, F, T> Parser<S> for Map<P, F>
    where P: Parser<S>,
          S: Parseable,
          F: Fn(P::Res, S, usize) -> T       
{
    type Res = T;
    type Err = P::Err;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        self.parser.parse(s)
            .map(|(res, s, p)| ((self.map)(res, s, p), s, p))
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
///     map(tag("foo"), |_, _, _| Foo).parse("foo")
/// );
/// ```
pub fn map<P, F, S, T>(parser: P, map: F) -> Map<P, F>
    where P: Parser<S>,
          S: Parseable,
          F: Fn(P::Res, S, usize) -> T    
{
    Map {
        parser,
        map
    }
}

/// Allows flat mapping parser return type to different one. Used via `parsco::flat_map` function.
pub struct FlatMap<P, F> {
    parser: P,
    map: F
}

impl<P, S, F, E, T> Parser<S> for FlatMap<P, F>
    where P: Parser<S>,
          S: Parseable,
          F: Fn(P::Res, S, usize) -> Result<S, T, E>
{
    type Res = T;
    type Err = Err2<P::Err, E>;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        self.parser.parse(s)
            .map_err(|(e, p)| (Err2::V1(e), p))
            .and_then(|(res, s, pp)|
                (self.map)(res, s, pp)
                    .map_err(|(e, r)| (Err2::V2(e), r))
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
///     flat_map(tag("foo"), |_, s, r| Ok::<_, ((), _)>((Foo, s, r))).parse("foo")
/// );
/// ```
/// ```rust
/// # use parsco::{Parser, flat_map, tag};
/// # use parsco::common::Err2;
/// #[derive(Debug, PartialEq)]
/// struct Foo;
/// assert_eq!(
///     Err((Err2::V2(()), 0..3)),
///     flat_map(tag("foo"), |_, _, r| Err::<((), _, _), _>(((), 0..r))).parse("foo")
/// );
/// ```
pub fn flat_map<P, F, S, T, E>(parser: P, map: F) -> FlatMap<P, F>
    where P: Parser<S>,
          S: Parseable,
          F: Fn(P::Res, S, usize) -> Result<S, T, E>
{
    FlatMap {
        parser,
        map
    }
}

/// Allows replacing parsers return value with different one. Used via `parsco::eat` function.
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