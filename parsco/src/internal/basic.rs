use {Parser, Parseable, Result, take};

use std::fmt;

pub struct Fun<P>(P);

impl<F, S, T, E> Parser<S> for Fun<F>
    where S: Parseable,
          F: for<'a> Fn(S) -> Result<S, T, E>,
{
    type Res = T;
    type Err = E;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        self.0(s)
    }
}

/// Makes function or closure with right signature to parser.
/// 
/// This is a workaround as direct `Parser` implementation conflicts with `Parser` implementation for reference.
/// 
/// # Examples
/// ```rust
/// # use parsco::{Parser, Result, alt, fun};
/// fn my_parser(s: &str) -> Result<&str, usize, ()> {
///     if s.is_empty() {
///         Err(((), 0..0))
///     } else {
///         Ok((1, s, 0))
///     }
/// }
/// 
/// (alt()
///     | fun(my_parser)
///     | fun(|s: &str| if s.is_empty() {
///         Err(((), 0..0))
///     } else {
///         Ok((1, s, 0))
///     })
/// ).parse("lol");
/// ```
pub fn fun<F, S, T, E>(f: F) -> Fun<F>
    where S: Parseable,
          F: for<'a> Fn(S) -> Result<S, T, E>,
{
    Fun(f)
}

pub struct Tag<S> {
    tag: S,
}

impl<S: Parseable> Parser<S> for Tag<S> {
    type Res = S;
    type Err = ();
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        if s.starts_with(&self.tag) {
            Ok((self.tag, s.split_at(self.tag.len()).expect("There should be tag at the start").1, self.tag.len()))
        } else {
            let mut prefix = self.tag;
            while !s.starts_with(&prefix) {
                prefix = prefix.split_at(prefix.len() - 1).expect("Everything but last").0;
            }
            Err(((), 0..prefix.len()))
        }
    }
}

/// Recognises tag at the start of the input.
/// 
/// # Examples
/// ```rust
/// # use parsco::{Parser, tag};
/// assert_eq!(
///     Ok(("return", " foo;", 6)),
///     tag("return").parse("return foo;")
/// );
/// ```
pub fn tag<S>(tag: S) -> Tag<S>
    where S: Parseable
{
    Tag {
        tag,
    }
}

pub struct One<S> {
    symbol: S,
}

impl<S> Parser<S> for One<S::Symbol>
    where S: Parseable,
          S::Symbol: PartialEq + Clone
{
    type Res = S::Symbol;
    type Err = ();
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        s.first()
            .ok_or(((), 0..1))
            .and_then(|f| if f == self.symbol {
                Ok((self.symbol.clone(), s.split_at(1).expect("There is first").1, 1))
            } else {
                Err(((), 0..1))
            })
    }
}

pub fn one<S>(symbol: S) -> One<S>
    where S: PartialEq + Clone
{
    One {
        symbol
    }
}

pub struct Fst;

impl<S: Parseable> Parser<S> for Fst {
    type Res = S::Symbol;
    type Err = ();
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        take(1).parse(s)
            .and_then(|(t, s, pp)|
                t.first()
                    .map(|t| (t, s, pp))
                    .ok_or(((), 0..pp))
            )
    }
}

pub fn fst() -> Fst {
    Fst
}

pub struct Dbg<P>(P);

impl<P: Parser<S>, S: Parseable + fmt::Debug> Parser<S> for Dbg<P> {
    type Res = P::Res;
    type Err = P::Err;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        eprintln!("DBG: {:#?}", s);
        self.0.parse(s)
    }
}

/// Debug prints the state of the input.
pub fn dbg<P: Parser<S>, S: Parseable>(parser: P) -> Dbg<P> {
    Dbg(parser)
}