//! # Parsco
//! 
//! A parser combinator library without use of macros.
//! 
//! Parser combinator is way of constructing parser from smaller parsers.
//! 
//! The library introduces trait `Parser` and bunch of small parsers implementing it
//! that can be used as building blocks of bigger parsers.
//! 
//! The `Parser` trait is generic on the input, the output and the error value that
//! the parser can return. To be able to be parsed `Parseable` trait has to be implement
//! for the input type.
//! 
//! # Examples
//! ```rust
//! extern crate parsco;
//! use parsco::{Parser, take_while, delimited, tag, list0, fun, map};
//! use std::char;
//! 
//! #[derive(Debug, PartialEq)]
//! struct Res {
//!     name: String,
//!     value: Vec<Res>,
//! }
//! 
//! fn parse_res(s: &str) -> parsco::Result<&str, Res, ()> {
//!     map((
//!         take_while(char::is_alphabetic),
//!         delimited(
//!             tag("{"),
//!             list0(
//!                 fun(parse_res),
//!                 ","
//!             ),
//!             tag("}")
//!         )
//!     ),
//!     |(name, value)| Res {
//!         name,
//!         value
//!     }).parse(s)
//!         .map_err(|(_, r)| ((), r))
//! }
//!     
//! fn main() {
//!     let text = "foo{bar{},baz{},}";
//!     assert_eq!(
//!         Ok((Res {
//!             name: "foo".into(),
//!             value: vec![
//!                 Res {
//!                     name: "bar".into(),
//!                     value: vec![],
//!                 },
//!                 Res {
//!                     name: "baz".into(),
//!                     value: vec![],
//!                 },
//!             ]
//!         }, "", text.len())),
//!         parse_res(text)
//!     );
//! }
//! ```
use std::fmt;
use std::ops::Range;

pub use delimited::*;
pub use repeating::*;
pub use control::*;

mod control;
mod delimited;
mod repeating;

type Place = usize;

/// Type alias for the result that parsers return.
pub type Result<S, T, E> = ::std::result::Result<(T, S, Place), (E, Range<Place>)>;

/// Never type. This is workaround while ! is not stable. It's used to indicate imposibility of certain errors.
pub enum Void {}

pub trait FromErr<E> {
    fn from(e: E) -> Self;
}

/// Type that can be parsed by `Parser`.
/// 
/// It's `Copy`, because it's supposed to be implemented on shared references which are always `Copy`.
pub trait Parseable: Copy {
    /// Symbol contained inside the parseable type.
    type Symbol;
    /// Returns how many symbols there are inside.
    fn len(self) -> usize;
    /// Returns true if there are no symbols inside.
    fn is_empty(self) -> bool {
        self.len() == 0
    }
    /// Returns the first symbol inside or `None` if there are no symbols.
    fn first(self) -> Option<Self::Symbol>;
    /// Checks if contained symbols have pattern as prefix.
    fn starts_with(self, pat: &Self) -> bool;
    /// Splits the symbols at the index and returns both left and right side.
    /// If empty this method returns None.
    fn split_at(self, i: usize) -> Option<(Self, Self)>;
}

impl<'a> Parseable for &'a str {
    type Symbol = char;

    fn len(self) -> usize {
        self.len()
    }

    fn first(self) -> Option<Self::Symbol> {
        self.chars().next()
    }

    fn starts_with(self, pat: &Self) -> bool {
        self.starts_with(pat)
    }

    fn split_at(self, i: usize) -> Option<(Self, Self)> {
        if i <= self.len() {
            Some(self.split_at(i))
        } else {
            None
        }
    }
}

impl<'a, T: PartialEq + Clone> Parseable for &'a [T] {
    type Symbol = T;

    fn len(self) -> usize {
        self.len()
    }

    fn first(self) -> Option<Self::Symbol> {
       <[T]>::first(self).cloned()
    }

    fn starts_with(self, pat: &Self) -> bool {
        self.starts_with(pat)
    }

    fn split_at(self, i: usize) -> Option<(Self, Self)> {
        if i <= self.len() {
            Some(self.split_at(i))
        } else {
            None
        }
    }
}

/// Trait implemented on all parsers.
/// 
/// It's generic over the input `Parseable` type, result and error it returns.
/// 
/// # Examples
/// ```rust
/// extern crate parsco;
/// 
/// use parsco::{Parser, Parseable, Result};
/// 
/// struct TakeDigit;
/// 
/// impl<'a> Parser<&'a str> for TakeDigit {
///     type Res = u8;
///     type Err = ();
///     fn parse<'b>(&self, s: &'b str) -> Result<&'b str, Self::Res, Self::Err> {
///         if let Some(i) = s.first().and_then(|c| c.to_digit(10)) {
///             Ok((i as u8, s.split_at(1).1, 1))
///         } else {
///             Err(((), 0..0))
///         }
///     }
/// }
/// 
/// fn main() {
///     assert_eq!(
///         Ok((1, "2", 1)),
///         TakeDigit.parse("12")
///     );
///     assert_eq!(
///         Err(((), 0..0)),
///         TakeDigit.parse("a2")
///     );
/// }
/// ```
pub trait Parser<S: Parseable> {
    /// The result that the parser parses the input to.
    type Res;
    /// The error that parser returns if it fails.
    type Err;
    /// Method used to parse input to output.
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err>;
}

impl<'b, T: Parser<S>, S: Parseable> Parser<S> for &'b T {
    type Res = T::Res;
    type Err = T::Err;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        (*self).parse(s)
    }
}

impl<S: Parseable> Parser<S> for () {
    type Res = ();
    type Err = Void;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        Ok(((), s, 0))
    }
}

pub struct Wrapper<P>(P);

impl<F, S, T, E> Parser<S> for Wrapper<F>
    where S: Parseable,
          F: for<'a> Fn(S) -> Result<S, T, E>,
{
    type Res = T;
    type Err = E;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        self.0(s)
    }
}

pub fn fun<F, S, T, E>(f: F) -> Wrapper<F>
    where S: Parseable,
          F: for<'a> Fn(S) -> Result<S, T, E>,
{
    Wrapper(f)
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


pub fn dbg<P: Parser<S>, S: Parseable>(parser: P) -> Dbg<P> {
    Dbg(parser)
}