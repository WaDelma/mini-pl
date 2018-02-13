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
//! To chain multiple parsers together, the `Parser` trait is implemented for tuples.
//! Currently tuples upto 5 elements are supported.
//! When variadic generics or equivalent feature exists
//! and there are anonymous enums this restriction will be lifted.
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
//!     |(name, value), _, _| Res {
//!         name: name.into(),
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
#![deny(missing_docs)]

use std::ops::Range;

use common::Void;

pub use internal::basic::{tag, sym, fst, dbg, fun};
pub use internal::delimited::{preceded, terminated, delimited};
pub use internal::repeating::{many0, many1, list0, take_while, take_until, ws, take};
pub use internal::control::{alt, map, flat_map, eat, opt};

/// The structs used for the actual parsing.
/// 
/// User of the crate shouldn't have need to use these directly, but use functions that create them instead.
pub mod parsers {
    pub use internal::control::{Alt, Empty, Map, FlatMap, Eat, Opt};
    pub use internal::delimited::{Preceded, Terminated, Delimited};
    pub use internal::repeating::{Many0, Many1, List0, TakeWhile, TakeUntil, Whitespace, Take};
    pub use internal::basic::{Tag, Symbol, Fst, Fun, Dbg};
}

pub mod common;

mod internal;

type Place = usize;

/// Type alias for the result that parsers return.
/// 
/// To use this trait alias, it's recomended to refer it by `parsco::Result` or renaming it more specific `use parsco::Result as ParseResult;`.
pub type Result<S, T, E> = ::std::result::Result<(T, S, Place), (E, Range<Place>)>;

/// Tranforms error type to another. WIP
pub trait FromErr<E> {
    /// Takes error and makes it to `Self`.
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