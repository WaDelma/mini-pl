use std::fmt;

pub use delimited::*;
pub use repeating::*;
pub use control::*;

mod control;
mod delimited;
mod repeating;

pub trait Parseable: Copy {
    type Symbol;
    fn len(self) -> usize;
    fn is_empty(self) -> bool {
        self.len() == 0
    }
    fn first(self) -> Option<Self::Symbol>;
    fn starts_with(self, pat: &Self) -> bool;
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

pub trait Parser<S: Parseable> {
    type Res;
    fn parse(&self, s: S) -> Option<(Self::Res, S)>;
}

impl<'b, T: Parser<S>, S: Parseable> Parser<S> for &'b T {
    type Res = T::Res;
    fn parse(&self, s: S) -> Option<(Self::Res, S)> {
        (*self).parse(s)
    }
}

impl<S: Parseable> Parser<S> for () {
    type Res = ();
    fn parse(&self, s: S) -> Option<(Self::Res, S)> {
        Some(((), s))
    }
}

pub struct Wrapper<P>(P);

impl<F, T, S> Parser<S> for Wrapper<F>
    where S: Parseable,
          F: for<'a> Fn(S) -> Option<(T, S)>,
{
    type Res = T;
    fn parse(&self, s: S) -> Option<(Self::Res, S)> {
        self.0(s)
    }
}

pub fn fun<F, S, T>(f: F) -> Wrapper<F>
    where S: Parseable,
          F: for<'a> Fn(S) -> Option<(T, S)>,
{
    Wrapper(f)
}

pub struct Tag<S> {
    tag: S,
}

impl<S: Parseable> Parser<S> for Tag<S> {
    type Res = S;
    fn parse(&self, s: S) -> Option<(Self::Res, S)> {
        if s.starts_with(&self.tag) {
            Some((self.tag, s.split_at(self.tag.len()).unwrap().1))
        } else {
            None
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
    fn parse(&self, s: S) -> Option<(Self::Res, S)> {
        s.first()
            .and_then(|f| if f == self.symbol {
                Some((self.symbol.clone(), s.split_at(1).unwrap().1))
            } else {
                None
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
    fn parse(&self, s: S) -> Option<(Self::Res, S)> {
        take(1).parse(s)
            .and_then(|(t, s)| t.first().map(|t| (t, s)))
    }
}

pub fn fst() -> Fst {
    Fst
}

pub struct Dbg<P>(P);

impl<P: Parser<S>, S: Parseable + fmt::Debug> Parser<S> for Dbg<P> {
    type Res = P::Res;
    fn parse(&self, s: S) -> Option<(Self::Res, S)> {
        eprintln!("DBG: {:#?}", s);
        self.0.parse(s)
    }
}


pub fn dbg<P: Parser<S>, S: Parseable>(parser: P) -> Dbg<P> {
    Dbg(parser)
}