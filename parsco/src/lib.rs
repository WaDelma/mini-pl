use std::marker::PhantomData;
use std::ops::BitOr;

pub use delimited::*;
pub use repeating::*;

mod delimited;
mod repeating;

pub trait Parser {
    type Res;
    fn parse<'a>(&self, s: &'a str) -> Option<(Self::Res, &'a str)>;
}

impl<'b, T: Parser> Parser for &'b T {
    type Res = T::Res;
    fn parse<'a>(&self, s: &'a str) -> Option<(Self::Res, &'a str)> {
        (*self).parse(s)
    }
}

pub struct Wrapper<P>(P);

impl<F, T> Parser for Wrapper<F>
    where F: for<'a> Fn(&'a str) -> Option<(T, &'a str)>
{
    type Res = T;
    fn parse<'a>(&self, s: &'a str) -> Option<(Self::Res, &'a str)> {
        self.0(s)
    }
}

pub fn fun<F>(f: F) -> Wrapper<F> {
    Wrapper(f)
}

pub struct Tag<T> {
    tag: &'static str,
    result: T,
}

impl<T: Clone> Parser for Tag<T> {
    type Res = T;
    fn parse<'a>(&self, s: &'a str) -> Option<(Self::Res, &'a str)> {
        if s.starts_with(self.tag) {
            Some((self.result.clone(), &s[self.tag.len()..]))
        } else {
            None
        }
    }
}

pub fn tag<T>(tag: &'static str, result: T) -> Tag<T> {
    Tag {
        tag,
        result
    }
}

pub struct Alt<P1, P2> {
    parser: P1,
    rest: P2,
}

impl<P1, P2, P3> BitOr<P3> for Alt<P1, P2> {
    type Output = Alt<Self, P3>;
    fn bitor(self, lhs: P3) -> Self::Output {
        Alt {
            parser: self,
            rest: lhs,
        }
    }
}

impl<P1: Parser<Res=T>, P2: Parser<Res=T>, T> Parser for Alt<P1, P2> {
    type Res = T;
    fn parse<'a>(&self, s: &'a str) -> Option<(Self::Res, &'a str)> {
        self.parser.parse(s)
            .or_else(|| self.rest.parse(s))
    }
}

pub struct Empty<T>(PhantomData<T>);

impl<P1, P2> BitOr<P2> for Empty<P1> {
    type Output = Alt<Self, P2>;
    fn bitor(self, lhs: P2) -> Self::Output {
        Alt {
            parser: self,
            rest: lhs,
        }
    }
}

impl<T> Parser for Empty<T> {
    type Res = T;
    fn parse<'a>(&self, s: &'a str) -> Option<(Self::Res, &'a str)> {
        None
    }
}

pub fn alt<P>() -> Empty<P> {
    Empty(PhantomData)
}