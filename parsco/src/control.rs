use std::marker::PhantomData;
use std::ops::BitOr;

use {Parser, Parseable};

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

impl<P1, P2, T, S> Parser<S> for Alt<P1, P2>
    where S: Parseable,
          P1: Parser<S, Res=T>,
          P2: Parser<S, Res=T>,
{
    type Res = T;
    fn parse(&self, s: S) -> Option<(Self::Res, S)> {
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

impl<T, S> Parser<S> for Empty<T>
    where S: Parseable,
{
    type Res = T;
    fn parse(&self, _: S) -> Option<(Self::Res, S)> {
        None
    }
}

pub fn alt<P>() -> Empty<P> {
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
    fn parse(&self, s: S) -> Option<(Self::Res, S)> {
        self.parser.parse(s)
            .map(|(r, s)| (Some(r), s))
            .or_else(|| Some((None, s)))
    }
}

pub fn opt<P>(parser: P) -> Opt<P> {
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
    fn parse(&self, s: S) -> Option<(Self::Res, S)> {
        self.parser.parse(s)
            .map(|(res, s)| ((self.map)(res), s))
    }
}

pub fn map<P, F>(parser: P, map: F) -> Map<P, F> {
    Map {
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
    fn parse(&self, s: S) -> Option<(Self::Res, S)> {
        self.parser.parse(s)
            .map(|(_, s)| (self.substitute.clone(), s))
    }
}

pub fn eat<P, T>(parser: P, substitute: T) -> Eat<P, T> {
    Eat {
        parser,
        substitute
    }
}