use {Parser, Parseable, Result, Tag, tag, terminated, preceded, opt, Err2};

use std::marker::PhantomData;

pub struct TakeWhile<F, P> {
    predicate: F,
    _marker: PhantomData<P>,
}

impl<F, S> Parser<S> for TakeWhile<F, S>
    where S: Parseable,
          F: Fn(<S as Parseable>::Symbol) -> bool,
{
    type Res = S;
    type Err = ();
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        let mut n = 0;
        let mut cur = s;
        while let Some((start, end)) = cur.split_at(1) {
            if (self.predicate)(start.first().expect("Split should ensure that there is first.")) {
                cur = end;
                n += 1;
            } else {
                break;
            }
        }
        if n == 0 {
            Err(((), 0..0))
        } else {
            let (start, end) = s.split_at(n).expect("This index should be already split at.");
            Ok((start, end, n))
        }
    }
}

pub fn take_while<F, S>(predicate: F) -> TakeWhile<F, S>
    where F: Fn(char) -> bool,
          S: Parseable
{
    TakeWhile {
        predicate,
        _marker: PhantomData,
    }
}

pub struct TakeUntil<P> {
    parser: P,
}

impl<'b, P, S> Parser<S> for TakeUntil<P>
    where S: Parseable,
          P: Parser<S>,
{
    type Res = (S, P::Res);
    type Err = ();
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        let mut cur = s;
        let mut n = 0;
        loop {
            if let Ok((res, ss, pp)) = self.parser.parse(cur) {
                return Ok(((s.split_at(n).unwrap().0, res), ss, n + pp));
            }
            if let Ok((_, rest, _)) = take(1).parse(cur) {
                n += 1;
                cur = rest;
            } else {
                return Err(((), 0..n));
            }
        }
    }
}

pub fn take_until<P, S>(parser: P) -> TakeUntil<P>
    where S: Parseable,
          P: Parser<S>,
{
    TakeUntil {
        parser
    }
}

pub struct Many0<P> {
    parser: P,
}

impl<P, S> Parser<S> for Many0<P>
    where S: Parseable,
          P: Parser<S>,
{
    type Res = Vec<P::Res>;
    type Err = ();
    fn parse(&self, mut s: S) -> Result<S, Self::Res, Self::Err> {
        let mut result = Vec::new();
        let mut p = 0;
        while let Ok((t, rest, pp)) = self.parser.parse(s) {
            p += pp;
            s = rest;
            result.push(t);
        }
        Ok((result, s, p))
    }
}

pub fn many0<P, S>(parser: P) -> Many0<P>
    where S: Parseable,
          P: Parser<S>,
{
    Many0 {
        parser
    }
}

pub struct Many1<P> {
    parser: P,
}

impl<P, S> Parser<S> for Many1<P>
    where S: Parseable,
          P: Parser<S>,
{
    type Res = Vec<P::Res>;
    type Err = ();
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        many0(&self.parser).parse(s)
            .and_then(|(t, s, pp)|
                if t.is_empty() {
                    Err(((), 0..pp))
                } else {
                    Ok((t, s, pp))
                })
    }
}

pub fn many1<P, S>(parser: P) -> Many1<P>
    where S: Parseable,
          P: Parser<S>,
{
    Many1 {
        parser
    }
}

pub struct List0<P, S> {
    separator: Tag<S>,
    parser: P,
}

impl<P, S> Parser<S> for List0<P, S>
    where S: Parseable,
          P: Parser<S>,
{
    type Res = Vec<P::Res>;
    type Err = ();
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        many0(
            terminated(&self.parser, &self.separator)
        ).parse(s)
    }
}

pub fn list0<P, S>(parser: P, separator: S) -> List0<P, S>
    where S: Parseable,
          P: Parser<S>,
{
    List0 {
        separator: tag(separator),
        parser
    }
}

pub struct Whitespace<P> {
    parser: P,   
}

impl<'b, P> Parser<&'b str> for Whitespace<P>
    where P: Parser<&'b str>,
{
    type Res = P::Res;
    type Err = Err2<(), P::Err>;
    fn parse(&self, s: &'b str) ->  Result<&'b str, Self::Res, Self::Err> {
        preceded(
            opt(take_while(char::is_whitespace)),
            &self.parser
        ).parse(s)
        .map_err(|(e, p)| (match e {
            Err2::V1(_) => Err2::V1(()),
            Err2::V2(e) => Err2::V2(e),
        }, p))
    }
}

pub fn ws<'b, P>(parser: P) -> Whitespace<P>
    where P: Parser<&'b str>,
{
    Whitespace {
        parser
    }
}

pub struct Take {
    amount: usize,
}

impl<S> Parser<S> for Take
    where S: Parseable,
{
    type Res = S;
    type Err = ();
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        s.split_at(self.amount)
            .ok_or(((), 0..self.amount))
            .map(|(s, r)| (s, r, self.amount))
    }
}

pub fn take(amount: usize) -> Take {
    Take {
        amount
    }
}
