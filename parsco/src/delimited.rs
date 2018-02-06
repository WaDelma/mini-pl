use {Parser, Parseable, Result, FromErr};

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
    type Err = Err2<P2::Err, P1::Err>;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        (&self.precedator, &self.parser).parse(s)
            .map(|((_, r), s, p)| (r, s, p))
    }
}

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
    type Err = Err2<P1::Err, P2::Err>;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        (&self.parser, &self.terminator).parse(s)
            .map(|((r, _), s, p)|(r, s, p))
    }
}

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
    type Err = Err3<P1::Err, P2::Err, P3::Err>;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        (&self.precedator, &self.parser, &self.terminator).parse(s)
            .map(|((_, r, _), s, p)| (r, s, p))
    }
}

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

#[derive(Clone, Debug, PartialEq)]
pub enum Err2<E1, E2> {
    V1(E1),
    V2(E2),
}

impl<E1, E2, E> FromErr<Err2<E1, E2>> for E
    where E: FromErr<E1>,
          E: FromErr<E2>,
{
    fn from(e: Err2<E1, E2>) -> Self {
        use self::Err2::*;
        match e {
            V1(e) => FromErr::from(e),
            V2(e) => FromErr::from(e),
        }
    }
}

impl<P1, P2, S> Parser<S> for (P1, P2)
    where S: Parseable,
          P1: Parser<S>,
          P2: Parser<S>,
{
    type Res = (P1::Res, P2::Res);
    type Err = Err2<P1::Err, P2::Err>;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        self.0
            .parse(s)
            .map_err(|(e, p)| (Err2::V1(e), p))
            .and_then(|(r1, s, pp)|
                self.1
                    .parse(s)
                    .map(|(r2, s, p)| ((r1, r2), s, pp + p))
                    .map_err(|(e, p)| (Err2::V2(e), pp..(pp + p.end)))
            )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Err3<E1, E2, E3> {
    V1(E1),
    V2(E2),
    V3(E3),
}

impl<E1, E2, E3, E> FromErr<Err3<E1, E2, E3>> for E
    where E: FromErr<E1>,
          E: FromErr<E2>,
          E: FromErr<E3>,
{
    fn from(e: Err3<E1, E2, E3>) -> Self {
        use self::Err3::*;
        match e {
            V1(e) => FromErr::from(e),
            V2(e) => FromErr::from(e),
            V3(e) => FromErr::from(e),
        }
    }
}


impl<P1, P2, P3, S> Parser<S> for (P1, P2, P3)
    where S: Parseable,
          P1: Parser<S>,
          P2: Parser<S>,
          P3: Parser<S>,
{
    type Res = (P1::Res, P2::Res, P3::Res);
    type Err = Err3<P1::Err, P2::Err, P3::Err>;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        use self::Err2::*;
        use self::Err2 as E2;
        use self::Err3 as E3;
        let (ref p1, ref p2, ref p3) = *self;
        ((p1, p2), p3)
            .parse(s)
            .map_err(|(e, p)| (match e {
                E2::V1(V1(e1)) => E3::V1(e1),
                E2::V1(V2(e2)) => E3::V2(e2),
                E2::V2(e3) => E3::V3(e3),
            }, p))
            .map(|(((r1, r2), r3), s, p)| ((r1, r2, r3), s, p))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Err4<E1, E2, E3, E4> {
    V1(E1),
    V2(E2),
    V3(E3),
    V4(E4),
}

impl<E1, E2, E3, E4, E> FromErr<Err4<E1, E2, E3, E4>> for E
    where E: FromErr<E1>,
          E: FromErr<E2>,
          E: FromErr<E3>,
          E: FromErr<E4>,
{
    fn from(e: Err4<E1, E2, E3, E4>) -> Self {
        use self::Err4::*;
        match e {
            V1(e) => FromErr::from(e),
            V2(e) => FromErr::from(e),
            V3(e) => FromErr::from(e),
            V4(e) => FromErr::from(e),
        }
    }
}

impl<P1, P2, P3, P4, S> Parser<S> for (P1, P2, P3, P4)
    where S: Parseable,
          P1: Parser<S>,
          P2: Parser<S>,
          P3: Parser<S>,
          P4: Parser<S>,
{
    type Res = (P1::Res, P2::Res, P3::Res, P4::Res);
    type Err = Err4<P1::Err, P2::Err, P3::Err, P4::Err>;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        use self::Err3::*;
        use self::Err2 as E2;
        use self::Err4 as E4;
        let (ref p1, ref p2, ref p3, ref p4) = *self;
        ((p1, p2, p3), p4)
            .parse(s)
            .map_err(|(e, p)| (match e {
                E2::V1(V1(e1)) => E4::V1(e1),
                E2::V1(V2(e2)) => E4::V2(e2),
                E2::V1(V3(e3)) => E4::V3(e3),
                E2::V2(e4) => E4::V4(e4),
            }, p))
            .map(|(((r1, r2, r3), r4), s, p)| ((r1, r2, r3, r4), s, p))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Err5<E1, E2, E3, E4, E5> {
    V1(E1),
    V2(E2),
    V3(E3),
    V4(E4),
    V5(E5),
}

impl<E1, E2, E3, E4, E5, E> FromErr<Err5<E1, E2, E3, E4, E5>> for E
    where E: FromErr<E1>,
          E: FromErr<E2>,
          E: FromErr<E3>,
          E: FromErr<E4>,
          E: FromErr<E5>,
{
    fn from(e: Err5<E1, E2, E3, E4, E5>) -> Self {
        use self::Err5::*;
        match e {
            V1(e) => FromErr::from(e),
            V2(e) => FromErr::from(e),
            V3(e) => FromErr::from(e),
            V4(e) => FromErr::from(e),
            V5(e) => FromErr::from(e),
        }
    }
}

impl<P1, P2, P3, P4, P5, S> Parser<S> for (P1, P2, P3, P4, P5)
    where S: Parseable,
          P1: Parser<S>,
          P2: Parser<S>,
          P3: Parser<S>,
          P4: Parser<S>,
          P5: Parser<S>,
{
    type Res = (P1::Res, P2::Res, P3::Res, P4::Res, P5::Res);
    type Err = Err5<P1::Err, P2::Err, P3::Err, P4::Err, P5::Err>;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        use self::Err4::*;
        use self::Err2 as E2;
        use self::Err5 as E5;
        let (ref p1, ref p2, ref p3, ref p4, ref p5) = *self;
        ((p1, p2, p3, p4), p5)
            .parse(s)
            .map_err(|(e, p)| (match e {
                E2::V1(V1(e1)) => E5::V1(e1),
                E2::V1(V2(e2)) => E5::V2(e2),
                E2::V1(V3(e3)) => E5::V3(e3),
                E2::V1(V4(e4)) => E5::V4(e4),
                E2::V2(e5) => E5::V5(e5),
            }, p))
            .map(|(((r1, r2, r3, r4), r5), s, p)| ((r1, r2, r3, r4, r5), s, p))
    }
}