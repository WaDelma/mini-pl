use {Parser, Parseable};

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
    fn parse(&self, s: S) -> Option<(Self::Res, S)> {
        (&self.precedator, &self.parser).parse(s)
            .map(|((_, r), s)| (r, s))
    }
}

pub fn preceded<P1, P2>(precedator: P2, parser: P1) -> Preceded<P1, P2> {
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
    fn parse(&self, s: S) -> Option<(Self::Res, S)> {
        (&self.parser, &self.terminator).parse(s)
            .map(|((r, _), s)|(r, s))
    }
}

pub fn terminated<P1, P2>(parser: P1, terminator: P2) -> Terminated<P1, P2> {
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
    fn parse<'a>(&self, s: S) -> Option<(Self::Res, S)> {
        (&self.precedator, &self.parser, &self.terminator).parse(s)
            .map(|((_, r, _), s)| (r, s))
    }
}

pub fn delimited<P1, P2, P3>(precedator: P1, parser: P2, terminator: P3) -> Delimited<P1, P2, P3> {
    Delimited {
        precedator,
        parser,
        terminator,
    }
}

impl<P1, P2, S> Parser<S> for (P1, P2)
    where S: Parseable,
          P1: Parser<S>,
          P2: Parser<S>,
{
    type Res = (P1::Res, P2::Res);
    fn parse(&self, s: S) -> Option<(Self::Res, S)> {
        self.0
            .parse(s)
            .and_then(|(r1, s)|
                self.1
                    .parse(s)
                    .map(|(r2, s)| ((r1, r2), s))
            )
    }
}

impl<P1, P2, P3, S> Parser<S> for (P1, P2, P3)
    where S: Parseable,
          P1: Parser<S>,
          P2: Parser<S>,
          P3: Parser<S>,
{
    type Res = (P1::Res, P2::Res, P3::Res);
    fn parse(&self, s: S) -> Option<(Self::Res, S)> {
        let (ref p1, ref p2, ref p3) = *self;
        ((p1, p2), p3)
            .parse(s)
            .map(|(((r1, r2), r3), s)| ((r1, r2, r3), s))
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
    fn parse(&self, s: S) -> Option<(Self::Res, S)> {
        let (ref p1, ref p2, ref p3, ref p4) = *self;
        ((p1, p2, p3), p4)
            .parse(s)
            .map(|(((r1, r2, r3), r4), s)| ((r1, r2, r3, r4), s))
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
    fn parse(&self, s: S) -> Option<(Self::Res, S)> {
        let (ref p1, ref p2, ref p3, ref p4, ref p5) = *self;
        ((p1, p2, p3, p4), p5)
            .parse(s)
            .map(|(((r1, r2, r3, r4), r5), s)| ((r1, r2, r3, r4, r5), s))
    }
}