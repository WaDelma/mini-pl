use Parser;

pub struct Preceded<P1, P2> {
    parser: P1,
    precedator: P2,
}

impl<P1: Parser<Res=T1>, P2: Parser<Res=T2>, T1, T2> Parser for Preceded<P1, P2> {
    type Res = T1;
    fn parse<'a>(&self, s: &'a str) -> Option<(Self::Res, &'a str)> {
        self.precedator.parse(s)
            .and_then(|(_, s)| {
                self.parser.parse(s)
            })
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

impl<P1: Parser<Res=T1>, P2: Parser<Res=T2>, T1, T2> Parser for Terminated<P1, P2> {
    type Res = T1;
    fn parse<'a>(&self, s: &'a str) -> Option<(Self::Res, &'a str)> {
        self.parser
            .parse(s)
            .and_then(|(result, s)| {
                self.terminator
                    .parse(s)
                    .map(|(_, s)| (result, s))
            })
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

impl<P1: Parser<Res=T1>, P2: Parser<Res=T2>, P3: Parser<Res=T3>, T1, T2, T3> Parser for Delimited<P1, P2, P3> {
    type Res = T2;
    fn parse<'a>(&self, s: &'a str) -> Option<(Self::Res, &'a str)> {
        terminated(preceded(&self.precedator, &self.parser), &self.terminator).parse(s)
    }
}

pub fn delimited<P1, P2, P3>(precedator: P1, parser: P2, terminator: P3) -> Delimited<P1, P2, P3> {
    Delimited {
        precedator,
        parser,
        terminator,
    }
}
