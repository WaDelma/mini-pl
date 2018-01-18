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