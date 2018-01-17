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

pub struct Many0<P> {
    parser: P,
}

impl<P: Parser<Res=T>, T> Parser for Many0<P> {
    type Res = Vec<T>;
    fn parse<'a>(&self, mut s: &'a str) -> Option<(Self::Res, &'a str)> {
        let mut result = Vec::new();
        while let Some((t, rest)) = self.parser.parse(s) {
            s = rest;
            result.push(t);
        }
        Some((result, s))
    }
}

pub fn many0<P>(parser: P) -> Many0<P> {
    Many0 {
        parser
    }
}

pub struct Many1<P> {
    parser: P,
}

impl<P: Parser<Res=T>, T> Parser for Many1<P> {
    type Res = Vec<T>;
    fn parse<'a>(&self, mut s: &'a str) -> Option<(Self::Res, &'a str)> {
        let mut result = Vec::new();
        if let Some((t, rest)) = self.parser.parse(s) {
            s = rest;
            result.push(t);
        } else {
            return None;
        }
        if let Some((t, rest)) = many0(&self.parser).parse(s) {
            s = rest;
            result.extend(t);
        }
        Some((result, s))
    }
}

pub fn many1<P>(parser: P) -> Many1<P> {
    Many1 {
        parser
    }
}

pub struct List0<P> {
    separator: Tag<()>,
    parser: P,
}

impl<P: Parser<Res=T>, T> Parser for List0<P> {
    type Res = Vec<T>;
    fn parse<'a>(&self, s: &'a str) -> Option<(Self::Res, &'a str)> {
        many0(
            terminated(&self.parser, &self.separator)
        ).parse(s)
    }
}

pub fn list0<P>(separator: &'static str, parser: P) -> List0<P> {
    List0 {
        separator: tag(separator, ()),
        parser
    }
}