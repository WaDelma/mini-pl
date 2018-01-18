use {Parser, Tag, tag, terminated};

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