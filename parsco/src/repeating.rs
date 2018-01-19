use {Parser, Tag, tag, terminated, preceded, opt};

pub struct TakeWhile<F> {
    predicate: F,
}

impl<F: Fn(char) -> bool> Parser for TakeWhile<F> {
    type Res = String;
    fn parse<'a>(&self, s: &'a str) -> Option<(Self::Res, &'a str)> {
        if s.len() == 0 {
            None
        } else if let Some(i) = s.char_indices().skip_while(|&(_, c)| (self.predicate)(c)).map(|c| c.0).next() {
            if i == 0 {
                None
            } else {
                Some((s[..i].to_string(), &s[i..]))
            }
        } else {
            Some((s.to_string(), &s[..0]))
        }
    }
}

pub fn take_while<F>(predicate: F) -> TakeWhile<F> {
    TakeWhile {
        predicate
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

pub struct Whitespace<P> {
    parser: P,   
}

impl<P: Parser> Parser for Whitespace<P> {
    type Res = P::Res;
    fn parse<'a>(&self, s: &'a str) -> Option<(Self::Res, &'a str)> {
        preceded(
            opt(take_while(char::is_whitespace)),
            &self.parser
        ).parse(s)
    }
}

pub fn ws<P>(parser: P) -> Whitespace<P> {
    Whitespace {
        parser
    }
}