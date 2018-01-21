use {Parser, Parseable, Tag, tag, terminated, preceded, opt};

pub struct TakeWhile<F> {
    predicate: F,
}

impl<'b, F> Parser<&'b str> for TakeWhile<F>
    where F: Fn(char) -> bool,
{
    type Res = String;
    fn parse<'a>(&self, s: &'a str) -> Option<(Self::Res, &'a str)> {
        if s.len() == 0 {
            None
        } else if let Some(i) = s.char_indices().skip_while(|&(_, c)| (self.predicate)(c)).map(|c| c.0).next() {
            if i == 0 {
                None
            } else {
                Some((s[..i].to_string(), &&s[i..]))
            }
        } else {
            Some((s.to_string(), &&s[..0]))
        }
    }
}

pub fn take_while<F>(predicate: F) -> TakeWhile<F> {
    TakeWhile {
        predicate
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
    fn parse(&self, s: S) -> Option<(Self::Res, S)> {
        let mut cur = s;
        let mut n = 0;
        loop {
            if let Some((res, ss)) = self.parser.parse(cur) {
                return Some(((s.split_at(n).unwrap().0, res), ss));
            }
            if let Some((_, rest)) = take(1).parse(cur) {
                n += 1;
                cur = rest;
            } else {
                return None;
            }
        }
    }
}

pub fn take_until<P>(parser: P) -> TakeUntil<P> {
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
    fn parse(&self, mut s: S) -> Option<(Self::Res, S)> {
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

impl<P, S> Parser<S> for Many1<P>
    where S: Parseable,
          P: Parser<S>,
{
    type Res = Vec<P::Res>;
    fn parse(&self, s: S) -> Option<(Self::Res, S)> {
        many0(&self.parser).parse(s)
            .and_then(|(t, s)|
                if t.is_empty() {
                    None
                } else {
                    Some((t, s))
                })
    }
}

pub fn many1<P>(parser: P) -> Many1<P> {
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
    fn parse<'a>(&self, s: S) -> Option<(Self::Res, S)> {
        many0(
            terminated(&self.parser, &self.separator)
        ).parse(s)
    }
}

pub fn list0<P, S>(separator: S, parser: P) -> List0<P, S> {
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
    fn parse(&self, s: &'b str) -> Option<(Self::Res, &'b str)> {
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

pub struct Take {
    amount: usize,
}

impl<S> Parser<S> for Take
    where S: Parseable,
{
    type Res = S;
    fn parse(&self, s: S) -> Option<(Self::Res, S)> {
        s.split_at(self.amount)
    }
}

pub fn take(amount: usize) -> Take {
    Take {
        amount
    }
}
