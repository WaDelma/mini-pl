use {Parser, Parseable, Result, Tag, tag, terminated, preceded, opt};

pub struct TakeWhile<F> {
    predicate: F,
}

impl<'b, F> Parser<&'b str> for TakeWhile<F>
    where F: Fn(char) -> bool,
{
    type Res = String;
    type Err = ();
    fn parse<'a>(&self, s: &'a str) -> Result<&'a str, Self::Res, Self::Err> {
        if s.len() == 0 {
            Err(())
        } else if let Some(i) = s.char_indices().skip_while(|&(_, c)| (self.predicate)(c)).map(|c| c.0).next() {
            if i == 0 {
                Err(())
            } else {
                Ok((s[..i].to_string(), &&s[i..]))
            }
        } else {
            Ok((s.to_string(), &&s[..0]))
        }
    }
}

pub fn take_while<F>(predicate: F) -> TakeWhile<F>
    where F: Fn(char) -> bool,
{
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
    type Err = ();
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        let mut cur = s;
        let mut n = 0;
        loop {
            if let Ok((res, ss)) = self.parser.parse(cur) {
                return Ok(((s.split_at(n).unwrap().0, res), ss));
            }
            if let Ok((_, rest)) = take(1).parse(cur) {
                n += 1;
                cur = rest;
            } else {
                return Err(());
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
        while let Ok((t, rest)) = self.parser.parse(s) {
            s = rest;
            result.push(t);
        }
        Ok((result, s))
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
            .and_then(|(t, s)|
                if t.is_empty() {
                    Err(())
                } else {
                    Ok((t, s))
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
          <P as Parser<S>>::Err: ::std::convert::From<()>
{
    type Res = Vec<P::Res>;
    type Err = ();
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        many0(
            terminated(&self.parser, &self.separator)
        ).parse(s)
    }
}

pub fn list0<P, S>(separator: S, parser: P) -> List0<P, S>
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
    type Err = P::Err;
    fn parse(&self, s: &'b str) ->  Result<&'b str, Self::Res, Self::Err> {
        preceded(
            opt(take_while(char::is_whitespace)),
            &self.parser
        ).parse(s)
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
        s.split_at(self.amount).ok_or(())
    }
}

pub fn take(amount: usize) -> Take {
    Take {
        amount
    }
}
