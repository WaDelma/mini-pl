use {Parser, Parseable, Sym, Result, take};
use common::Void;

use std::fmt;

/// Accepts anything and returns constant value. Used via `parsco::constant` function.
pub struct Constant<C>(C);

impl<C, S> Parser<S> for Constant<C>
    where S: Parseable,
          C: Clone,
{
    type Res = C;
    type Err = Void;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        Ok((self.0.clone(), s, 0))
    }
}

/// Returns constant value regardless of the input.
/// 
/// Beware: This combinator doesn't eat anything from the input.
/// 
/// # Examples
/// ```rust
/// use parsco::{Parser, Result, constant};
/// 
/// assert_eq!(
///     Ok(("things", "stuff", 0)),
///     constant("things").parse("stuff")
/// );
/// ```
pub fn constant<C>(c: C) -> Constant<C>
    where C: Clone
{
    Constant(c)
}

/// Allows wrapping function as parser. Used via `parsco::fun` function.
pub struct Fun<P>(P);

impl<F, S, T, E> Parser<S> for Fun<F>
    where S: Parseable,
          F: for<'a> Fn(S) -> Result<S, T, E>,
{
    type Res = T;
    type Err = E;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        self.0(s)
    }
}

/// Makes function or closure with right signature to parser.
/// 
/// This is a workaround as direct `Parser` implementation conflicts with `Parser` implementation for reference.
/// 
/// # Examples
/// ```rust
/// use parsco::{Parser, Result, alt, fun};
/// 
/// fn my_parser(s: &str) -> Result<&str, usize, ()> {
///     if s.is_empty() {
///         Err(((), 0..0))
///     } else {
///         Ok((1, s, 0))
///     }
/// }
/// 
/// (alt()
///     | fun(my_parser)
///     | fun(|s: &str| if s.is_empty() {
///         Err(((), 0..0))
///     } else {
///         Ok((1, s, 0))
///     })
/// ).parse("lol");
/// ```
pub fn fun<F, S, T, E>(f: F) -> Fun<F>
    where S: Parseable,
          F: for<'a> Fn(S) -> Result<S, T, E>,
{
    Fun(f)
}

/// Allows recognising given tag. Used via `parsco::tag` function.
pub struct Tag<S> {
    tag: S,
}

impl<S: Parseable> Parser<S> for Tag<S> {
    type Res = S;
    type Err = ();
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        if s.starts_with(&self.tag) {
            Ok((self.tag, s.split_at(self.tag.len()).expect("There should be tag at the start").1, self.tag.len()))
        } else {
            let mut prefix = self.tag;
            while !s.starts_with(&prefix) {
                prefix = prefix.split_at(prefix.len() - 1).expect("Everything but last").0;
            }
            Err(((), 0..prefix.len()))
        }
    }
}

/// Recognises tag at the start of the input.
/// 
/// # Examples
/// ```rust
/// use parsco::{Parser, tag};
/// 
/// assert_eq!(
///     Ok(("return", " foo;", 6)),
///     tag("return").parse("return foo;")
/// );
/// ```
pub fn tag<S>(tag: S) -> Tag<S>
    where S: Parseable
{
    Tag {
        tag,
    }
}

/// Allows recognising given symbol. Used via `parsco::sym` function.
pub struct Symbol<S> {
    symbol: S,
}

impl<S> Parser<S> for Symbol<<S::Symbol as Sym>::Sym>
    where S: Parseable,
          <S::Symbol as Sym>::Sym: PartialEq + Clone
{
    type Res = <S::Symbol as Sym>::Sym;
    type Err = <S::Symbol as Sym>::Sym;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        s.first()
            .ok_or_else(|| (self.symbol.clone(), 0..1))
            .and_then(|f| if f.sym().clone() == self.symbol {
                Ok((self.symbol.clone(), s.split_at(1).expect("There is first").1, 1))
            } else {
                Err((self.symbol.clone(), 0..1))
            })
    }
}

/// Recognises symbol at the start of the input.
/// 
/// # Examples
/// ```rust
/// use parsco::{Parser, sym};
/// 
/// assert_eq!(
///     Ok(('f', "function", 1)),
///     sym('f').parse("ffunction")
/// );
/// ```
/// ```rust
/// use parsco::{Parser, sym};
/// 
/// assert_eq!(
///     Err(('f', 0..1)),
///     sym('f').parse("gfunction")
/// );
/// ```
pub fn sym<S>(symbol: S) -> Symbol<S>
    where S: PartialEq + Clone
{
    Symbol {
        symbol
    }
}

/// Allows taking first symbol. Used via `parsco::fst` function.
pub struct Fst;

impl<S: Parseable> Parser<S> for Fst {
    type Res = S::Symbol;
    type Err = ();
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        take(1).parse(s)
            .and_then(|(t, s, pp)|
                t.first()
                    .map(|t| (t, s, pp))
                    .ok_or(((), 0..pp))
            )
    }
}

/// Takes first symbol from the input.
/// 
/// # Examples
/// ```rust
/// use parsco::{Parser, fst};
/// 
/// assert_eq!(
///     Ok(('f', "unction", 1)),
///     fst().parse("function")
/// );
/// ```
pub fn fst() -> Fst {
    Fst
}

/// Allows printing debug information. Used via `parsco::dbg` function.
pub struct Dbg<P>(P);

impl<P: Parser<S>, S: Parseable + fmt::Debug> Parser<S> for Dbg<P> {
    type Res = P::Res;
    type Err = P::Err;
    fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        eprintln!("DBG: {:#?}", s);
        self.0.parse(s)
    }
}

/// Debug prints the state of the input.
pub fn dbg<P: Parser<S>, S: Parseable>(parser: P) -> Dbg<P> {
    Dbg(parser)
}