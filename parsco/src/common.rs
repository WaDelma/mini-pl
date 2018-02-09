//! Technical types used by parsers.

use {FromErr, Parser, Parseable, Result};

/// Never type.
/// 
/// This is workaround while `!` is not stable.
/// It's used to indicate imposibility of certain return values.
pub enum Void {}

impl<S: Parseable> Parser<S> for Void {
    type Res = Self;
    type Err = Self;
    fn parse(&self, _: S) -> Result<S, Self::Res, Self::Err> {
        unreachable!();
    }
}

/// Two variant enum.
/// 
/// This is workaround to the lack of anonymous enums.
/// It's used as an error for tuple of parsers.
#[derive(Clone, Debug, PartialEq)]
pub enum Err2<E1, E2> {
    /// First variant
    V1(E1),
    /// Second variant
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

/// Three variant enum.
/// 
/// This is workaround to the lack of anonymous enums.
/// It's used as an error for tuple of parsers.
#[derive(Clone, Debug, PartialEq)]
pub enum Err3<E1, E2, E3> {
    /// First variant
    V1(E1),
    /// Second variant
    V2(E2),
    /// Third variant
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

/// Four variant enum.
/// 
/// This is workaround to the lack of anonymous enums.
/// It's used as an error for tuple of parsers.
#[derive(Clone, Debug, PartialEq)]
pub enum Err4<E1, E2, E3, E4> {
    /// First variant
    V1(E1),
    /// Second variant
    V2(E2),
    /// Third variant
    V3(E3),
    /// Fourth variant
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

/// Five variant enum.
/// 
/// This is workaround to the lack of anonymous enums.
/// It's used as an error for tuple of parsers.
#[derive(Clone, Debug, PartialEq)]
pub enum Err5<E1, E2, E3, E4, E5> {
    /// First variant
    V1(E1),
    /// Second variant
    V2(E2),
    /// Third variant
    V3(E3),
    /// Fourth variant
    V4(E4),
    /// Fift variant
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