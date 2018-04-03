//! Technical types used by parsers.

use {FromErr, Parser, Parseable, Result};

/// Never type.
/// 
/// This is workaround while `!` is not stable.
/// It's used to indicate imposibility of certain return values.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Void {}

impl<S: Parseable> Parser<S> for Void {
    type Res = Self;
    type Err = Self;
    fn parse(&self, _: S) -> Result<S, Self::Res, Self::Err> {
        unreachable!();
    }
}

macro_rules! error_enums(
    (@rec [] [$($gen:tt)*]) => {};
    (@rec
        [
            ($name:ident, $variant:ident),
            $($rest:tt)*
        ]
        [
            $(($gen:ident, $var:ident),)*
        ]
    ) => {
        #[allow(missing_docs)]
        #[doc =
"Generic enum used for errors.

This is workaround to the lack of anonymous enums.
It's used as an error for tuple of parsers.
"]
        #[derive(Clone, Debug, PartialEq)]
        pub enum $name<$($var,)* $variant> {
            $($var($var),)*
            $variant($variant)
        }
        impl<$($var,)* $variant, E> FromErr<$name<$($var,)* $variant>> for E
            where $(E: FromErr<$var>,)*
                  E: FromErr<$variant>
        {
            fn from(e: $name<$($var,)* $variant>) -> Self {
                use self::$name::*;
                match e {
                    $($var(e) => FromErr::from(e),)*
                    $variant(e) => FromErr::from(e)
                }
            }
        }
        // TODO: Implement `Parser` for tuples
        // impl<$($var,)* $variant, S> Parser<S> for ($($var,)* $variant)
        //     where S: Parseable,
        //           $($var: Parser<S>,)*
        //           $variant: Parser<S>
        // {
        //     type Res = (($var::Res,)* $variant::Res);
        //     type Err = ::common::Err2<P1::Err, (P1::Res, P2::Err)>;
        //     fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
        //         self.0
        //             .parse(s)
        //             .map_err(|(e, p)| (::common::Err2::V1(e), p))
        //             .and_then(|(r1, s, pp)|
        //                 match self.1.parse(s) {
        //                     Ok((r2, s, p)) => Ok(((r1, r2), s, pp + p)),
        //                     Err((e, p)) => Err((::common::Err2::V2((r1, e)), (pp + p.start)..(pp + p.end))),
        //                 }
        //             )
        //     }
        // }
        error_enums!(@rec [$($rest)*] [$(($gen, $var),)* ($name, $variant),]);
    };
    ($($name:ident::$variant:ident),+) => {
        error_enums!(@rec [$(($name, $variant),)+] []);
    };
);

error_enums!(Err1::V1, Err2::V2, Err3::V3, Err4::V4, Err5::V5, Err6::V6, Err7::V7);