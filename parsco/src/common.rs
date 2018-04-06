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
    (@err_rec
        [$name:ident]
        []
        [$($prev:ident,)*]
        [$($gens:tt)*]
    ) => {
        $name<$($gens)*>
    };
    (@err_rec
        [$name:ident]
        [$var:ident, $($rest:ident,)*]
        [$($prev:ident,)*]
        [$(
            ($($gen:tt)*),
        )*]
    ) => {
        error_enums!(@err_rec
            [$name]
            [$($rest,)*]
            [$($prev,)* $var,]
            [$(
                ($($gen)*),
            )* (
                $($prev::Res,)*
                $var::Err
            ),]
        )
    };
    (@rec [] [$($gen:tt)*] [$prev:ident]) => {};
    (@rec
        [($name:ident, $variant:ident), $($rest:tt)*]
        [$(($gen:ident, $var:ident),)*]
        [$prev:ident]
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
        #[allow(unused_parens, unused_variables, bad_style)]
        impl<$($var,)* $variant, S> Parser<S> for ($($var,)* $variant,)
            where S: Parseable,
                  $($var: Parser<S>,)*
                  $variant: Parser<S>
        {
            type Res = ($($var::Res,)* $variant::Res,);
            type Err = error_enums!(@err_rec [$name] [$($var,)* $variant,] [] []);
            fn parse(&self, s: S) -> Result<S, Self::Res, Self::Err> {
                let ($(ref $var,)* ref $variant,) = *self;
                ($($var,)*)
                    .parse(s)
                    .map_err(|(e, p)| {
                        let e: Self::Err = match e {
                            $($prev::$var(e) => $name::$var(e),)*
                        };
                        #[allow(unreachable_code)]
                        (e, p)
                    })
                    .and_then(|(($($var,)*), s, pp)|
                        match $variant.parse(s) {
                            Ok((r2, s, p)) => Ok((($($var,)* r2,), s, pp + p)),
                            Err((e, p)) => Err(($name::$variant(($($var,)* e)), (pp + p.start)..(pp + p.end))),
                        }
                    )
            }
        }
        error_enums!(@rec [$($rest)*] [$(($gen, $var),)* ($name, $variant),] [$name]);
    };
    ($($name:ident::$variant:ident),+) => {
        error_enums!(@rec [$(($name, $variant),)+] [] [Void]);
    };
);

error_enums!(Err1::V1, Err2::V2, Err3::V3, Err4::V4, Err5::V5, Err6::V6, Err7::V7, Err8::V8);