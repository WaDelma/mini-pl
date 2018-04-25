//! # Mini-pl
//! 
//! Lexer, parser, static analyzer and interpreter for mini-pl language.
//! 
//! Each of these are implemented as separate pass to faciliate cleaner code.
#![deny(missing_docs)]

extern crate parsco;
extern crate char_stream;
extern crate num_traits;
extern crate num_bigint;
extern crate num_rational;
extern crate llvm_sys;

// Replace standard library provided `assert_eq` macro with one that does pretty printing.
// TODO: Remove this uggly hack
#[cfg(test)]
macro_rules! assert_eq {
    ($left:expr, $right:expr) => {
        match (&$left, &$right) {
            (left_val, right_val) => {
                if !(*left_val == *right_val) {
                    panic!(r#"assertion failed: `(left == right)`
        left: `{:#?}`,
        right: `{:#?}`"#, left_val, right_val)
                }
            }
        }
    };
}

pub mod lexer;
pub mod parser;
// pub mod analyzer;
// pub mod interpreter;
pub mod util;
pub mod codegen;

/// New type for identifier, which is just a `String`.
// TODO: Optimise memory usage by doing interning.
pub type Ident = String;