extern crate parsco;
extern crate char_stream;
extern crate num_traits;
extern crate num_bigint;

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
//pub mod parser;
//pub mod interpreter;

pub type Ident = String;