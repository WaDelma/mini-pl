use util::{Positioned, Position};
use super::{Token, tokenize};
use super::tokens::Token::*;
use super::tokens::Punctuation::*;
use super::tokens::Side::*;
use super::tokens::Keyword::*;
use super::tokens::Operator::*;
use super::tokens::Literal::*;

mod error;

fn tok(token: Token, (from_line, from_column): (usize, usize), (to_line, to_column): (usize, usize)) -> Positioned<Token> {
    Positioned::new(token, Position::new(from_line, from_column), Position::new(to_line, to_column))
}

#[test]
fn string_literal_lexes() {
    assert_eq!(
        Ok((
            vec![
                tok(Literal(StringLit(String::from("Hello, World!"))), (0, 0), (0, 15)),
            ],
            "",
            15
        )),
        tokenize(r#""Hello, World!""#)
    );
}

#[test]
fn string_literal_with_escaped_quote_lexes() {
    assert_eq!(
        Ok((
           vec![
                tok(Literal(StringLit(String::from("Hello, \"World!\""))), (0, 0), (0, 19)),
            ],
            "",
            19
        )),
        tokenize(r#""Hello, \"World!\"""#)
    );
}

#[test]
fn string_literal_with_escaped_linebreak_lexes() {
    assert_eq!(
        Ok((
           vec![
                tok(Literal(StringLit(String::from("Hello\n World!"))), (0, 0), (0, 16)),
            ],
            "",
            16
        )),
        tokenize(r#""Hello\n World!""#)
    );
}

#[test]
fn string_literal_with_escaped_tab_lexes() {
    assert_eq!(
        Ok((
           vec![
                tok(Literal(StringLit(String::from("Hello\t World!"))), (0, 0), (0, 16)),
            ],
            "",
            16
        )),
        tokenize(r#""Hello\t World!""#)
    );
}

#[test]
fn string_literal_with_escaped_backslash_lexes() {
    assert_eq!(
        Ok((
           vec![
                tok(Literal(StringLit(String::from("Hello\\ World!"))), (0, 0), (0, 16)),
            ],
            "",
            16
        )),
        tokenize(r#""Hello\\ World!""#)
    );
}

#[test]
fn string_literal_with_alert_lexes() {
    assert_eq!(
        Ok((
           vec![
                tok(Literal(StringLit(String::from("Hello, World\x07"))), (0, 0), (0, 16)),
            ],
            "",
            16
        )),
        tokenize(r#""Hello, World\a""#)
    );
}

#[test]
fn string_literal_with_backspace_lexes() {
    assert_eq!(
        Ok((
            vec![
                tok(Literal(StringLit(String::from("Hello,\x08 World!"))), (0, 0), (0, 17)),
            ],
            "",
            17
        )),
        tokenize(r#""Hello,\b World!""#)
    );
}

#[test]
fn string_literal_with_formfeed_lexes() {
    assert_eq!(
        Ok((
            vec![
                tok(Literal(StringLit(String::from("Hello, World!\x0C"))), (0, 0), (0, 17)),
            ],
            "",
            17
        )),
        tokenize(r#""Hello, World!\f""#)
    );
}

#[test]
fn string_literal_with_vertical_tab_lexes() {
    assert_eq!(
        Ok((
           vec![
                tok(Literal(StringLit(String::from("Hello\x0BWorld!"))), (0, 0), (0, 15)),
            ],
            "",
            15
        )),
        tokenize(r#""Hello\vWorld!""#)
    );
}

#[test]
fn string_literal_with_hexadecimal_escape_lexes() {
    assert_eq!(
        Ok((
           vec![
                tok(Literal(StringLit(String::from("\x1B\x07\x0C"))), (0, 0), (0, 14)),
            ],
            "",
            14
        )),
        tokenize(r#""\x1B\x07\x0C""#)
    );
}

#[test]
fn string_literal_with_unicode_escape_lexes() {
    assert_eq!(
        Ok((
           vec![
                tok(Literal(StringLit(String::from("👌 🤔 😽 ⸙ 𝝅 ≪ 𝝉 ⸎"))), (0, 0), (0, 77)),
            ],
            "",
            77
        )),
        tokenize(r#""\U0001F44C \U0001F914 \U0001F63D \u2E19 \U0001D745 \u226A \U0001D749 \u2E0E""#)
    );
}

#[test]
fn string_literal_with_octal_escape_lexes() {
    assert_eq!(
        Ok((
           vec![
                tok(Literal(StringLit(String::from_utf8(vec![0o0, 0o10, 0o100, 0o2, 0o12, 0o102]).unwrap())), (0, 0), (0, 20)),
            ],
            "",
            20
        )),
        tokenize(r#""\0\10\100\2\12\102""#)
    );
}

#[test]
fn line_comment_lexes() {
    assert_eq!(
        Ok((
           vec![
                tok(Identifier(String::from("x")), (0, 0), (0, 1)),
            ],
            "",
            4
        )),
        tokenize("x//y")
    );
}

#[test]
fn multiline_comment_simple_lexes() {
    assert_eq!(
        Ok((
           vec![
                tok(Identifier(String::from("x")), (0, 0), (0, 1)),
            ],
            "",
            6
        )),
        tokenize("x/*y*/")
    );
}

#[test]
fn multiline_comment_star_lexes() {
    assert_eq!(
        Ok((
           vec![
                tok(Identifier(String::from("x")), (0, 0), (0, 1)),
            ],
            "",
            7
        )),
        tokenize("x/**y*/")
    );
}

#[test]
fn multiline_comment_nested_lexes() {
    assert_eq!(
        Ok((
           vec![
                tok(Identifier(String::from("x")), (0, 0), (0, 1)),
            ],
            "",
            10
        )),
        tokenize("x/*/*y*/*/")
    );
}

#[test]
fn multiline_comment_complex_nesting_lexes() {
    assert_eq!(
        Ok((
           vec![
                tok(Identifier(String::from("x")), (0, 0), (0, 1)),
            ],
            "",
            16
        )),
        tokenize("x/*/*y*/z/*w*/*/")
    );
}

#[test]
fn identifiers_lex() {
    assert_eq!(
        Ok((
           vec![
                tok(Identifier(String::from("x")), (0, 0), (0, 1)),
            ],
            "",
            1
        )),
        tokenize("x")
    );
    assert_eq!(
        Ok((
           vec![
                tok(Identifier(String::from("l33t")), (0, 0), (0, 4)),
            ],
            "",
            4
        )),
        tokenize("l33t")
    );
    assert_eq!(
        Ok((
           vec![
                tok(Identifier(String::from("l_l")), (0, 0), (0, 3)),
            ],
            "",
            3
        )),
        tokenize("l_l")
    );
}

#[test]
fn keyword_starting_identifier_lexes() {
    let string = "foray into reading independent dodo endofunctors";
    assert_eq!(
        Ok((
           vec![
                tok(Identifier(String::from("foray")), (0, 0), (0, 5)),
                tok(Identifier(String::from("into")), (0, 6), (0, 10)),
                tok(Identifier(String::from("reading")), (0, 11), (0, 18)),
                tok(Identifier(String::from("independent")), (0, 19), (0, 30)),
                tok(Identifier(String::from("dodo")), (0, 31), (0, 35)),
                tok(Identifier(String::from("endofunctors")), (0, 36), (0, 48)),
            ],
            "",
            string.len()
        )),
        tokenize(string)
    );
}


#[ignore] // TODO: To fix this, the lexer needs to be switched to use grapheme clusters
#[test]
fn unicode_identifier_lexes() {
    assert_eq!(
        Ok((
           vec![
                tok(Identifier(String::from("föö")), (0, 0), (0, 3)),
            ],
            "",
            3
        )),
        tokenize("föö")
    );
}

#[test]
fn operators_lex() {
    assert_eq!(
        Ok((
           vec![
                tok(Operator(Addition), (0, 0), (0, 1)),
                tok(Operator(Substraction), (0, 1), (0, 2)),
                tok(Operator(Multiplication), (0, 2), (0, 3)),
                tok(Operator(Division), (0, 3), (0, 4)),
                tok(Operator(And), (0, 4), (0, 5)),
                tok(Operator(Not), (0, 5), (0, 6)),
                tok(Operator(Equality), (0, 6), (0, 7)),
                tok(Operator(LessThan), (0, 7), (0, 8)),
            ],
            "",
            8
        )),
        tokenize("+-*/&!=<")
    );
}

#[test]
#[ignore] // TODO: To fix this, the lexer needs to be switched to use grapheme clusters
fn punctuation_lexes() {
    assert_eq!(
        Ok((
           vec![
                tok(Punctuation(Parenthesis(Open)), (0, 0), (0, 1)),
                tok(Punctuation(Semicolon), (0, 1), (0, 2)),
                tok(Punctuation(Parenthesis(Close)), (0, 2), (0, 3)),
            ],
            "§",
            3
        )),
        tokenize("(;)§")
    );
}

#[test]
fn example1_lexes() {
    let string = r#"
        var X : int := 4 + (6 * 2);
        print X;
    "#;
    assert_eq!(
        Ok((
           vec![
                tok(Keyword(Var), (1, 8), (1, 11)),
                tok(Identifier(String::from("X")), (1, 12), (1, 13)),
                tok(Punctuation(Colon), (1, 14), (1, 15)),
                tok(Keyword(Int), (1, 16), (1, 19)),
                tok(Operator(Assignment), (1, 20), (1, 22)),
                tok(Literal(Integer(4.into())), (1, 23), (1, 24)),
                tok(Operator(Addition), (1, 25), (1, 26)),
                tok(Punctuation(Parenthesis(Open)), (1, 27), (1, 28)),
                tok(Literal(Integer(6.into())), (1, 28), (1, 29)),
                tok(Operator(Multiplication), (1, 30), (1, 31)),
                tok(Literal(Integer(2.into())), (1, 32), (1, 33)),
                tok(Punctuation(Parenthesis(Close)), (1, 33), (1, 34)),
                tok(Punctuation(Semicolon), (1, 34), (1, 35)),
                tok(Keyword(Print), (2, 8), (2, 13)),
                tok(Identifier(String::from("X")), (2, 14), (2, 15)),
                tok(Punctuation(Semicolon), (2, 15), (2, 16)),
            ],
            "",
            string.len() - 3 // TODO: Why have to subtract amount of linebreaks?
        )),
        tokenize(string)
    );
}

#[test]
fn example2_lexes() {
    let string = r#"
        var nTimes : int := 0;
        print "How many times?"; 
        read nTimes; 
        var x : int;
        for x in 0..nTimes-1 do 
            print x;
            print " : Hello, World!\n";
        end for;
        assert (x = nTimes);
    "#;
    assert_eq!(
        Ok((
           vec![
                tok(Keyword(Var), (1, 8), (1, 11)),
                tok(Identifier(String::from("nTimes")), (1, 12), (1, 18)),
                tok(Punctuation(Colon), (1, 19), (1, 20)),
                tok(Keyword(Int), (1, 21), (1, 24)),
                tok(Operator(Assignment), (1, 25), (1, 27)),
                tok(Literal(Integer(0.into())), (1, 28), (1, 29)),
                tok(Punctuation(Semicolon), (1, 29), (1, 30)),
                tok(Keyword(Print), (2, 8), (2, 13)),
                tok(Literal(StringLit(String::from("How many times?"))), (2, 14), (2, 31)),
                tok(Punctuation(Semicolon), (2, 31), (2, 32)),
                tok(Keyword(Read), (3, 8), (3, 12)),
                tok(Identifier(String::from("nTimes")), (3, 13), (3, 19)),
                tok(Punctuation(Semicolon), (3, 19), (3, 20)),
                tok(Keyword(Var), (4, 8), (4, 11)),
                tok(Identifier(String::from("x")), (4, 12), (4, 13)),
                tok(Punctuation(Colon), (4, 14), (4, 15)),
                tok(Keyword(Int), (4, 16), (4, 19)),
                tok(Punctuation(Semicolon), (4, 19), (4, 20)),
                tok(Keyword(For), (5, 8), (5, 11)),
                tok(Identifier(String::from("x")), (5, 12), (5, 13)),
                tok(Keyword(In), (5, 14), (5, 16)),
                tok(Literal(Integer(0.into())), (5, 17), (5, 18)),
                tok(Operator(Range), (5, 18), (5, 20)),
                tok(Identifier(String::from("nTimes")), (5, 20), (5, 26)),
                tok(Operator(Substraction), (5, 26), (5, 27)),
                tok(Literal(Integer(1.into())), (5, 27), (5, 28)),
                tok(Keyword(Do), (5, 29), (5, 31)),
                tok(Keyword(Print), (6, 12), (6, 17)),
                tok(Identifier(String::from("x")), (6, 18), (6, 19)),
                tok(Punctuation(Semicolon), (6, 19), (6, 20)),
                tok(Keyword(Print), (7, 12), (7, 17)),
                tok(Literal(StringLit(String::from(" : Hello, World!\n"))), (7, 18), (7, 38)),
                tok(Punctuation(Semicolon), (7, 38), (7, 39)),
                tok(Keyword(End), (8, 8), (8, 11)),
                tok(Keyword(For), (8, 12), (8, 15)),
                tok(Punctuation(Semicolon), (8, 15), (8, 16)),
                tok(Keyword(Assert), (9, 8), (9, 14)),
                tok(Punctuation(Parenthesis(Open)), (9, 15), (9, 16)),
                tok(Identifier(String::from("x")), (9, 16), (9, 17)),
                tok(Operator(Equality), (9, 18), (9, 19)),
                tok(Identifier(String::from("nTimes")), (9, 20), (9, 26)),
                tok(Punctuation(Parenthesis(Close)), (9, 26), (9, 27)),
                tok(Punctuation(Semicolon), (9, 27), (9, 28)),
            ],
            "",
            string.len() - 10 // TODO: Why have to subtract amount of linebreaks?
        )),
        tokenize(string)
    );
}

#[test]
fn example3_lexes() {
    let string = r#"
        print "Give a number"; 
        var n : int;
        read n;
        var v : int := 1;
        var i : int;
        for i in 1..n do 
            v := v * i;
        end for;
        print "The result is: ";
        print v; 
    "#;
    assert_eq!(
        Ok((
           vec![
                tok(Keyword(Print), (1, 8), (1, 13)),
                tok(Literal(StringLit(String::from("Give a number"))), (1, 14), (1, 29)),
                tok(Punctuation(Semicolon), (1, 29), (1, 30)),
                tok(Keyword(Var), (2, 8), (2, 11)),
                tok(Identifier(String::from("n")), (2, 12), (2, 13)),
                tok(Punctuation(Colon), (2, 14), (2, 15)),
                tok(Keyword(Int), (2, 16), (2, 19)),
                tok(Punctuation(Semicolon), (2, 19), (2, 20)),
                tok(Keyword(Read), (3, 8), (3, 12)),
                tok(Identifier(String::from("n")), (3, 13), (3, 14)),
                tok(Punctuation(Semicolon), (3, 14), (3, 15)),
                tok(Keyword(Var), (4, 8), (4, 11)),
                tok(Identifier(String::from("v")), (4, 12), (4, 13)),
                tok(Punctuation(Colon), (4, 14), (4, 15)),
                tok(Keyword(Int), (4, 16), (4, 19)),
                tok(Operator(Assignment), (4, 20), (4, 22)),
                tok(Literal(Integer(1.into())), (4, 23), (4, 24)),
                tok(Punctuation(Semicolon), (4, 24), (4, 25)),
                tok(Keyword(Var), (5, 8), (5, 11)),
                tok(Identifier(String::from("i")), (5, 12), (5, 13)),
                tok(Punctuation(Colon), (5, 14), (5, 15)),
                tok(Keyword(Int), (5, 16), (5, 19)),
                tok(Punctuation(Semicolon), (5, 19), (5, 20)),
                tok(Keyword(For), (6, 8), (6, 11)),
                tok(Identifier(String::from("i")), (6, 12), (6, 13)),
                tok(Keyword(In), (6, 14), (6, 16)),
                tok(Literal(Integer(1.into())), (6, 17), (6, 18)),
                tok(Operator(Range), (6, 18), (6, 20)),
                tok(Identifier(String::from("n")), (6, 20), (6, 21)),
                tok(Keyword(Do), (6, 22), (6, 24)),
                tok(Identifier(String::from("v")), (7, 12), (7, 13)),
                tok(Operator(Assignment), (7, 14), (7, 16)),
                tok(Identifier(String::from("v")), (7, 17), (7, 18)),
                tok(Operator(Multiplication), (7, 19), (7, 20)),
                tok(Identifier(String::from("i")), (7, 21), (7, 22)),
                tok(Punctuation(Semicolon), (7, 22), (7, 23)),
                tok(Keyword(End), (8, 8), (8, 11)),
                tok(Keyword(For), (8, 12), (8, 15)),
                tok(Punctuation(Semicolon), (8, 15), (8, 16)),
                tok(Keyword(Print), (9, 8), (9, 13)),
                tok(Literal(StringLit(String::from("The result is: "))), (9, 14), (9, 31)),
                tok(Punctuation(Semicolon), (9, 31), (9, 32)),
                tok(Keyword(Print), (10, 8), (10, 13)),
                tok(Identifier(String::from("v")), (10, 14), (10, 15)),
                tok(Punctuation(Semicolon), (10, 15), (10, 16)),
            ],
            "",
            string.len() - 11 // TODO: Why have to subtract amount of linebreaks?
        )),
        tokenize(string)
    );
}