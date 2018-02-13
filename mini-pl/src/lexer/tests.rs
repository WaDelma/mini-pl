use super::{Tok, Token, Position};
use super::tokens::Token::*;
use super::tokens::Punctuation::*;
use super::tokens::Side::*;
use super::tokens::Keyword::*;
use super::tokens::Operator::*;
use super::tokens::Literal::*;
use super::tokenize;

fn tok(token: Token, (from_line, from_column): (usize, usize), (to_line, to_column): (usize, usize)) -> Tok {
    Tok::new(token, Position::new(from_line, from_column), Position::new(to_line, to_column))
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
                tok(Keyword(Var), (0, 0), (0, 0)),
                tok(Identifier(String::from("X")), (0, 0), (0, 0)),
                tok(Punctuation(Colon), (0, 0), (0, 0)),
                tok(Keyword(Int), (0, 0), (0, 0)),
                tok(Operator(Assignment), (0, 0), (0, 0)),
                tok(Literal(Integer(4.into())), (0, 0), (0, 0)),
                tok(Operator(Addition), (0, 0), (0, 0)),
                tok(Punctuation(Parenthesis(Open)), (0, 0), (0, 0)),
                tok(Literal(Integer(6.into())), (0, 0), (0, 0)),
                tok(Operator(Multiplication), (0, 0), (0, 0)),
                tok(Literal(Integer(2.into())), (0, 0), (0, 0)),
                tok(Punctuation(Parenthesis(Close)), (0, 0), (0, 0)),
                tok(Punctuation(Semicolon), (0, 0), (0, 0)),
                tok(Keyword(Print), (0, 0), (0, 0)),
                tok(Identifier(String::from("X")), (0, 0), (0, 0)),
                tok(Punctuation(Semicolon), (0, 0), (0, 0)),
            ],
            "",
            string.len()
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
                tok(Keyword(Var), (0, 0), (0, 0)),
                tok(Identifier(String::from("nTimes")), (0, 0), (0, 0)),
                tok(Punctuation(Colon), (0, 0), (0, 0)),
                tok(Keyword(Int), (0, 0), (0, 0)),
                tok(Operator(Assignment), (0, 0), (0, 0)),
                tok(Literal(Integer(0.into())), (0, 0), (0, 0)),
                tok(Punctuation(Semicolon), (0, 0), (0, 0)),
                tok(Keyword(Print), (0, 0), (0, 0)),
                tok(Literal(StringLit(String::from("How many times?"))), (0, 0), (0, 0)),
                tok(Punctuation(Semicolon), (0, 0), (0, 0)),
                tok(Keyword(Read), (0, 0), (0, 0)),
                tok(Identifier(String::from("nTimes")), (0, 0), (0, 0)),
                tok(Punctuation(Semicolon), (0, 0), (0, 0)),
                tok(Keyword(Var), (0, 0), (0, 0)),
                tok(Identifier(String::from("x")), (0, 0), (0, 0)),
                tok(Punctuation(Colon), (0, 0), (0, 0)),
                tok(Keyword(Int), (0, 0), (0, 0)),
                tok(Punctuation(Semicolon), (0, 0), (0, 0)),
                tok(Keyword(For), (0, 0), (0, 0)),
                tok(Identifier(String::from("x")), (0, 0), (0, 0)),
                tok(Keyword(In), (0, 0), (0, 0)),
                tok(Literal(Integer(0.into())), (0, 0), (0, 0)),
                tok(Operator(Range), (0, 0), (0, 0)),
                tok(Identifier(String::from("nTimes")), (0, 0), (0, 0)),
                tok(Operator(Substraction), (0, 0), (0, 0)),
                tok(Literal(Integer(1.into())), (0, 0), (0, 0)),
                tok(Keyword(Do), (0, 0), (0, 0)),
                tok(Keyword(Print), (0, 0), (0, 0)),
                tok(Identifier(String::from("x")), (0, 0), (0, 0)),
                tok(Punctuation(Semicolon), (0, 0), (0, 0)),
                tok(Keyword(Print), (0, 0), (0, 0)),
                tok(Literal(StringLit(String::from(" : Hello, World!\n"))), (0, 0), (0, 0)),
                tok(Punctuation(Semicolon), (0, 0), (0, 0)),
                tok(Keyword(End), (0, 0), (0, 0)),
                tok(Keyword(For), (0, 0), (0, 0)),
                tok(Punctuation(Semicolon), (0, 0), (0, 0)),
                tok(Keyword(Assert), (0, 0), (0, 0)),
                tok(Punctuation(Parenthesis(Open)), (0, 0), (0, 0)),
                tok(Identifier(String::from("x")), (0, 0), (0, 0)),
                tok(Operator(Equality), (0, 0), (0, 0)),
                tok(Identifier(String::from("nTimes")), (0, 0), (0, 0)),
                tok(Punctuation(Parenthesis(Close)), (0, 0), (0, 0)),
                tok(Punctuation(Semicolon), (0, 0), (0, 0)),
            ],
            "",
            string.len()
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
                tok(Keyword(Print), (0, 0), (0, 0)),
                tok(Literal(StringLit(String::from("Give a number"))), (0, 0), (0, 0)),
                tok(Punctuation(Semicolon), (0, 0), (0, 0)),
                tok(Keyword(Var), (0, 0), (0, 0)),
                tok(Identifier(String::from("n")), (0, 0), (0, 0)),
                tok(Punctuation(Colon), (0, 0), (0, 0)),
                tok(Keyword(Int), (0, 0), (0, 0)),
                tok(Punctuation(Semicolon), (0, 0), (0, 0)),
                tok(Keyword(Read), (0, 0), (0, 0)),
                tok(Identifier(String::from("n")), (0, 0), (0, 0)),
                tok(Punctuation(Semicolon), (0, 0), (0, 0)),
                tok(Keyword(Var), (0, 0), (0, 0)),
                tok(Identifier(String::from("v")), (0, 0), (0, 0)),
                tok(Punctuation(Colon), (0, 0), (0, 0)),
                tok(Keyword(Int), (0, 0), (0, 0)),
                tok(Operator(Assignment), (0, 0), (0, 0)),
                tok(Literal(Integer(1.into())), (0, 0), (0, 0)),
                tok(Punctuation(Semicolon), (0, 0), (0, 0)),
                tok(Keyword(Var), (0, 0), (0, 0)),
                tok(Identifier(String::from("i")), (0, 0), (0, 0)),
                tok(Punctuation(Colon), (0, 0), (0, 0)),
                tok(Keyword(Int), (0, 0), (0, 0)),
                tok(Punctuation(Semicolon), (0, 0), (0, 0)),
                tok(Keyword(For), (0, 0), (0, 0)),
                tok(Identifier(String::from("i")), (0, 0), (0, 0)),
                tok(Keyword(In), (0, 0), (0, 0)),
                tok(Literal(Integer(1.into())), (0, 0), (0, 0)),
                tok(Operator(Range), (0, 0), (0, 0)),
                tok(Identifier(String::from("n")), (0, 0), (0, 0)),
                tok(Keyword(Do), (0, 0), (0, 0)),
                tok(Identifier(String::from("v")), (0, 0), (0, 0)),
                tok(Operator(Assignment), (0, 0), (0, 0)),
                tok(Identifier(String::from("v")), (0, 0), (0, 0)),
                tok(Operator(Multiplication), (0, 0), (0, 0)),
                tok(Identifier(String::from("i")), (0, 0), (0, 0)),
                tok(Punctuation(Semicolon), (0, 0), (0, 0)),
                tok(Keyword(End), (0, 0), (0, 0)),
                tok(Keyword(For), (0, 0), (0, 0)),
                tok(Punctuation(Semicolon), (0, 0), (0, 0)),
                tok(Keyword(Print), (0, 0), (0, 0)),
                tok(Literal(StringLit(String::from("The result is: "))), (0, 0), (0, 0)),
                tok(Punctuation(Semicolon), (0, 0), (0, 0)),
                tok(Keyword(Print), (0, 0), (0, 0)),
                tok(Identifier(String::from("v")), (0, 0), (0, 0)),
                tok(Punctuation(Semicolon), (0, 0), (0, 0)),
            ],
            "",
            string.len()
        )),
        tokenize(string)
    );
}