use super::tokens::Token::*;
use super::tokens::Punctuation::*;
use super::tokens::Side::*;
use super::tokens::Keyword::*;
use super::tokens::Operator::*;
use super::tokens::Literal::*;
use super::tokenize;

#[test]
fn string_literal_lexes() {
    assert_eq!(
        Some((
            vec![
                Literal(StringLit(String::from("Hello, World!"))),
            ],
            ""
        )),
        tokenize(r#""Hello, World!""#)
    );
}

#[test]
fn string_literal_with_escaped_quote_lexes() {
    assert_eq!(
        Some((
            vec![
                Literal(StringLit(String::from("Hello, \"World!\""))),
            ],
            ""
        )),
        tokenize(r#""Hello, \"World!\"""#)
    );
}

#[test]
fn string_literal_with_escaped_linebreak_lexes() {
    assert_eq!(
        Some((
            vec![
                Literal(StringLit(String::from("Hello\n World!"))),
            ],
            ""
        )),
        tokenize(r#""Hello\n World!""#)
    );
}

#[test]
fn string_literal_with_escaped_tab_lexes() {
    assert_eq!(
        Some((
            vec![
                Literal(StringLit(String::from("Hello\t World!"))),
            ],
            ""
        )),
        tokenize(r#""Hello\t World!""#)
    );
}

#[test]
fn string_literal_with_escaped_backslash_lexes() {
    assert_eq!(
        Some((
            vec![
                Literal(StringLit(String::from("Hello\\ World!"))),
            ],
            ""
        )),
        tokenize(r#""Hello\\ World!""#)
    );
}

#[test]
fn string_literal_with_alert_lexes() {
    assert_eq!(
        Some((
            vec![
                Literal(StringLit(String::from("Hello, World\x07"))),
            ],
            ""
        )),
        tokenize(r#""Hello, World\a""#)
    );
}

#[test]
fn string_literal_with_backspace_lexes() {
    assert_eq!(
        Some((
            vec![
                Literal(StringLit(String::from("Hello,\x08 World!"))),
            ],
            ""
        )),
        tokenize(r#""Hello,\b World!""#)
    );
}

#[test]
fn string_literal_with_formfeed_lexes() {
    assert_eq!(
        Some((
            vec![
                Literal(StringLit(String::from("Hello, World!\x0C"))),
            ],
            ""
        )),
        tokenize(r#""Hello, World!\f""#)
    );
}

#[test]
fn string_literal_with_vertical_tab_lexes() {
    assert_eq!(
        Some((
            vec![
                Literal(StringLit(String::from("Hello\x0BWorld!"))),
            ],
            ""
        )),
        tokenize(r#""Hello\vWorld!""#)
    );
}

#[test]
fn line_comment_lexes() {
    assert_eq!(
        Some((
            vec![
                Identifier(String::from("x")),
            ],
            ""
        )),
        tokenize("x//y")
    );
}

#[test]
fn multiline_comment_simple_lexes() {
    assert_eq!(
        Some(
            (vec![
                Identifier(String::from("x")),
            ],
            ""
        )),
        tokenize("x/*y*/")
    );
}

#[test]
fn multiline_comment_star_lexes() {
    assert_eq!(
        Some(
            (vec![
                Identifier(String::from("x")),
            ],
            ""
        )),
        tokenize("x/**y*/")
    );
}

#[test]
fn multiline_comment_nested_lexes() {
    assert_eq!(
        Some((
            vec![
                Identifier(String::from("x")),
            ],
            ""
        )),
        tokenize("x/*/*y*/*/")
    );
}

#[test]
fn multiline_comment_complex_nesting_lexes() {
    assert_eq!(
        Some((
            vec![
                Identifier(String::from("x")),
            ],
            ""
        )),
        tokenize("x/*/*y*/z/*w*/*/")
    );
}

#[test]
fn identifiers_lex() {
    assert_eq!(
        Some((
            vec![
                Identifier(String::from("x")),
            ],
            ""
        )),
        tokenize("x")
    );
    assert_eq!(
        Some((
            vec![
                Identifier(String::from("föö")),
            ],
            ""
        )),
        tokenize("föö")
    );
    assert_eq!(
        Some((
            vec![
                Identifier(String::from("l33t")),
            ],
            ""
        )),
        tokenize("l33t")
    );
    assert_eq!(
        Some((
            vec![
                Identifier(String::from("l_l")),
            ],
            ""
        )),
        tokenize("l_l")
    );
}

#[test]
fn operators_lex() {
    assert_eq!(
        Some((
            vec![
                Operator(Addition),
                Operator(Substraction),
                Operator(Multiplication),
                Operator(Division),
                Operator(And),
                Operator(Not),
                Operator(Equality),
                Operator(LessThan),
            ],
            ""
        )),
        tokenize("+-*/&!=<")
    );
}

#[test]
fn punctuation_lexes() {
    assert_eq!(
        Some((
            vec![
                Punctuation(Parenthesis(Open)),
                Punctuation(Semicolon),
                Punctuation(Parenthesis(Close)),
            ],
            "§"
        )),
        tokenize("(;)§")
    );
}

#[test]
fn example1_lexes() {
    assert_eq!(
        Some((
            vec![
                Keyword(Var),
                Identifier(String::from("X")),
                Punctuation(Colon),
                Keyword(Int),
                Operator(Assignment),
                Literal(Integer(4.into())),
                Operator(Addition),
                Punctuation(Parenthesis(Open)),
                Literal(Integer(6.into())),
                Operator(Multiplication),
                Literal(Integer(2.into())),
                Punctuation(Parenthesis(Close)),
                Punctuation(Semicolon),
                Keyword(Print),
                Identifier(String::from("X")),
                Punctuation(Semicolon),
            ],
            ""
        )),
        tokenize("
            var X : int := 4 + (6 * 2);
            print X;
        ")
    );
}

#[test]
fn example2_lexes() {
    assert_eq!(
        Some((
            vec![
                Keyword(Var),
                Identifier(String::from("nTimes")),
                Punctuation(Colon),
                Keyword(Int),
                Operator(Assignment),
                Literal(Integer(0.into())),
                Punctuation(Semicolon),
                Keyword(Print),
                Literal(StringLit(String::from("How many times?"))),
                Punctuation(Semicolon),
                Keyword(Read),
                Identifier(String::from("nTimes")),
                Punctuation(Semicolon),
                Keyword(Var),
                Identifier(String::from("x")),
                Punctuation(Colon),
                Keyword(Int),
                Punctuation(Semicolon),
                Keyword(For),
                Identifier(String::from("x")),
                Keyword(In),
                Literal(Integer(0.into())),
                Operator(Range),
                Identifier(String::from("nTimes")),
                Operator(Substraction),
                Literal(Integer(1.into())),
                Keyword(Do),
                Keyword(Print),
                Identifier(String::from("x")),
                Punctuation(Semicolon),
                Keyword(Print),
                Literal(StringLit(String::from(" : Hello, World!\n"))),
                Punctuation(Semicolon),
                Keyword(End),
                Keyword(For),
                Punctuation(Semicolon),
                Keyword(Assert),
                Punctuation(Parenthesis(Open)),
                Identifier(String::from("x")),
                Operator(Equality),
                Identifier(String::from("nTimes")),
                Punctuation(Parenthesis(Close)),
                Punctuation(Semicolon),
            ],
            ""
        )),
        tokenize(r#"
            var nTimes : int := 0;
            print "How many times?"; 
            read nTimes; 
            var x : int;
            for x in 0..nTimes-1 do 
                print x;
                print " : Hello, World!\n";
            end for;
            assert (x = nTimes);
        "#)
    );
}

#[test]
fn example3_lexes() {
    assert_eq!(
        Some((
            vec![
                Keyword(Print),
                Literal(StringLit(String::from("Give a number"))),
                Punctuation(Semicolon),
                Keyword(Var),
                Identifier(String::from("n")),
                Punctuation(Colon),
                Keyword(Int),
                Punctuation(Semicolon),
                Keyword(Read),
                Identifier(String::from("n")),
                Punctuation(Semicolon),
                Keyword(Var),
                Identifier(String::from("v")),
                Punctuation(Colon),
                Keyword(Int),
                Operator(Assignment),
                Literal(Integer(1.into())),
                Punctuation(Semicolon),
                Keyword(Var),
                Identifier(String::from("i")),
                Punctuation(Colon),
                Keyword(Int),
                Punctuation(Semicolon),
                Keyword(For),
                Identifier(String::from("i")),
                Keyword(In),
                Literal(Integer(1.into())),
                Operator(Range),
                Identifier(String::from("n")),
                Keyword(Do),
                Identifier(String::from("v")),
                Operator(Assignment),
                Identifier(String::from("v")),
                Operator(Multiplication),
                Identifier(String::from("i")),
                Punctuation(Semicolon),
                Keyword(End),
                Keyword(For),
                Punctuation(Semicolon),
                Keyword(Print),
                Literal(StringLit(String::from("The result is: "))),
                Punctuation(Semicolon),
                Keyword(Print),
                Identifier(String::from("v")),
                Punctuation(Semicolon),
            ],
            ""
        )),
        tokenize(r#"
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
        "#)
    );
}