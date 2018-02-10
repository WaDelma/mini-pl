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
        Ok((
            vec![
                Literal(StringLit(String::from("Hello, World!"))),
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
                Literal(StringLit(String::from("Hello, \"World!\""))),
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
                Literal(StringLit(String::from("Hello\n World!"))),
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
                Literal(StringLit(String::from("Hello\t World!"))),
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
                Literal(StringLit(String::from("Hello\\ World!"))),
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
                Literal(StringLit(String::from("Hello, World\x07"))),
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
                Literal(StringLit(String::from("Hello,\x08 World!"))),
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
                Literal(StringLit(String::from("Hello, World!\x0C"))),
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
                Literal(StringLit(String::from("Hello\x0BWorld!"))),
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
                Literal(StringLit(String::from("\x1B\x07\x0C"))),
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
                Literal(StringLit(String::from("👌 🤔 😽 ⸙ 𝝅 ≪ 𝝉 ⸎"))),
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
                Literal(StringLit(String::from_utf8(vec![0o0, 0o10, 0o100, 0o2, 0o12, 0o102]).unwrap())),
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
                Identifier(String::from("x")),
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
                Identifier(String::from("x")),
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
                Identifier(String::from("x")),
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
                Identifier(String::from("x")),
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
                Identifier(String::from("x")),
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
                Identifier(String::from("x")),
            ],
            "",
            1
        )),
        tokenize("x")
    );
    assert_eq!(
        Ok((
            vec![
                Identifier(String::from("l33t")),
            ],
            "",
            4
        )),
        tokenize("l33t")
    );
    assert_eq!(
        Ok((
            vec![
                Identifier(String::from("l_l")),
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
                Identifier(String::from("foray")),
                Identifier(String::from("into")),
                Identifier(String::from("reading")),
                Identifier(String::from("independent")),
                Identifier(String::from("dodo")),
                Identifier(String::from("endofunctors")),
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
                Identifier(String::from("föö")),
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
                Operator(Addition),
                Operator(Substraction),
                Operator(Multiplication),
                Operator(Division),
                Operator(And),
                Operator(Not),
                Operator(Equality),
                Operator(LessThan),
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
                Punctuation(Parenthesis(Open)),
                Punctuation(Semicolon),
                Punctuation(Parenthesis(Close)),
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
            "",
            string.len()
        )),
        tokenize(string)
    );
}