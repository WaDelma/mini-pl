extern crate parsco;

use parsco::{Parser, tag, many0, alt, fun, terminated, delimited, take_while, ws};

use self::Punctuation::*;
use self::Side::*;
use self::Keyword::*;
use self::Operator::*;
use self::Literal::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    Literal(Literal),
    Punctuation(Punctuation),
    Keyword(Keyword),
    Operator(Operator),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Integer(i64),
    StringLit(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operator {
    Assignment,
    Equality,
    Addition,
    Substraction,
    Multiplication,
    Range,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Punctuation {
    Semicolon,
    Colon,
    Parenthesis(Side),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Side {
    Open,
    Close
}

#[derive(Clone, Debug, PartialEq)]
pub enum Keyword {
    Var,
    For,
    End,
    In,
    Do,
    Read,
    Print,
    Int,
    Str,
    Bool,
    Assert,
}

pub fn lex(s: &str) -> Option<(Vec<Token>, &str)> {
    terminated(many0(ws(
        alt()
            | fun(operator)
            | fun(punctuation)
            | fun(keyword)
            | fun(identifier)
            | fun(integer)
            | fun(str_literal)
    )), ws(())).parse(s)
}

fn operator(s: &str) -> Option<(Token, &str)> {
    (alt()
        | tag("+", Addition)
        | tag("-", Substraction)
        | tag("*", Multiplication)
        | tag(":=", Assignment)
        | tag("=", Equality)
        | tag("..", Range)
    ).parse(s)
        .map(|(p, s)| (Token::Operator(p), s))
}

fn punctuation(s: &str) -> Option<(Token, &str)> {
    (alt()
        | tag(";", Semicolon)
        | tag(":", Colon)
        | tag("(", Parenthesis(Open))
        | tag(")", Parenthesis(Close))
    ).parse(s)
        .map(|(p, s)| (Token::Punctuation(p), s))
}

fn keyword(s: &str) -> Option<(Token, &str)> {
    (alt()
        | tag("var", Var)
        | tag("for", For)
        | tag("end", End)
        | tag("do", Do)
        | tag("read", Read)
        | tag("print", Print)
        | tag("int", Int)
        | tag("in", In)
        | tag("string", Str)
        | tag("bool", Bool)
        | tag("assert", Assert)
    ).parse(s)
        .map(|(p, s)| (Token::Keyword(p), s))
}

fn identifier(s: &str) -> Option<(Token, &str)> {
    take_while(char::is_alphabetic)
        .parse(s)
        .map(|(p, s)| (Token::Identifier(p), s))
}

fn str_literal(s: &str) -> Option<(Token, &str)> {
    delimited(
        tag("\"", ()),
        take_while(|c| c != '"'),
        tag("\"", ())
    )
        .parse(s)
        .map(|(p, s)| (Token::Literal(StringLit(p)), s))
}

fn integer(s: &str) -> Option<(Token, &str)> {
    take_while(|c| char::is_digit(c, 10))
        .parse(s)
        .map(|(p, s)| (Token::Literal(Integer(p.parse::<i64>().unwrap())), s))
}

#[test]
fn example1_lexes() {
    use Token::*;
    assert_eq!(
        Some((
            vec![
                Keyword(Var),
                Identifier(String::from("X")),
                Punctuation(Colon),
                Keyword(Int),
                Operator(Assignment),
                Literal(Integer(4)),
                Operator(Addition),
                Punctuation(Parenthesis(Open)),
                Literal(Integer(6)),
                Operator(Multiplication),
                Literal(Integer(2)),
                Punctuation(Parenthesis(Close)),
                Punctuation(Semicolon),
                Keyword(Print),
                Identifier(String::from("X")),
                Punctuation(Semicolon),
            ],
            ""
        )),
        lex("
            var X : int := 4 + (6 * 2);
            print X;
        ")
    );
}

#[test]
fn example2_lexes() {
    use Token::*;
    assert_eq!(
        Some((
            vec![
                Keyword(Var),
                Identifier(String::from("nTimes")),
                Punctuation(Colon),
                Keyword(Int),
                Operator(Assignment),
                Literal(Integer(0)),
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
                Literal(Integer(0)),
                Operator(Range),
                Identifier(String::from("nTimes")),
                Operator(Substraction),
                Literal(Integer(1)),
                Keyword(Do),
                Keyword(Print),
                Identifier(String::from("x")),
                Punctuation(Semicolon),
                Keyword(Print),
                Literal(StringLit(String::from(" : Hello, World!\\n"))),
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
        lex(r#"
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
    use Token::*;
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
                Literal(Integer(1)),
                Punctuation(Semicolon),
                Keyword(Var),
                Identifier(String::from("i")),
                Punctuation(Colon),
                Keyword(Int),
                Punctuation(Semicolon),
                Keyword(For),
                Identifier(String::from("i")),
                Keyword(In),
                Literal(Integer(1)),
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
        lex(r#"
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

#[test]
fn punctuation_lexes() {
    use Token::*;
    assert_eq!(
        Some((
            vec![
                Punctuation(Parenthesis(Open)),
                Punctuation(Semicolon),
                Punctuation(Parenthesis(Close)),
            ],
            "§"
        )),
        lex("(;)§")
    );
}

/*
 <prog>   ::=  <stmts>
 <stmts>  ::=  <stmt> ";" ( <stmt> ";" )*
 <stmt>   ::=  "var" <var_ident> ":" <type> [ ":=" <expr> ] 
           |   <var_ident> ":=" <expr>  
           |   "for" <var_ident> "in" <expr> ".." <expr> "do" 
                  <stmts> "end" "for"  
           |   "read" <var_ident>  
           |   "print" <expr>  
           |   "assert" "(" <expr> ")"

 <expr>   ::=  <opnd> <op> <opnd>
           |   [ <unary_op> ] <opnd>
		   
 <opnd>   ::=  <int>
           |   <string>
           |   <var_ident>
           |   "(" expr ")"
              
 <type>   ::=  "int" | "string" | "bool"
 <var_ident> ::= <ident>
 
 <reserved keyword> ::= 
              "var" | "for" | "end" | "in" | "do" | "read" | 
              "print" | "int" | "string" | "bool" | "assert"
*/