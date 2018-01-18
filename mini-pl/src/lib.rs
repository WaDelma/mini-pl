extern crate parsco;

use parsco::{Parser, tag, many0, alt, fun};

use self::Punctuation::*;
use self::Side::*;
use self::Keyword::*;
use self::Operator::*;

#[derive(Clone, Debug, PartialEq)]
enum Token {
   Punctuation(Punctuation),
   Keyword(Keyword),
   Operator(Operator),
}

#[derive(Clone, Debug, PartialEq)]
enum Operator {
    Assignment,
    Range,
}

#[derive(Clone, Debug, PartialEq)]
enum Punctuation {
    Semicolon,
    Parenthesis(Side),
}

#[derive(Clone, Debug, PartialEq)]
enum Side {
    Open,
    Close
}

#[derive(Clone, Debug, PartialEq)]
enum Keyword {
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

fn lex(s: &str) -> Option<(Vec<Token>, &str)> {
    many0(
        alt()
            | fun(operator)
            | fun(punctuation)
            | fun(keyword)
    ).parse(s)
}

fn operator(s: &str) -> Option<(Token, &str)> {
    (alt()
        | tag(":=", Assignment)
        | tag("..", Range)
    ).parse(s)
        .map(|(p, s)| (Token::Operator(p), s))
}

fn punctuation(s: &str) -> Option<(Token, &str)> {
    (alt()
        | tag(";", Semicolon)
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
        | tag("in", In)
        | tag("do", Do)
        | tag("read", Read)
        | tag("print", Print)
        | tag("int", Int)
        | tag("string", Str)
        | tag("bool", Bool)
        | tag("assert", Assert)
    ).parse(s)
        .map(|(p, s)| (Token::Keyword(p), s))
}

#[test]
fn punctuation_lexes() {
    assert_eq!(
        Some((
            vec![
                Token::Punctuation(Parenthesis(Open)),
                Token::Punctuation(Semicolon),
                Token::Punctuation(Parenthesis(Close)),
            ],
            "a"
        )),
        lex("(;)a")
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