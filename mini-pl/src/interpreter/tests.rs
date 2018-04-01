use num_bigint::BigInt;

use lexer::tokenize;
use parser::parse;
use analyzer::analyze;
use util::context::Context;

use super::interpret;
use super::repr::TypedValue;
use super::repr::Value::*;

#[test]
fn example1_interprets() {
    let mut stdio = ("", vec![], vec![]);
    let mut analysis_ctx = Context::new();
    let mut ctx = Context::new();
    let ast = parse(&tokenize("
        var X : int := 4 + (6 * 2);
        print X;
    ").unwrap().0).unwrap().0;
    assert_eq!(
        analyze(&ast, &mut analysis_ctx),
        vec![]
    );
    interpret(&ast, &mut ctx, &mut stdio);
    assert_eq!(
        "16",
        &String::from_utf8(stdio.1).unwrap()
    );
    assert_eq!(
        "",
        &String::from_utf8(stdio.2).unwrap()
    );
    assert_eq!(
        Some(&TypedValue::from_value(Integer(16.into()))),
        ctx.get(&String::from("X"))
    );
}

#[test]
fn example2_interprets() {
    let mut stdio = ("3 ", vec![], vec![]);
    let mut analysis_ctx = Context::new();
    let mut ctx = Context::new();
    let ast = parse(&tokenize(r#"
        var nTimes : int := 0;
        print "How many times?"; 
        read nTimes; 
        var x : int;
        for x in 0..nTimes-1 do 
            print x;
            print " : Hello, World!\n";
        end for;
        assert (x = nTimes);
    "#).unwrap().0).unwrap().0;
    assert_eq!(
        analyze(&ast, &mut analysis_ctx),
        vec![]
    );
    interpret(&ast, &mut ctx, &mut stdio);
    assert_eq!(
        "How many times?0 : Hello, World!\n1 : Hello, World!\n2 : Hello, World!\n",
        &String::from_utf8(stdio.1).unwrap()
    );
    assert_eq!(
        "",
        &String::from_utf8(stdio.2).unwrap()
    );
    assert_eq!(
        Some(&TypedValue::from_value(Integer(3.into()))),
        ctx.get(&String::from("nTimes"))
    );
    assert_eq!(
        Some(&TypedValue::from_value(Integer(3.into()))),
        ctx.get(&String::from("x"))
    );
}

#[test]
fn example3_interprets() {
    let mut stdio = ("100 ", vec![], vec![]);
    let mut analysis_ctx = Context::new();
    let mut ctx = Context::new();
    let ast = parse(&tokenize(r#"
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
    "#).unwrap().0).unwrap().0;
    assert_eq!(
        analyze(&ast, &mut analysis_ctx),
        vec![]
    );
    interpret(&ast, &mut ctx, &mut stdio);
    assert_eq!(
        "Give a numberThe result is: 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000",
        &String::from_utf8(stdio.1).unwrap()
    );
    assert_eq!(
        "",
        &String::from_utf8(stdio.2).unwrap()
    );
    assert_eq!(
        Some(&TypedValue::from_value(Integer(100.into()))),
        ctx.get(&String::from("n"))
    );
    assert_eq!(
        Some(&TypedValue::from_value(Integer(101.into()))),
        ctx.get(&String::from("i"))
    );
    let mut n = BigInt::from(1);
    for i in 1..101 {
        n = n * BigInt::from(i);
    }
    assert_eq!(
        Some(&TypedValue::from_value(Integer(n))),
        ctx.get(&String::from("v"))
    );
}
