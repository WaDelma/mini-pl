use util::{Positioned, Position};
use util::context::Context;
use lexer::tokenize;
use parser::parse;
use parser::ast::{BinOp, UnaOp};
use super::super::{AnalysisError, Type, Mutability, analyze};
use super::super::AnalysisError::*;

fn analyze_code(code: &str) -> (Vec<Positioned<AnalysisError>>, Context<(Type, Mutability)>) {
    let mut ctx = Context::new();
    let result = analyze(&parse(&tokenize(code).unwrap().0).unwrap().0, &mut ctx);
    (result, ctx)
}

#[test]
fn type_mismatch_bool_int() {
    assert_eq!(
        analyze_code(r#"
            var X: bool := 1;
        "#).0,
        vec![
            Positioned::new(
                TypeMismatch(Type::Bool, Type::Integer),
                Position::new(1, 12),
                Position::new(1, 29)
            )
        ]
    );
}

#[test]
fn type_mismatch_int_bool() {
    assert_eq!(
        analyze_code(r#"
            var X: int := 1 = 1;
        "#).0,
        vec![
            Positioned::new(
                TypeMismatch(Type::Integer, Type::Bool),
                Position::new(1, 12),
                Position::new(1, 32)
            )
        ]
    );
}

#[test]
fn type_mismatch_str_bool() {
    assert_eq!(
        analyze_code(r#"
            var X: string := 1 = 1;
        "#).0,
        vec![
            Positioned::new(
                TypeMismatch(Type::Str, Type::Bool),
                Position::new(1, 12),
                Position::new(1, 35)
            )
        ]
    );
}

#[test]
fn type_mismatch_bool_str() {
    assert_eq!(
        analyze_code(r#"
            var X: bool := "hai";
        "#).0,
        vec![
            Positioned::new(
                TypeMismatch(Type::Bool, Type::Str),
                Position::new(1, 12),
                Position::new(1, 33)
            )
        ]
    );
}

#[test]
fn type_mismatch_in_print() {
    assert_eq!(
        analyze_code(r#"
            print 5 = 5;
        "#).0,
        vec![
            Positioned::new(
                IOMismatch(Type::Bool),
                Position::new(1, 12),
                Position::new(1, 24)
            )
        ]
    );
}

#[test]
fn type_mismatch_in_read() {
    assert_eq!(
        analyze_code(r#"
            var x: bool;
            read x;
        "#).0,
        vec![
            Positioned::new(
                IOMismatch(Type::Bool),
                Position::new(2, 12),
                Position::new(2, 19)
            )
        ]
    );
}

#[test]
fn type_mismatch_in_expr() {
    assert_eq!(
        analyze_code(r#"
            print "hai" + 5;
        "#).0,
        vec![
            Positioned::new(
                TypeMismatch(Type::Str, Type::Integer),
                Position::new(1, 18),
                Position::new(1, 27)
            )
        ]
    );
}

#[test]
fn binary_operator_on_unsupported_type() {
    assert_eq!(
        analyze_code(r#"
            var x: bool := 1 = 1;
            print x + x;
        "#).0,
        vec![
            Positioned::new(
                UnableToBinOp(Type::Bool, BinOp::Addition),
                Position::new(2, 18),
                Position::new(2, 23)
            )
        ]
    );
}

#[test]
fn unary_operator_on_unsupported_type() {
    assert_eq!(
        analyze_code(r#"
            print !24;
        "#).0,
        vec![
            Positioned::new(
                UnableToUnaOp(Type::Integer, UnaOp::Not),
                Position::new(1, 18),
                Position::new(1, 21)
            )
        ]
    );
}

#[test]
fn recursive_variable_in_expr() {
    assert_eq!(
        analyze_code(r#"
            var x: int := x;
        "#).0,
        vec![
            Positioned::new(
                UnknownVariable("x".into()), // TODO: Separate error for recursive case?
                Position::new(1, 26),
                Position::new(1, 27)
            )
        ]
    );
}

#[test]
fn unknown_variable_in_expr() {
    assert_eq!(
        analyze_code(r#"
            var x: int := (5 + y) + 1;
        "#).0,
        vec![
            Positioned::new(
                UnknownVariable("y".into()),
                Position::new(1, 31),
                Position::new(1, 32)
            )
        ]
    );
}

#[test]
fn loop_in_loop() {
    assert_eq!(
        analyze_code(r#"
            var x: int;
            for x in 0..10 do
                for x in 0..10 do
                    print x;
                end for;
            end for;
        "#).0,
        vec![
            Positioned::new(
                MutationOfImmutable("x".into()),
                // TODO: More accurate error position
                Position::new(3, 16),
                Position::new(5, 24)
            )
        ]
    );
}


#[test]
fn dublicate_declaration() {
    assert_eq!(
        analyze_code(r#"
            var x: int;
            var x: int;
        "#).0,
        vec![
            Positioned::new(
                DublicateDeclaration("x".into()),
                Position::new(2, 12),
                Position::new(2, 23)
            )
        ]
    );
}