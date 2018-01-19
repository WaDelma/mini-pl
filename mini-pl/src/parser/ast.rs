
struct Stmts(Vec<Stmt>);

enum Stmt {
    Declaration {
        ident: String,
        ty: Type,
        value: Option<Expr>,
    },
    Assignment {
        ident: String,
        value: Expr,
    },
    Loop {
        ident: String,
        from: Expr,
        to: Expr,
        stmts: Stmts,
    },
    Read {
        ident: String,
    },
    Print {
        expr: Expr,
    },
    Assert {
        expr: Expr,
    }
}

enum Expr {
    BinOp {
        lhs: Opnd,
        op: Op,
        rhs: Opnd,
    },
}

enum Opnd {
    Int(i64),
    Str(String),
    Ident(String),
    Expr(Box<Expr>),
}

enum Op {
    Multiplication,
    Addition,
    Substraction,
}

enum Type {
    Integer,
    Str,
    Bool
}