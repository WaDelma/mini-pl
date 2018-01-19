use repr::Value;

mod context;
mod repr;
#[cfg(tests)]
mod tests;

pub fn interpret(stmts: &[Stmt], ctx: &mut Context<Value>) {
    for stmt in stmts {
        interpret_stmt(stmt, ctx);
    }
}

fn interpret_stmt(stmt: Stmt, ctx: &mut Context<Value>) {

}