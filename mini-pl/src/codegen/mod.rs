#![allow(missing_docs)]

// use llvm_sys::core::*;
// use llvm_sys::{LLVMModule, LLVMIntPredicate, LLVMBuilder};
// use llvm_sys::ir_reader::LLVMParseIRInContext;
// use llvm_sys::transforms::pass_manager_builder::*;
// use llvm_sys::prelude::*;
// use llvm_sys::target::*;
// use llvm_sys::target_machine::*;

// use std::os::raw::{c_ulonglong, c_uint, c_char};
// use std::ffi::{CString, CStr};
// use std::ptr::null_mut;
// use std::str;
// use std::marker::PhantomData;
// use std::collections::HashMap;
// use std::num::Wrapping;

use parser::ast::{
    Program,
    Stmt,
    Type::{self, *},
    AccessBy::*,
};

// use self::llvm::*;

// mod llvm;

fn codegen(program: &Program, ctx: &mut ::util::context::Context<()>) -> String {
    let mut code = String::new();
    code.push_str(&format!("; ModuleID = '{}'\n", program.name.data));
    code.push_str("define i32 @main() {\n");
    add_code(&mut code, &program.stmts.data);
    code.push_str("ret i32 0\n");
    code.push_str("}\n");
    for f in program.functions.iter().map(|f| &f.data) {
        let result = f.result.as_ref()
            .map(|t| generate_type(&t.data))
            .unwrap_or("void");
        let params = f.params.iter()
            .map(|p| &p.data)
            .map(|p| format!(
                "{type}{access} {name}",
                type = generate_type(&p.ty.data),
                access = if let Reference = p.by {
                    "*"
                } else {
                    ""
                },
                name = p.name.data
            ))
            .collect::<Vec<_>>()
            .join(", ");
        code.push_str(&format!("define {result} @{name}({params}) {{\n", name = f.name.data, result = result, params = params));
        add_code(&mut code, &f.stmts.data);
        code.push_str("}\n");
    }
    code
}

fn add_code(code: &mut String, stmt: &Stmt) {
    use self::Stmt::*;
    match stmt {
        Declaration {
            idents,
            ty,
        } => {
            let ty = generate_type(ty);
            for ident in idents {
                code.push_str(&format!("%{name} = alloca {type}\n", name = ident.data, type = ty));
            }
        },
        Assignment {
            ident,
            value,
        } => {},
        Loop {
            cond,
            body,
        } => {},
        Block {
            stmts,
        } => {},
        Read {
            ident,
        } => {},
        Print {
            expr,
        } => {},
        Assert {
            expr,
        } => {},
        t => panic!("Error while trying to generate code for statement: {:?}", t),
    }
}

fn generate_type(ty: &Type) -> &'static str {
    match ty {
        Integer => "i32",
        Str => unimplemented!(),
        Real => unimplemented!(),
        Bool => "u8",
        t => panic!("Error while trying to generate code for type: {:?}", t),
    }
}

// #[test]
// fn codegen_test() {
//     let res = codegen(&::parser::parse(&::lexer::tokenize(r#"program p; begin
//             x := (
//                 (
//                     (
//                         4/2
//                     )+1
//                 )<(
//                     1-(
//                         2*3
//                     )
//                 )
//             )=(
//                 0 and (
//                     not 1
//                 )
//             ) end.
//         "#).unwrap().0).unwrap().0.data, &mut ::util::context::Context::new());
//     panic!("{}", res);
// }

    // let context = Context::new();
    // let program_name = CString::new(program.name.data.clone()).unwrap();
    // let mut module = context.module(&program_name);

    // for fun in &program.functions {
    //     let void_ty = Type::void(&context);
    //     let function_type = Type::function(void_ty, &[], false);

    //     // TODO: Hmm.. This shouldn't build: function name gets deallocated
    //     let function_name =  CString::new(fun.data.name.data.clone()).unwrap();
    //     let mut function = module.add_function(&function_name, function_type);
    //     let basic_block = function.add_basic_block(&context, cstr!("entry"));

    //     let mut builder = context.builder_at_end(&basic_block);
    //     builder.ret_void();
    // }

    // module.dump();
    
    // let hello_world = cstr!(r#"
    //     ; Declare the string constant as a global constant.
    //     @.str = private unnamed_addr constant [13 x i8] c"hello world\0A\00"

    //     ; External declaration of the puts function
    //     declare i32 @puts(i8* nocapture) nounwind

    //     ; Definition of main function
    //     define i32 @main() {   ; i32()*
    //         ; Convert [13 x i8]* to i8*...
    //         %cast210 = getelementptr [13 x i8], [13 x i8]* @.str, i64 0, i64 0

    //         ; Call puts function to write out the string to stdout.
    //         call i32 @puts(i8* %cast210)
    //         ret i32 0
    //     }

    //     ; Named metadata
    //     !0 = !{i32 42, null, !"string"}
    //     !foo = !{!0}
    // "#);
    
    // unsafe {
        // let code =  LLVMCreateMemoryBufferWithMemoryRange(
        //     hello_world.as_ptr(), 
        //     hello_world.to_bytes().len(), 
        //     cstr!("buffer").as_ptr(),
        //     LLVM_FALSE
        // );
        // let mut msg: *mut c_char = ::std::ptr::null_mut();
        // if LLVMParseIRInContext(llvm_context.0, code, &mut llvm_module.0 as *mut _, &mut msg as *mut _) != LLVM_FALSE {
        //     println!("{:?}", unsafe { CStr::from_ptr(msg) });
        //     LLVMDisposeMessage(msg);
        // } else {
        //     llvm_module.dump();
        // }
    // }
// }

