use llvm_sys::core::*;
use llvm_sys::{LLVMModule, LLVMIntPredicate, LLVMBuilder, LLVMTypeKind, LLVMValueKind};
use llvm_sys::ir_reader::LLVMParseIRInContext;
use llvm_sys::transforms::pass_manager_builder::*;
use llvm_sys::prelude::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;

use std::ffi::{CString, CStr};
use std::marker::PhantomData;
use std::mem::transmute;

const LLVM_TRUE: LLVMBool = 1;
const LLVM_FALSE: LLVMBool = 0;

#[repr(C)]
pub struct Context(pub LLVMContextRef);

impl Context {
    pub fn new() -> Self {
        unsafe {
            Context(LLVMContextCreate())
        }
    }

    pub fn module<'a, 'b>(&'a self, name: &'b CStr) -> Module<'a, 'b> {
        unsafe {
            Module(
                LLVMModuleCreateWithNameInContext(name.as_ptr(), self.0),
                PhantomData
            )
        }
    }

    pub fn builder_at_end(&self, bb: &BasicBlock) -> Builder {
        unsafe {
            let builder = Builder(
                LLVMCreateBuilderInContext(self.0),
                PhantomData
            );
            LLVMPositionBuilderAtEnd(builder.0, bb.0);
            builder
        }
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe {
            LLVMContextDispose(self.0);
        }
    }
}

#[repr(C)]
pub struct Module<'a, 'b>(pub LLVMModuleRef, PhantomData<(&'a CStr, &'b Context)>);

impl<'a, 'b> Module<'a, 'b> {
    pub fn add_function<'c: 'a>(&mut self, name: &'c CStr, function_type: Type) -> Value<'c> {
        unsafe {
            assert_eq!(LLVMTypeKind::LLVMFunctionTypeKind, LLVMGetTypeKind(function_type.0));
            Value(
                LLVMAddFunction(self.0, name.as_ptr(), function_type.0),
                PhantomData
            )
        }
    }

    pub fn dump(&self) {
        unsafe {
            LLVMDumpModule(self.0);
        }
    }
}

impl<'a, 'b> Drop for Module<'a, 'b> {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeModule(self.0);
        }
    }
}

#[repr(C)]
pub struct Builder<'a>(pub LLVMBuilderRef, PhantomData<&'a Context>);

impl<'a> Builder<'a> {
    pub fn ret_void(&mut self) -> &mut Self {
        unsafe {
            LLVMBuildRetVoid(self.0);
        }
        self
    }
}

impl<'a> Drop for Builder<'a> {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.0);
        }
    }
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct Type<'a>(pub LLVMTypeRef, PhantomData<&'a Context>);

impl<'a> Type<'a> {
    pub fn void(ctx: &'a Context) -> Self {
        unsafe {
            Type(
                LLVMVoidTypeInContext(ctx.0),
                PhantomData
            )
        }
    }

    pub fn function(ret: Type<'a>, params: &'a [Type<'a>], varargs: bool) -> Self {
        assert!(params.len() < u32::max_value() as usize);
        unsafe {
            let params: &'a [LLVMTypeRef] = transmute(params);
            Type(
                LLVMFunctionType(ret.0, params as *const _ as *mut _, params.len() as u32, llvm_bool(varargs)),
                PhantomData
            )
        }
    }
}

#[repr(C)]
pub struct Value<'a>(pub LLVMValueRef, PhantomData<&'a CStr>);

impl<'a> Value<'a> {
    pub fn add_basic_block<'b, 'c>(&mut self, ctx: &'b Context, name: &'c CStr) -> BasicBlock<'b, 'c> {
        unsafe {
            assert_eq!(LLVMValueKind::LLVMFunctionValueKind, LLVMGetValueKind(self.0));
            BasicBlock(
                LLVMAppendBasicBlockInContext(ctx.0, self.0, name.as_ptr()),
                PhantomData
            )
        }
    }
}

#[repr(C)]
pub struct BasicBlock<'a, 'b>(pub LLVMBasicBlockRef, PhantomData<(&'a Context, &'b CStr)>);

fn llvm_bool(b: bool) -> LLVMBool {
    if b {
        LLVM_TRUE
    } else {
        LLVM_FALSE
    }
}