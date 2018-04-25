use llvm_sys::core::*;
use llvm_sys::{LLVMModule, LLVMIntPredicate, LLVMBuilder};
use llvm_sys::transforms::pass_manager_builder::*;
use llvm_sys::prelude::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;

use std::os::raw::{c_ulonglong, c_uint};
use std::ffi::{CString, CStr};
use std::ptr::null_mut;
use std::str;

use std::collections::HashMap;
use std::num::Wrapping;