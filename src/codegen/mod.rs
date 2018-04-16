#![allow(warnings)]

pub mod generator;

use std::fmt::{
    Display,
    Formatter,
    Result as fmtResult,
};

#[derive(Debug, Hash, Clone)]
pub struct Label {
    name: String,
    id: usize,
}

impl Label {
    pub fn new(name: &str, id: usize) -> Label {
        Label { name: String::from(name), id }
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut Formatter) -> fmtResult {
        write!(f, "{}{}:", self.name, self.id)
    }
}

pub struct LabelMaker {
    id: usize,
}

impl LabelMaker {
    pub fn new() -> LabelMaker { LabelMaker { id: 0 } }

    pub fn make_named(&mut self, name: &str) -> Label {
        let id = self.id;
        self.id += 1;
        Label::new(name, id)
    }

    pub fn named_after(&mut self) -> Label {
        self.make_named("after")
    }

    pub fn named_test(&mut self) -> Label {
        self.make_named("test")
    }

    pub fn named_body(&mut self) -> Label {
        self.make_named("body")
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Hash, Clone)]
pub enum Bytecode {
    iload_0,
    iload_1,
    iload_2,
    iload_3,
    iload_x(u32),
    iconst_0,
    iconst_1,
    iconst_2,
    iconst_3,
    iconst_4,
    iconst_5,
    bipush_x(u32),
    istore_0,
    istore_1,
    istore_2,
    istore_3,
    istore_x(u32),
    ireturn,
    areturn,
    return_void,
    iadd,
    isub,
    imul,
    idiv,
    irem,
    ineg,
    iinc(u32, u32),
    ior,
    iand,
    ixor,
    ifeq(Label),
    iflt(Label),
    ifne(Label),
    ifgt(Label),
    ifge(Label),
    ifnull(Label),
    ifnonnull(Label),
    if_icmpeq(Label),
    if_icmplt(Label),
    if_icmpne(Label),
    if_icmpgt(Label),
    if_icmpge(Label),
    if_icmpnull(Label),
    if_icmpnonnull(Label),
    goto(Label),
    pop,
    dup,
    swap,
    ldc_str(String),
    aload_0,
    aload_1,
    aload_2,
    aload_3,
    aload_x(u64),
    iaload,
    iastore,
    getfield,
    putfield,
    getstatic,
    putstatic,
    newarray,
    anewarray,
    multianewarray,
    invokevirtual(String, String), // Class name, Method signature
    invokestatic(String, String),  // Class name, Function signature
    invokespecial(String), // Class name (calls constructor)
}

use self::Bytecode::*;

impl Display for Bytecode {
    fn fmt(&self, f: &mut Formatter) -> fmtResult {
        match *self {
            iload_0 => write!(f, "iload_0"),
            iload_1 => write!(f, "iload_1"),
            iload_2 => write!(f, "iload_2"),
            iload_3 => write!(f, "iload_3"),
            iload_x(x) => write!(f, "iload {}", x),
            iconst_0 => write!(f, "iconst_0"),
            iconst_1 => write!(f, "iconst_1"),
            iconst_2 => write!(f, "iconst_2"),
            iconst_3 => write!(f, "iconst_3"),
            iconst_4 => write!(f, "iconst_4"),
            iconst_5 => write!(f, "iconst_5"),
            bipush_x(x) => write!(f, "bipush {}", x),
            istore_0 => write!(f, "istore_0"),
            istore_1 => write!(f, "istore_1"),
            istore_2 => write!(f, "istore_2"),
            istore_3 => write!(f, "istore_3"),
            istore_x(x) => write!(f, "istore {}", x),
            ireturn => write!(f, "ireturn"),
            areturn => write!(f, "areturn"),
            return_void => write!(f, "return"),
            iadd => write!(f, "iadd"),
            isub => write!(f, "isub"),
            imul => write!(f, "imul"),
            idiv => write!(f, "idiv"),
            irem => write!(f, "irem"),
            ineg => write!(f, "ineq"),
            iinc(x, y) => write!(f, "iinc {}, {}", x, y),
            ior => write!(f, "ior"),
            iand => write!(f, "iand"),
            ixor => write!(f, "ixor"),
            ifeq(ref label) => write!(f, "ifeq {}", label),
            iflt(ref label) => write!(f, "iflt {}", label),
            ifne(ref label) => write!(f, "ifne {}", label),
            ifgt(ref label) => write!(f, "ifgt {}", label),
            ifge(ref label) => write!(f, "ifge {}", label),
            ifnull(ref label) => write!(f, "ifnull {}", label),
            ifnonnull(ref label) => write!(f, "ifnonnull {}", label),
            if_icmpeq(ref label) => write!(f, "if_icmpeq {}", label),
            if_icmplt(ref label) => write!(f, "if_icmplt {}", label),
            if_icmpne(ref label) => write!(f, "if_icmpne {}", label),
            if_icmpgt(ref label) => write!(f, "if_icmpgt {}", label),
            if_icmpge(ref label) => write!(f, "if_icmpge {}", label),
            if_icmpnull(ref label) => write!(f, "if_icmpnull {}", label),
            if_icmpnonnull(ref label) => write!(f, "if_icmpnonnull {}", label),
            goto(ref label) => write!(f, "goto {}", label),
            pop => write!(f, "pop"),
            dup => write!(f, "dup"),
            swap => write!(f, "swap"),
            ldc_str(ref string) => write!(f, "ldc {}", string),
            aload_0 => write!(f, "aload_0"),
            aload_1 => write!(f, "aload_1"),
            aload_2 => write!(f, "aload_2"),
            aload_3 => write!(f, "aload_3"),
            aload_x(x) => write!(f, "aload {}", x),
            iaload => write!(f, "iaload"),
            iastore => write!(f, "iastore"),
            getfield => write!(f, "getfield"),
            putfield => write!(f, "putfield"),
            getstatic => write!(f, "getstatic"),
            putstatic => write!(f, "putstatic"),
            newarray => write!(f, "newarray"),
            anewarray => write!(f, "anewarray"),
            multianewarray => write!(f, "multianewarray"),
            invokevirtual(ref class, ref method) => write!(f, "invokevirtual {}/{}", class, method),
            invokestatic(ref class, ref method) => write!(f, "invokestatic {}/{}", class, method),
            invokespecial(ref class) => write!(f, "invokespecial {}/<init>()V", class),
        }
    }
}

/// Represents a JVM bytecode method declaration.
/// This must be located inside a class declaration.
///
/// ```
/// .method <public> <static> [name] (args;)return
/// ```
pub struct MethodDecl {
    name: String,
    main: bool,
    signature: String,
    code: Vec<Bytecode>,
}

impl Display for MethodDecl {
    fn fmt(&self, f: &mut Formatter) -> fmtResult {
        write!(f, ".method public ")?;
        if self.main { write!(f, "static ")?; }
        writeln!(f, "{}{}", self.name, self.signature)?;
        writeln!(f, ".limit stack 5")?;
        writeln!(f, ".limit locals 5")?;
        for instruction in self.code.iter() {
            writeln!(f, "{}", instruction)?;
        }
        writeln!(f, ".end method");
        Ok(())
    }
}

/// Represents a JVM bytecode class declaration
///
/// ```
/// .class <public> [name]
/// .super [extends]
/// ```
pub struct ClassDecl {
    pub name: String,
    pub extends: String,
    pub methods: Vec<MethodDecl>,
}

impl Display for ClassDecl {
    fn fmt(&self, f: &mut Formatter) -> fmtResult {
        writeln!(f, ".class public {}", self.name)?;
        writeln!(f, ".super {}", self.extends)?;
        Ok(())
    }
}
