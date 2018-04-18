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
        write!(f, "{}{}", self.name, self.id)
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
    label(Label),
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
    astore_0,
    astore_1,
    astore_2,
    astore_3,
    astore_x(u64),
    aload_0,
    aload_1,
    aload_2,
    aload_3,
    aload_x(u64),
    iaload,
    iastore,
    aaload,
    aastore,
    getfield(String, String), // Class/Member, Type
    putfield(String, String), // Class/Member, Type
    getstatic(String, String),
    putstatic,
    new(String), // Instantiate a new class.
    newarray(String), // Array type
    anewarray,
    multianewarray,
    arraylength,
    invokevirtual(String, String), // Class name, Method signature
    invokestatic(String, String),  // Class name, Function signature
    invokespecial(String), // Class name (calls constructor)
    comment(String), // Used to insert comments into jasmin assembly
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
            label(ref marker) => write!(f, "{}:", marker),
            ifeq(ref marker) => write!(f, "ifeq {}", marker),
            iflt(ref marker) => write!(f, "iflt {}", marker),
            ifne(ref marker) => write!(f, "ifne {}", marker),
            ifgt(ref marker) => write!(f, "ifgt {}", marker),
            ifge(ref marker) => write!(f, "ifge {}", marker),
            ifnull(ref marker) => write!(f, "ifnull {}", marker),
            ifnonnull(ref marker) => write!(f, "ifnonnull {}", marker),
            if_icmpeq(ref marker) => write!(f, "if_icmpeq {}", marker),
            if_icmplt(ref marker) => write!(f, "if_icmplt {}", marker),
            if_icmpne(ref marker) => write!(f, "if_icmpne {}", marker),
            if_icmpgt(ref marker) => write!(f, "if_icmpgt {}", marker),
            if_icmpge(ref marker) => write!(f, "if_icmpge {}", marker),
            if_icmpnull(ref marker) => write!(f, "if_icmpnull {}", marker),
            if_icmpnonnull(ref marker) => write!(f, "if_icmpnonnull {}", marker),
            goto(ref marker) => write!(f, "goto {}", marker),
            pop => write!(f, "pop"),
            dup => write!(f, "dup"),
            swap => write!(f, "swap"),
            ldc_str(ref string) => write!(f, "ldc {}", string),
            astore_0 => write!(f, "astore_0"),
            astore_1 => write!(f, "astore_1"),
            astore_2 => write!(f, "astore_2"),
            astore_3 => write!(f, "astore_3"),
            astore_x(x) => write!(f, "astore {}", x),
            aload_0 => write!(f, "aload_0"),
            aload_1 => write!(f, "aload_1"),
            aload_2 => write!(f, "aload_2"),
            aload_3 => write!(f, "aload_3"),
            aload_x(x) => write!(f, "aload {}", x),
            iaload => write!(f, "iaload"),
            iastore => write!(f, "iastore"),
            aaload => write!(f, "aaload"),
            aastore => write!(f, "aastore"),
            getfield(ref field_spec, ref kind) => write!(f, "getfield {} {}", field_spec, kind),
            putfield(ref field_spec, ref kind) => write!(f, "putfield {} {}", field_spec, kind),
            getstatic(ref name, ref kind) => write!(f, "getstatic {} {}", name, kind),
            putstatic => write!(f, "putstatic"),
            new(ref class) => write!(f, "new {}", class),
            newarray(ref kind) => write!(f, "newarray {}", kind),
            anewarray => write!(f, "anewarray"),
            multianewarray => write!(f, "multianewarray"),
            arraylength => write!(f, "arraylength"),
            invokevirtual(ref class, ref method) => write!(f, "invokevirtual {}/{}", class, method),
            invokestatic(ref class, ref method) => write!(f, "invokestatic {}/{}", class, method),
            invokespecial(ref class) => write!(f, "invokespecial {}/<init>()V", class),
            comment(ref string) => write!(f, "; {}", string),
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
            match instruction {
                comment(_) => writeln!(f, "  {}", instruction),
                label(_) => writeln!(f, "    {}", instruction),
                _ => writeln!(f, "        {}", instruction),
            };
        }
        writeln!(f, ".end method");
        Ok(())
    }
}

pub struct MemberDecl {
    name: String,
    kind: String,
}

impl Display for MemberDecl {
    fn fmt(&self, f: &mut Formatter) -> fmtResult {
        write!(f, ".field public {} {}", self.name, self.kind)
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
    pub members: Vec<MemberDecl>,
    pub methods: Vec<MethodDecl>,
}

impl Display for ClassDecl {
    fn fmt(&self, f: &mut Formatter) -> fmtResult {
        writeln!(f, ".class public {}", self.name)?;
        writeln!(f, ".super {}", self.extends)?;
        for member in self.members.iter() {
            writeln!(f, "{}", member)?;
        }
        Ok(())
    }
}
