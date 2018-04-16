# CSCI 742 Compiler

[![Build Status](https://travis-ci.com/nicholastmosher/csci-compiler.svg?token=SAsHwf1pH4QtncEYs9HJ&branch=master)](https://travis-ci.com/nicholastmosher/csci-compiler)

## Getting Started

This project is written in Rust, so if you'd like to build it from source
you'll need to install the [rust toolchain](https://rustup.rs/), which will
include the compiler and the build tool, "cargo".

To build the project, simply run `cargo build --release`. This will output
the binary into `target/release/emjc`. You can either directly execute that
binary by `cd`ing into `target/releases` and running `emjc <args>`, or you
can use `cargo run --release -- <args>` to compile and run all in one step.

## Command line interface

If you execute `emjc` without any arguments, you'll see a help display:

```
$ cargo run
Compiling emjc v0.1.0 (file:///home/nick/Documents/classes/compilers/emjc)
    Finished dev [unoptimized + debuginfo] target(s) in 1.47 secs
    Running `target/debug/emjc`
emjc
USAGE:
    emjc <file>
FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information
ARGS:
    <file>
```

To see the output of the lexer, just provide a path to a `.emj` file.

```
cargo run -- my_emj_program.emj
```

# Design

This project is split into two halves, a library and a binary. The core
functionality of the compiler is written in the library, whose entry point
is `src/lib.rs`. The binary half's entry point is `src/bin/main.rs`, and is
in charge of interpreting command line arguments and loading any necessary
files.

Within the library, the compiler implementation is further divided into
"modules". The lexer is one module, and each subsequent subsystem will have
one or more modules of its own.

## Lexer

The Lexer implementation is located in `src/lexer.rs`. In it I make heavy use
of Rust's "Regex" library as well as Rust enums for representing the Tokens.
Enums in Rust can hold values, so any Tokens which have variable content
(such as `INTLIT`, for example) simply store the string representation of the
text they matched inside.

## Parser

I originally started implementing the parser non-recursively, using a stack
for pushing and popping symbols. It worked really well, until I realized I
had no idea how to build the AST using this method. The remnants of that
version of the parser are in `src/syntax/stack_parser.rs` and
`src/emj_grammar.rs`.

I then switched gears and went for the recursive descent approach. This is
the parser I'll be using from now on (or perhaps until I can make the stack
parser work). It's in `src/syntax/recursive_parser.rs`. The AST is defined
in `src/syntax/ast.rs`, and I've made visitor traits and a printer
implementation which are in `src/syntax/visitor/mod.rs` and
`src/syntax/visitor/printer.rs`, respectively.

Some known issues with the parser: currently I have a grammar ambiguity I have
yet to resolve which can't tell the difference between a variable and an
assignment statement. I also haven't implemented if statements yet. But I
believe the rest should work fine, and the printer works as expected on a
known good AST.

## Name Analysis

The name analysis takes place in `src/semantics/name_analysis.rs`. The analysis
is performed in two passes, one to declare all of the classes, functions, and
variables, and another to verify that all the usages of these items are valid.
Some data structures that were used are defined in `src/semantics/mod.rs`, and
include `GlobalScope`, `ClassScope`, and `FunctionScope`.

One known issue right now is that while each identifier is correctly assigned
a unique symbol name, for some reason there are places where the symbol number
will increment by more than one. Besides this, all of the linking works
correctly.

## Type Analysis

The type analysis takes place in `src/semantics/type_analysis.rs`. When running
the program with the `--type` option, all name errors and type errors are
printed.

## Code Generation

The only prerequisite to running the code generation phase is to have a version
of the java runtime (JRE) installed. Upon being run with the `--cgen` switch,
the compiler will check for a `jasmin.jar` file in the system's `$HOME` directory
(`~/jasmin.jar` on UNIX-like systems). If `~/jasmin.jar` does not already exist,
the compiler has a version of jasmin bundled into the binary which it will write
to disk so it can invoke it with java.

When generating code, the compiler outputs one `.jasmin` file per class declared
in the program. These files are named after the class symbols, and get placed in
a '.jasmin' directory created in the current directory when the compiler was
executed. Then, the compiler automatically executes Jasmin and places the generated
`.class` files into a directory called `.class/`, also created in the current
directory. So the file structure after running the compiler with the `--cgen`
option would look something like this:

```
my_program.emj
.jasmine/
  Main_0_.jasmin
  Foo_6_.jasmin
.class/
  Main_0_.class
  Foo_6_.class
```

To execute the compiled program, then, navigate to the `.class` directory and run
`java Main_0_`.

# Benchmarks

In the `benches/resources` folder I included all of the `.emj` files that were
provided for reference. The `benches/emjc_benches.rs` file runs the lexer on
each file and measures the time taken, reporting some statistics on the
performance of `emjc`. To run the benchmarks, just run `cargo bench`.
