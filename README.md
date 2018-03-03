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
version of the parser are in `src/parser/stack_parser.rs` and
`src/emj_grammar.rs`.

I then switched gears and went for the recursive descent approach. This is
the parser I'll be using from now on (or perhaps until I can make the stack
parser work). It's in `src/parser/recursive_parser.rs`. The AST is defined
in `src/parser/ast.rs`, and I've made visitor traits and a printer
implementation which are in `src/parser/visitor/mod.rs` and
`src/parser/visitor/printer.rs`, respectively.

Some known issues with the parser: currently I have a grammar ambiguity I have
yet to resolve which can't tell the difference between a variable and an
assignment statement. I also haven't implemented if statements yet. But I
believe the rest should work fine, and the printer works as expected on a
known good AST.

# Benchmarks

In the `benches/resources` folder I included all of the `.emj` files that were
provided for reference. The `benches/emjc_benches.rs` file runs the lexer on
each file and measures the time taken, reporting some statistics on the
performance of `emjc`. To run the benchmarks, just run `cargo bench`.
