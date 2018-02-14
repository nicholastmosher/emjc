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

# Benchmarks

In the `benches/resources` folder I included all of the `.emj` files that were
provided for reference. The `benches/emjc_benches.rs` file runs the lexer on
each file and measures the time taken, reporting some statistics on the
performance of `emjc`. To run the benchmarks, just run `cargo bench`.