 

# Ares v0.1.0

This is a small, compiled (with [LLVM](https://llvm.org/)), statically typed, high-level programming language made, in [Rust](https://www.rust-lang.org/), inspired by my 2 most favorite languages (as of writing this) Rust and [TypeScript](https://www.typescriptlang.org/). (Mostly inspired by Rust at this point in development) It combines the simplicity of TypeScript with the ergonomics of a language built from the ground up to be a statically typed language, Rust.

## An Example Program

```rust
fn fib(term: Int) -> Int {
	if term == 0 { 0 }
	else if term == 1 { 1 }
	else { fib(term - 1) + fib(term - 2) }
}

fn main() {
	let count = 0;
	loop {
		if count == 10 {
			return;
		}
		print(fib(count));
		count = count + 1;
	}
}

```

## Goals

Keep the language as simple as possible while also not being limiting. High performance with the productivity of a high level language like [JavaScript](https://en.wikipedia.org/wiki/JavaScript). Make the syntax easy to read and understand. (The language is a far cry from all of these goals though as of right now.)

## Features

- Variables and (very) primitive data types. (`Int`, `Float`, `Boolean` and `String` so far)
- Very basic control flow, `if` statements and `loop`s.
- Functions with parameters and return types.
- Static typing with type inference.
- Rust style interchangeable expressions and statements. (You can put statements inside expressions and vice versa)
- Dead code detection.
- Rust style implicit returns. (The last expression in a function body will get implicitly returned)
- Rust style error messages. (Most of the time they are clunky though)
- And that's really about it for now...

## Road map

- [x] Release a, probably working, version of Ares to GitHub.

- [ ] Unit testing.

- [ ] Language server ([LSP](https://microsoft.github.io/language-server-protocol/)) + [Visual Studio Code](https://code.visualstudio.com/) extension.

- [ ] Implement the rest of the built-in operators, data types, statements etc.

- [ ] `struct`s and `impl`s (Rust style)

- [ ] `enum`s which can carry extra data (Rust style)

  Yeah that's enough for now.

## Installation

You need to have [Rust](https://www.rust-lang.org/) and [Git](https://git-scm.com/) installed and any prerequisites required to compile the [`llvm-sys`](https://crates.io/crates/llvm-sys) crate.

> Ares has only been tested on Linux, Arch Linux x86_64, your experience may vary. If you want compile code for a different OS you may need to change the target triple hard coded in `compiler/src/main.rs` for your system.

Run the command below to clone this repository locally.

```sh
$ git clone https://github.com/VimHax/Ares
```

Afterwards change your directory into the compiler workspace, `./Ares/compiler`, and run;

```sh
$ cargo run main.ares ./main.o ./main --run
```

The first parameter is the path to the input Ares source code. The second parameter is the the path to store the object code. The final parameter is to store the linked executable. The `--run` flag makes the compiler execute the program for you after compilation. Use `--help` to view other options.
