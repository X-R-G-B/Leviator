# koaky

A Blazzing Light Lisp Interpreter

## Features

- Condition
- [x] `if`
- [x] `eq?`
- [x] `diff?`
- [x] `<`
- [x] `>`
- [x] `>=`
- [x] `<=`
- Operator
- [x] `+`
- [x] `-`
- [x] `*`
- [x] `div`
- [x] `mod`
- Type
- [x] `Integer`
- [x] `Boolean`
- [x] `Symbol`
- Function
- [x] `lambda`
- [x] `define`
- Error Handling
- [x] Error handling at execution time
- [x] Nice error message at execution time
- [x] Error handling at parsing time
- [ ] Nice error message at parsing time

## Install

2 methods to use this interpreter:
- from available binary
- build from source

### From Available Binary

- Download the binary for your platform in the latest release.

### Build From Source

- Clone this repository: <https://github.com/X-R-G-B/koaky.git>
- Install stack: <https://docs.haskellstack.org/en/stable/>
- `cd`'d in the repository you just cloned
- Run: `stack build --copy-bins --local-bin-path .`
- The binary will be available with the name `koaky-exe.exe`:windows `koaky-exe`:linux `koaky-exe`:macos

## Usage

```
Usage: koaky-exe [OPTION]

Interpret Lisp
With no options, koaky reads from standard input.

Options:
	-h, --help
		Display this help and exit
	-v, --version
		Output version information and exit
	-f FILE, --file FILE
		Read FILE and Interpret it
	-
		Read from standard input and Interpret it
```
