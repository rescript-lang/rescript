# ReScript Syntax

Documentation: https://rescript-lang.org/docs/manual/latest/overview

## Contribute

### Why

A detailed discussion by Jonathan Blow and Casey Muratori on why you would hand-roll a parser for a production quality programming language
[Discussion: Making Programming Language Parsers, etc](https://youtu.be/MnctEW1oL-E)

"One reason why I switched off these parser tools is that the promises didn't really materialize.
The amount of work that I had to do change a yacc script from one language to a variant of that language
was more than if I hand wrote the code myself.
"
J. Blow.

### Setup & Usage

Required:

- OCaml 4.10 or later
- OS: macOS, Linux or Windows

In the root of the rescript-compiler repo, run

```sh
opam install . --deps-only --with-test
```

To build the syntax sources, run

```sh
make # or "dune build"
```

This will produce the three binaries `res_parser`, `syntax_tests` and `syntax_benchmarks` (with `.exe` extension on Windows).

We only build production binaries, even in dev mode. No need for a separate dev binary when the build is fast enough. Plus, this encourages proper benchmarking of the (production) binary each diff.

After you make a change:

```sh
make
```

Run the core tests:

```sh
make test
```

Run the extended tests (not fully working on Windows yet):

```sh
make roundtrip-test
```

Those will tell you whether you've got a test output difference. If it's intentional, check them in.

Debug a file:

```sh
# write code in test.res
dune exec -- res_parser test.res # test printer
dune exec -- res_parser -print ast test.res # print ast
dune exec -- res_parser -print comments test.res # print comment table
dune exec -- res_parser -print ml test.res # show ocaml code
dune exec -- res_parser -print res -width 80 test.res # test printer and change default print width
```

Benchmark:

```sh
make bench
```

Enable stack trace:

```sh
# Before you run the binary
export OCAMLRUNPARAM="b"
```

This is likely a known knowledge: add the above line into your shell rc file so that every shell startup you have OCaml stack trace enabled.

### Development Docs

#### Folder Structure

- `src` contains all the parser/printer source code. Don't change folder structure without notice.
- `benchmarks`, `cli` and `tests` contain the source code for the executables used for testing/benchmarking.

#### Error Reporting Logic

Right now, ReScript's compiler's error reporting mechanism, for architectural reasons, is independent from this syntax repo's error reporting mechanism. However, we do want a unified look when they report the errors in the terminal. This is currently achieved by (carefully...) duplicating the error report logic from the compiler repo to here (or vice-versa; either way, just keep them in sync). The files to sync are the compiler repo's [super_location.ml](https://github.com/rescript-lang/rescript-compiler/blob/fcb21790dfb0592f609818df7790192061360631/jscomp/super_errors/super_location.ml) and [super_code_frame.ml](https://github.com/rescript-lang/rescript-compiler/blob/fcb21790dfb0592f609818df7790192061360631/jscomp/super_errors/super_code_frame.ml), into this repo's [res_diagnostics_printing_utils.ml](https://github.com/rescript-lang/syntax/blob/ec5cefb23b659b0a7be170ae0ad26f3fe8a05456/src/res_diagnostics_printing_utils.ml). A few notes:

- Some lines are lightly changed to fit this repo's needs; they're documented in the latter file.
- Please keep these files lightweight and as dependency-less as possible, for easier syncing.
- The syntax logic currently doesn't have warnings, only errors, and potentially more than one.
- In the future, ideally, the error reporting logic would also be unified with GenType and Reanalyze's. It'd be painful to copy paste around all this reporting logic.
- The errors are reported by the parser [here](https://github.com/rescript-lang/syntax/blob/ec5cefb23b659b0a7be170ae0ad26f3fe8a05456/src/res_diagnostics.ml#L146).
- Our editor plugin parses the error report from the compiler and from the syntax [here](https://github.com/rescript-lang/rescript-vscode/blob/0dbf2eb9cdb0bd6d95be1aee88b73830feecb5cc/server/src/utils.ts#L129-L329).

### Example API usage

```ocaml
let filename = "foo.res"
let src = FS.readFile filename

let p =
  (* intended for ocaml compiler *)
  let mode = Res_parser.ParseForTypeChecker in
  (* if you want to target the printer use: let mode = Res_parser.Default in*)
  Res_parser.make ~mode src filename

let structure = Res_core.parseImplementation p
let signature = Res_core.parseSpecification p

let () = match p.diagnostics with
| [] -> () (* no problems *)
| diagnostics -> (* parser contains problems *)
  Res_diagnostics.printReport diagnostics src
```
