# Analysis Library and Binary

This subfolder builds a private command line binary used by the plugin to power a few functionalities such as jump to definition, hover and autocomplete.

The binary reads the `.cmt` and `.cmti` files and analyses them.

For installation & build instructions, see the main CONTRIBUTING.md.

## Overview

See main CONTRIBUTING.md's repo structure. Additionally, `examples/` is a convenience debugging repo. Check out `test.sh` (invoked through `make test`) to see the snapshots testing workflow stored in `tests/`.

## Usage

```shell
dune exec -- rescript-editor-analysis --help
```

## History

This project is based on a fork of [Reason Language Server](https://github.com/jaredly/reason-language-server).

## Tests

### Prerequisites

- Ensure the compiler is built (`make build` in the repository root).
- Ensure the library is built (`make lib` in the repository root).

### Running the Tests

Run `make test` in `tests/analysis_tests/tests`.

### Key Concept

The tests in the `tests/analysis_tests/tests` folder are based on the `dune exec -- rescript-editor-analysis test` command. This special subcommand processes a file and executes specific editor analysis functionality based on special syntax found in code comments.

Consider the following code:

```res
let a = 5
// a.
//   ^com
```

After building the ReScript project (**⚠️ this is a requirement**), you can execute `dune exec -- rescript-editor-analysis test Sample.res`, and completion will be performed for the cursor position indicated by `^`. The `com` directive requests completion. To see other commands, check out the pattern match in the `test` function in [Commands.ml](./src/Commands.ml).

> [!WARNING]
> Ensure there are no spaces in the code comments, as the commands are captured by a regular expression that expects spaces and not tabs!

Here’s how it works: once a command is found in a comment, a copy of the source file is created inside a temporary directory, where the line above `^com` is uncommented. The corresponding analysis functionality is then processed, typically with `~debug:true`. With debug enabled, code paths like

```ml
if Debug.verbose () then
      print_endline "[complete_typed_value]--> Tfunction #other";
```

will print to stdout. This is helpful for observing what happens during the analysis.

When you run `make test` (from the `tests/analysis_tests` folder), `dune exec -- rescript-editor-analysis test <file>` will be executed for each `*.res` file in `analysis/tests/src`. The stdout will be compared to the corresponding `analysis/tests/src/expected` file. If `git diff` indicates changes, `make test` will fail, as these differences might be unintentional.

## Testing on Your Own Projects

To use a local version of `rescript-editor-analysis`, the targeted project needs to be compiled with the local compiler.

Install your local ReScript with `npm i /path/to/your-local-rescript-repo`.
Reinstall the dependencies and run `npx rescript` in your project. This ensures the project is compiled with the same compiler version that the `rescript-editor-analysis` will process.

## Debugging

It is possible to debug `analysis` via [ocamlearlybird](https://github.com/hackwaly/ocamlearlybird).

1. Install `opam install earlybird`.
2. Install the [earlybird extension](https://marketplace.visualstudio.com/items?itemName=hackwaly.ocamlearlybird).
3. Create a launch configuration (`.vscode/launch.json`):

```json
{
    "version": "0.2.0",
    "configurations": [
        {
            "name": "Debug analysis",
            "type": "ocaml.earlybird",
            "request": "launch",
            "program": "${workspaceFolder}/_build/default/analysis/bin/main.bc",
            "stopOnEntry": true,
            "cwd": "/projects/your-project",
            "env": {
                "CAML_LD_LIBRARY_PATH": "${workspaceFolder}/_build/default/compiler/ext"
            },
            "arguments": [
                "test",
                "src/Main.res"
            ]
        }
    ]
}
```

The `CAML_LD_LIBRARY_PATH` environment variable is required to tell OCaml where `dllext_stubs.so` can be loaded from.