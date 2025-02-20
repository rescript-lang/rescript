# Contributing

Welcome to the ReScript compiler project!

This document will give you guidance on how to get up and running to work on the ReScript compiler and toolchain.

(If you want to contribute to the documentation website, check out [rescript-association/rescript-lang.org](https://github.com/reason-association/rescript-lang.org).)

We tried to keep the installation process as simple as possible. In case you are having issues or get stuck in the process, please let us know in the issue tracker.

Happy hacking!

## Setup

> Most of our contributors are working on Apple machines, so all our instructions are currently macOS / Linux centric. Contributions for Windows development welcome!

- [NodeJS v18](https://nodejs.org/)
- C compiler toolchain (usually installed with `xcode` on Mac)
- Python <= 3.11 (required to build ninja)
- Rust toolchain (required to build rewatch; follow the instructions at https://www.rust-lang.org/tools/install)
- `opam` (OCaml Package Manager) v2.2.0
- VSCode (+ [OCaml Platform Extension](https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform))

## Cloning the Git Repo

The rescript-compiler git repo is very large because, prior to tooling improvements made for ReScript 11 development, build artifacts were checked in to the repo ("snapshotted"). Therefore, cloning the repo in full will consume a lot of bandwidth and disk space (> 2GB).

If you are only interested in the latest master commit, you can perform a shallow clone instead as follows:

```sh
git clone --depth 1 https://github.com/rescript-lang/rescript.git
```

This will only consume less than 50MB.

## Installation

### A. Manual installation

#### Install OCaml compiler + dependencies

The ReScript compiler compiles with any recent OCaml compiler. We are using `dune` as a build system for easy workflows and proper IDE support.

Make sure you have [opam](https://opam.ocaml.org/doc/Install.html) installed on your machine.

```sh
opam init

# Any recent OCaml version works as a development compiler
opam switch create 5.3.0 # can also create local switch with opam switch create

# Install dev dependencies from OPAM
opam install . --deps-only --with-test --with-dev-setup -y
```

#### npm install

Run `npm install --ignore-scripts`. This will install the npm dependencies required for the build scripts.

### B. Devcontainer

As an alternative to the manual installation, the repository provides a [development container](https://containers.dev/) definition that can be used with [VS Code's Remote Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers). Use this to get a stable development environment without having to install anything locally other than VS Code and Docker.

Run the `Dev Containers: Rebuild and Reopen in Container` action to get started.

You can also open this dev container with [GitHub Codespaces](https://github.com/features/codespaces/).

## Building the Compiler

The compiler binaries themselves can be built directly with dune as follows:

```sh
# One off build
dune build

# Watch mode
dune build -w
```

For all additional operations, a Makefile is provided:

```sh
# Build the compiler using dune and copy the exes into the platform dir
make

# Build the ninja build tool
make ninja

# Build the ReScript standard library using ninja and the compiler
make lib

# Run compiler tests
make test

# Run syntax tests
make test-syntax

# Run syntax tests including roundtrip tests
make test-syntax-roundtrip

# Populate lib/ocaml and update artifact list
make artifacts
```

## Coding Style

- OCaml Code: snake case format is used, e.g, `to_string`
- ReScript Code: the camel case format is used, e.g `toString`

## Adding new Files to the Npm Package

To make sure that no files are added to or removed from the npm package inadvertently, an artifact list is kept at `packages/artifacts.txt`. During CI build, it is verified that only the files that are listed there are actually included in the npm package.

After adding a new file to the repository that should go into the npm package - e.g., a new stdlib module -, run `make artifacts`.

## Test the compiler

### Single file

```sh
make lib # Build compiler and standard library
./cli/bsc myTestFile.res
```

To view the untyped tree of the file run:

```sh
./cli/bsc -dparsetree myTestFile.res
```

To view the typed tree of the file run:

```sh
./cli/bsc -dtypedtree myTestFile.res
```

### Project

```sh
make artifacts # Build compiler and standard library and populate lib/ocaml
npm link
cd myProject
npm install
npm link rescript
```

### Running Automatic Tests

We provide different test suites for different levels of the compiler and build system infrastructure. Always make sure to locally build your compiler before running any tests.

To run all tests:

```sh
make test
```

**Run Mocha tests only (for our runtime code):**

This will run our `mocha` unit test suite defined in `tests/tests`.

```
node scripts/test.js -mocha
```

**Run build system test (integration tests):**

This will run the whole build system test suite defined in `tests/build_tests`.

```
node scripts/test.js -bsb
```

**Run ounit tests:**

This will run unit tests for compiler related modules. The tests can be found in `tests/ounit_tests`.

```
node scripts/test.js -ounit
```

## Contributing to the Runtime

The runtime implementation is written in ReScript with some raw JS code embedded (`runtime` directory).

The goal is to implement the runtime **purely in ReScript**. This includes removing all existing occurrences of embedded raw JS code as well whenever possible, and you can help!

Each new PR should include appropriate testing.

Currently all tests are located in the `tests/tests` directory and you should either add / update test files according to your changes to the compiler.

There are currently two formats for test files:

1. Mocha test files that run javascript test code
2. Plain `.res` files to check the result of compilation to JS (expectation tests)

Below we will discuss on how to write, build and run these test files.

### 1) Write a Mocha Test File

- Create a file `tests/tests/src/feature_abc_test.res`. Make sure to end the file name with `_test.res`.
- Inside the file, add a mocha test suite. The mocha bindings are defined in `tests/tests/src/mt.res`. To get you started, here is a simple scaffold for a test suite with multiple test cases:

  ```rescript
  let suites: Mt.pair_suites = list{
    ("hey", _ => Eq(true, 3 > 2)),
    ("hi", _ => Neq(2, 3)),
    ("hello", _ => Approx(3.0, 3.0)),
    ("throw", _ => ThrowAny(_ => raise(SomeException))),
  }

  Mt.from_pair_suites(__MODULE__, suites)
  ```

- Build the test files and run the tests: `node scripts/test.js -mocha`.

### 2) Write a Plain `.res` Test File

This is usually the file you want to create to test certain compile behavior without running the JS code formally as a test, i.e., when you just want to check that the ReScript code compiles and produces the expected JS code.

- Create your test file `tests/tests/src/my_file_test.res`. Make sure to end the file name with `_test.res`.
- Build the `.js` artifact: `node scripts/test.js -mocha`.
- Verify the output, check in the `tests/tests/src/my_file_test.res` and `tests/tests/src/my_file_test.js` to version control. The checked in `.js` file is essential for verifying regressions later on.
- Eventually check in other relevant files changed during the rebuild (depends on your compiler changes).

## Contribute to the ReScript Playground Bundle

The "Playground bundle" is a JS version of the ReScript compiler; including all necessary dependency files (stdlib / belt etc). It is useful for building tools where you want to compile and execute arbitrary ReScript code in the browser.

The ReScript source code is compiled with a tool called [JSOO (js_of_ocaml)](https://ocsigen.org/js_of_ocaml/latest/manual/overview), which uses OCaml bytecode to compile to JavaScript and is part of the bigger OCaml ecosystem.

### Building the Bundle

The entry point of the JSOO bundle is located in `compiler/jsoo/jsoo_playground_main.ml`, the compiler and its relevant runtime cmij files can be built via make:

```sh
make playground
make playground-cmijs
```

Note that building the cmijs is based on the dependencies defined in `packages/playground-bundling/package.json`. In case you want to build some different version of e.g. `@rescript/react` or just want to add a new package, change the definition within the `package.json` file and run `make playground-cmijs` again.

After a successful compilation, you will find following files in your project:

- `playground/compiler.js` -> This is the ReScript compiler, which binds the ReScript API to the `window` object.
- `playground/packages` -> Contains third party deps with cmij.js files (as defined in `packages/playground-bundling/bsconfig.json`)
- `playground/compilerCmij.js` -> The compiler base cmij containing all the relevant core modules (`Js`, `Belt`, `Pervasives`, etc.)

You can now use the `compiler.js` file either directly by using a `<script src="/path/to/compiler.js"/>` and `<script src="/path/to/packages/compilerCmij.js"/>` inside a html file, use a browser bundler infrastructure to optimize it, or use `nodejs` to run it on a command line:

```
$ node
> require("./compiler.js");
> require("./packages/compilerCmij.js")
> let compiler = rescript_compiler.make()
> let result = compiler.rescript.compile(`Js.log(Sys.ocaml_version)`);
> eval(result.js_code);
4.06.2+BS
```

### Testing the Playground bundle

Run `node playground/playground_test.js` for a quick sanity check to see if all the build artifacts are working together correctly. When releasing the playground bundle, the test will always be executed before publishing to catch regressions.

### Working on the Playground JS API

Whenever you are modifying any files in the ReScript compiler, or in the `jsoo_playground_main.ml` file, you'll need to rebuild the source and recreate the JS bundle.

```
make playground

# optionally run your test / arbitrary node script to verify your changes
node playground/playground_test.js
```

### Publishing the Playground Bundle on our KeyCDN

> Note: If you want to publish from your local machine, make sure to set the `KEYCDN_USER` and `KEYCDN_PASSWORD` environment variables accordingly (credentials currently managed by @ryyppy). Our CI servers / GH Action servers are already pre-configured with the right env variable values.

Our `compiler.js` and third-party packages bundles are hosted on [KeyCDN](https://www.keycdn.com) and uploaded via FTPS.

The full release can be executed with the following make script:

```
make playground-release
```

The script will automatically detect the ReScript version from the `compiler.js` bundle and automatically create the correct directory structure on our CDN ftp server.

Note that there's currently still a manual step involved on [rescript-lang.org](https://rescript-lang.org) to make the uploaded playground version publicly available.

## Contribute to the API Reference

The API reference is generated from doc comments in the source code. [Here](https://github.com/rescript-lang/rescript-compiler/blob/99650/jscomp/others/js_re.mli#L146-L161)'s a good example.

Some tips:

- The first sentence or line should be a very short summary. This is used in indexes and by tools like merlin.
- Ideally, every function should have **at least one** `@example`.
- Cross-reference another definition with `{! identifier}`. But use them sparingly, they’re a bit verbose (currently, at least).
- Wrap non-cross-referenced identifiers and other code in `[ ... ]`.
- Escape `{`, `}`, `[`, `]` and `@` using `\`.
- It’s possible to use `{%html ...}` to generate custom html, but use this very, very sparingly.
- A number of "documentation tags" are provided that would be nice to use, but unfortunately they’re often not supported for \`external\`s. Which is of course most of the API.
- `@param` usually doesn’t work. Use `{b <param>} ...` instead
- `@returns` usually doesn’t work. Use `{b returns} ...` instead.
- Always use `@deprecated` when applicable.
- Always use `@raise` when applicable.
- Always provide a `@see` tag pointing to MDN for more information when available.

See [Ocamldoc documentation](http://caml.inria.fr/pub/docs/manual-ocaml/ocamldoc.html#sec333) for more details.

To generate the html:

```sh
../scripts/ninja docs
```

## Code structure

The highlevel architecture is illustrated as below:

```
Source Language
  |
  | (Parser)
  v
Surface Syntax Tree
  |
  | (Built-in Syntax tree transformation)
  v
Surface Syntax Tree
  |
  | (Reuse OCaml Type checker)
  v
Typedtree
  |
  | (Reuse OCaml pattern match compiler and erase types)
  v
Lambda IR (OCaml compiler libs) ---+
  |   ^                            |
  |   |                     Lambda Passes (lam_* files)
  |   |             Optimization/inlining/dead code elimination
  |   \                            |
  |    \ --------------------------+
  |
  |  Self tail call elimination
  |  Constant folding + propagation
  V
JS IR (J.ml)  ---------------------+
  |   ^                            |
  |   |                     JS Passes (js_* files)
  |   |            Optimization/inlining/dead code elimination
  |   \                            |
  |    \  -------------------------+
  |
  |  Smart printer includes scope analysis
  |
  V
Javascript Code
```

Note that there is one design goal to keep in mind, never introduce any meaningless symbol unless necessary, we do optimizations, however, it should also compile readable output code.

## PR target branch

Target branch `master` for development of new (breaking) features (v12).

Bug fixes and maintenance should target branch `11.0_release`.
We'll merge `11.0_release` into `master` from time to time to propagate those changes.

## Release Process

To build a new version and release it on NPM, follow these steps:

1. Verify that the version number is already set correctly for the release. (It should have been incremented after releasing the previous version.)
1. Create a PR to update `CHANGELOG.md`, removing the "(Unreleased)" for the version to be released.
1. Once that PR is merged and built successfully, tag the commit with the version number (e.g., "v10.0.0", or "v10.0.0-beta.1") and push the tag.
1. This triggers a tag build that will upload the playground bundle to KeyCDN and publish the `rescript` and `@rescript/std` npm packages with the tag "ci".
1. Verify that the playground bundle for the new version is now present on https://cdn.rescript-lang.org/.
1. Run `npm info rescript` to verify that the new version is now present with tag "ci".
1. Test the new version.
1. Tag the new version as appropriate (`latest` or `next`):
   - `npm dist-tag add rescript@<version> <tag>`
   - `npm dist-tag add @rescript/std@<version> <tag>`
1. Create a release entry for the version tag on the [Github Releases page](https://github.com/rescript-lang/rescript-compiler/releases), copying the changes from `CHANGELOG.md`.
1. Create a PR with the following changes to prepare for development of the next version:
   - Increment the version number in `package.json` for the next version.
   - Run `node scripts/setVersion.js` to take that version number over into other files.
   - Update `CHANGELOG.md` and add an entry for the next version, e.g., "10.0.0-beta.2 (Unreleased)"
1. Coordinate any forum/blog posts with [@ryyppy](https://github.com/ryyppy).

## Debugging issues from CI builds

To reproduce issues, it can be helpful to the team to install a specific version of the compiler. To do so:

1. Go to [Actions CI for master](https://github.com/rescript-lang/rescript-compiler/actions/workflows/ci.yml?query=branch%3Amaster)
   - If you need a specific branch, select a different one to filter to in the GitHub UI.
1. Select a specific run (likely the latest)
1. Under "Artifacts", download the `npm-packages` artifact and extract it to a folder.
1. In your repository run:

```console
npm i <path_to_download>npm-packages/rescript-*.tgz
```

1. Then attempt to rebuild your project as you would normally.

## Contribution Licensing

Since ReScript is distributed under the terms of the [LGPL Version 3](LICENSE), contributions that you make are licensed under the same terms. In order for us to be able to accept your contributions, we will need explicit confirmation from you that you are able and willing to provide them under these terms, and the mechanism we use to do this is called a Developer's Certificate of Origin [DCO](DCO.md). This is very similar to the process used by the Linux(R) kernel, Samba, and many other major open source projects.

To participate under these terms, all that you must do is include a line like the following as the last line of the commit message for each commit in your contribution:

    Signed-Off-By: Random J. Developer <random@developer.example.org>

You must use your real name (sorry, no pseudonyms, and no anonymous contributions).
