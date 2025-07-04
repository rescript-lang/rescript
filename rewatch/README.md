# Rewatch

## [![Release](https://github.com/rolandpeelen/rewatch/actions/workflows/build.yml/badge.svg?branch=master&event=release)](https://github.com/rolandpeelen/rewatch/actions/workflows/build.yml)

# Info

Rewatch is an alternative build system for the [Rescript Compiler](https://rescript-lang.org/) (which uses a combination of Ninja, OCaml and a Node.js script). It strives to deliver consistent and faster builds in monorepo setups. Bsb doesn't support a watch-mode in a monorepo setup, and when setting up a watcher that runs a global incremental compile it's consistent but very inefficient and thus slow. 

We couldn't find a way to improve this without re-architecting the whole build system. The benefit of having a specialized build system is that it's possible to completely tailor it to ReScript and not being dependent of the constraints of a generic build system like Ninja. This allowed us to have significant performance improvements even in non-monorepo setups (30% to 3x improvements reported).

# Project Status

This project should be considered in beta status. We run it in production at [Walnut](https://github.com/teamwalnut/). We're open to PR's and other contributions to make it 100% stable in the ReScript toolchain.

# Usage

  1. Install the package

  ```
  yarn add @rolandpeelen/rewatch
  ```

  2. Build / Clean / Watch

  ```
  yarn rewatch build
  ```

  ```
  yarn rewatch clean
  ```

  ```
  yarn rewatch watch
  ```

  You can pass in the folder as the second argument where the 'root' `bsconfig.json` lives. If you encounter a 'stale build error', either directly, or after a while, a `clean` may be needed to clean up some old compiler assets.

## Full Options

Find this output by running `yarn rewatch --help`.

```
Rewatch is an alternative build system for the Rescript Compiler bsb (which uses Ninja internally). It strives to deliver consistent and faster builds in monorepo setups with multiple packages, where the default build system fails to pick up changed interfaces across multiple packages

Usage: rewatch [OPTIONS] [FOLDER]
       rewatch <COMMAND>

Commands:
  build          Build using Rewatch
  watch          Build, then start a watcher
  clean          Clean the build artifacts
  format         Alias to `legacy format`
  dump           Alias to `legacy dump`
  compiler-args  This prints the compiler arguments. It expects the path to a rescript file (.res or .resi)
  legacy         Use the legacy build system
  help           Print this message or the help of the given subcommand(s)

Arguments:
  [FOLDER]
          The relative path to where the main rescript.json resides. IE - the root of your project
          
          [default: .]

Options:
  -v, --verbose...
          Increase logging verbosity

  -q, --quiet...
          Decrease logging verbosity

  -f, --filter <FILTER>
          Filter files by regex
          
          Filter allows for a regex to be supplied which will filter the files to be compiled. For instance, to filter out test files for compilation while doing feature work.

  -a, --after-build <AFTER_BUILD>
          Action after build
          
          This allows one to pass an additional command to the watcher, which allows it to run when finished. For instance, to play a sound when done compiling, or to run a test suite. NOTE - You may need to add '--color=always' to your subcommand in case you want to output color as well

  -c, --create-sourcedirs [<CREATE_SOURCEDIRS>]
          Create source_dirs.json
          
          This creates a source_dirs.json file at the root of the monorepo, which is needed when you want to use Reanalyze
          
          [default: false]
          [possible values: true, false]

      --dev [<DEV>]
          Build development dependencies
          
          This is the flag to also compile development dependencies It's important to know that we currently do not discern between project src, and dependencies. So enabling this flag will enable building _all_ development dependencies of _all_ packages
          
          [default: false]
          [possible values: true, false]

  -n, --no-timing [<NO_TIMING>]
          Disable timing on the output
          
          [default: false]
          [possible values: true, false]

  -s, --snapshot-output [<SNAPSHOT_OUTPUT>]
          simple output for snapshot testing
          
          [default: false]
          [possible values: true, false]

      --bsc-path <BSC_PATH>
          Custom path to bsc

  -h, --help
          Print help (see a summary with '-h')

  -V, --version
          Print version
```

# Contributing

  Pre-requisites:

  - [Rust](https://rustup.rs/)
  - [NodeJS](https://nodejs.org/en/) - For running testscripts only
  - [Yarn](https://yarnpkg.com/) or [Npm](https://www.npmjs.com/) - Npm probably comes with your node installation

  1. `cd testrepo && yarn` (install dependencies for submodule)
  2. `cargo run`

  Running tests:

  1. `cargo build --release`
  2. `./tests/suite.sh`
