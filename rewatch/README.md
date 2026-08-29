# Rewatch build system

Rewatch is the Rust implementation of the ReScript build and watch commands.
It reads `rescript.json`, discovers packages and source files, maintains module
dependencies, and invokes the compiler in dependency order.

## Code map

- [`src/config.rs`](src/config.rs) parses configuration and converts supported
  settings to compiler arguments.
- [`src/build/packages.rs`](src/build/packages.rs) discovers packages and
  resolves package dependencies.
- [`src/build/parse.rs`](src/build/parse.rs) produces compiler AST artifacts.
- [`src/build/deps.rs`](src/build/deps.rs) builds the module dependency graph.
- [`src/build/compile.rs`](src/build/compile.rs) invokes the compiler and
  updates build artifacts.
- [`src/watcher.rs`](src/watcher.rs) maps filesystem and configuration changes
  to incremental rebuilds.
- [`src/cli.rs`](src/cli.rs) and [`src/main.rs`](src/main.rs) own command-line
  parsing and dispatch.

Focused documentation:

- [configuration support matrix](CompilerConfigurationSpec.md)
- [monorepo discovery and build scope](MonorepoSupport.md)
- [feature-gated source directories](Features.md)
- [integration-test workspace](testrepo/README.md)

The ReScript website owns user-facing configuration documentation. The support
matrix in this directory records what the current Rewatch implementation
accepts, including compatibility fields which are not recommended for new
projects.

## Building and testing

Run commands from the repository root:

```sh
make rewatch
make test-rewatch
cargo test --manifest-path rewatch/Cargo.toml
cargo clippy --manifest-path rewatch/Cargo.toml --all-targets --all-features
cargo fmt --check --manifest-path rewatch/Cargo.toml
```

The integration suite is [`tests/suite.sh`](tests/suite.sh). It creates and
modifies projects under `testrepo/`; use its helpers for portable path and file
operations rather than adding platform-specific `sed` commands or fixed
sleeps.

When a CLI or configuration value affects compilation, trace it through both
build and watch entry points. Command-line values override project
configuration. Keep that precedence and the compiler-argument conversion in
one owning layer where possible, and add both unit and integration coverage.

Put public Rust API contracts in source documentation, local algorithm or
state invariants beside their implementation, and cross-module navigation in
this guide. Update or remove focused documents when the implementation changes;
do not preserve completed plans as descriptions of current behavior.
