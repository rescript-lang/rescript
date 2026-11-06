# Rewatch integration suite

This directory holds the integration suite of the ReScript build system and
documents its behavior. The implementation lives in
[`../rewatch-ocaml`](../rewatch-ocaml/README.md).

Focused documentation:

- [configuration support matrix](CompilerConfigurationSpec.md)
- [monorepo discovery and build scope](MonorepoSupport.md)
- [feature-gated source directories](Features.md)
- [integration-test workspace](testrepo/README.md)

The ReScript website owns user-facing configuration documentation. The support
matrix in this directory records what the build system accepts, including
compatibility fields which are not recommended for new projects.

## Running the suite

Run commands from the repository root:

```sh
make test-rewatch
```

The integration suite is [`tests/suite.sh`](tests/suite.sh). It creates and
modifies projects under `testrepo/`; use its helpers for portable path and file
operations rather than adding platform-specific `sed` commands or fixed
sleeps.

When a CLI or configuration value affects compilation, trace it through both
build and watch entry points. Command-line values override project
configuration. Keep that precedence and the compiler-argument conversion in
one owning layer where possible, and add both unit and integration coverage.

Update or remove focused documents when the implementation changes; do not
preserve completed plans as descriptions of current behavior.
