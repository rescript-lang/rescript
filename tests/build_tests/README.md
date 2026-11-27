# Build system tests

This directory holds the tests of the ReScript build system. The
implementation lives in [`build_system/`](../../build_system/README.md).

| Directory | Tests |
| --- | --- |
| [`integration/`](integration) | The integration suite: shell scripts that build, clean and watch [`testrepo/`](testrepo/README.md) and compare snapshots |
| [`isolated/`](isolated) | End-to-end scripts that each copy a small project of their own (`run.sh` and the `check_*.sh` scripts) |
| [`projects/`](projects) | One small project per directory, each checked by its `input.js` |
| [`unit/`](unit) | OUnit tests of the build system's modules |

Focused documentation:

- [configuration support matrix](../../docs/build_system/CompilerConfigurationSpec.md)
- [monorepo discovery and build scope](../../docs/build_system/MonorepoSupport.md)
- [feature-gated source directories](../../docs/build_system/Features.md)

The ReScript website owns user-facing configuration documentation. The support
matrix records what the build system accepts, including compatibility fields
which are not recommended for new projects.

## Running the tests

Run commands from the repository root:

```sh
make test-build                       # all of the suites below
make -C tests/build_tests unit         # unit tests
make -C tests/build_tests isolated     # isolated/run.sh; also snapshots, verbose, interactive
make -C tests/build_tests integration  # integration suite
node scripts/test.js -projects         # projects (with project_tests and error_tests)
```

The integration suite is [`integration/suite.sh`](integration/suite.sh). It
creates and modifies projects under `testrepo/`; use its helpers for portable
path and file operations rather than adding platform-specific `sed` commands or
fixed sleeps.

When a CLI or configuration value affects compilation, trace it through both
build and watch entry points. Command-line values override project
configuration. Keep that precedence and the compiler-argument conversion in
one owning layer where possible, and add both unit and integration coverage.

Update or remove focused documents when the implementation changes; do not
preserve completed plans as descriptions of current behavior.
