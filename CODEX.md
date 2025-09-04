# CODEX.md

This document provides detailed guidance for OpenAI Codex and similar code agents working in the ReScript compiler repository. It consolidates setup, build, testing, repository layout, workflows, and conventions into one place.

## Project Overview

ReScript is a robustly typed language that compiles to efficient and human-readable JavaScript. This monorepo contains the compiler (OCaml), build system, runtime (ReScript), analysis tools, and supporting tooling.

## Prerequisites and Environment

- Node.js: use `v22.x` locally (`.nvmrc` is `22`), but min `>=20.11.0` is enforced for tooling
- Yarn: v4 (Corepack). The repo uses Yarn workspaces
- OPAM: `v2.2.0+` recommended; `opam switch create 5.3.0`
- OCaml: `5.3.0` (for development switch); dune `>=3.17`
- Python: `<=3.11` (ninja build)
- Rust toolchain (for `rewatch`)
- C toolchain (Xcode CLT on macOS)

Devcontainer alternative:
- See `.devcontainer/` for a ready-to-use VSCode environment (installs gh, Node 20, OPAM 2.4.1, sets up switch, installs deps, enables corepack, runs yarn)

Recommended VS Code extensions:
- `ocamllabs.ocaml-platform`, `chenglou92.rescript-vscode`, `biomejs.biome`, `rust-lang.rust-analyzer`

## Initial Setup

```bash
# 1) OPAM and OCaml switch
opam init
opam switch create 5.3.0
opam install . --deps-only --with-test --with-dev-setup -y

# 2) Node/Yarn deps
corepack enable
yarn install

# 3) Build (compiler + copy exes)
make
```

Tip: Use the devcontainer (`Dev Containers: Rebuild and Reopen in Container`) if local setup is troublesome.

## Build and Watch

Using dune directly:
```bash
dune build           # one-off
dune build -w        # watch
```

Makefile targets (preferred wrappers):
```bash
make                 # dune build + copy executables
make watch           # dune build -w
make ninja           # build vendored ninja + copy exe
make rewatch         # cargo build rewatch + copy exe
make lib             # build @rescript/runtime
make artifacts       # populate lib/ocaml and update packages/artifacts.json
```

Executables are copied into the platform dir, e.g. `darwinarm64/`.

## Testing

High-level entry:
```bash
make test            # build + run all core test suites
make test-all        # test + gentype + analysis + tools + rewatch
```

Focused suites:
```bash
make test-syntax             # syntax parser tests
make test-syntax-roundtrip   # adds roundtrip tests (ROUNDTRIP_TEST=1)
make test-gentype            # tests/gentype_tests/typescript-react-example
make test-analysis           # analysis tests (language server/tooling)
make test-tools              # tools tests
make test-rewatch            # rewatch suite
```

Single-file compiler debug:
```bash
./cli/bsc.js myFile.res
./cli/bsc.js -dparsetree myFile.res
./cli/bsc.js -dtypedtree myFile.res
```

Token/untyped tree via parser:
```bash
dune exec res_parser -- -print tokens myFile.res
dune exec res_parser -- -print ast -recover myFile.res
```

Use local compiler in a project without linking:
```bash
RESCRIPT_BSC_EXE=/path/to/packages/@rescript/darwin-arm64/bin/bsc.exe npx rescript
```

## Code Quality and Linting

```bash
make format          # repository formatting (OCaml + others)
make checkformat     # formatting check
npm run check        # Biome lint (changed files)
npm run check:all    # Biome lint (entire repo)
npm run typecheck    # TypeScript type-check (TS tooling only)
```

## Repository Layout (selected)

- `compiler/syntax/`: ReScript syntax parser (MIT licensed)
- `compiler/frontend/`: AST transformations, externals, attributes
- `compiler/ml/`: OCaml typechecker infra (typedtree, etc.)
- `compiler/core/`: Lambda and JS IR passes (`lam_*`, `js_*`, `js_dump*`)
- `compiler/common/`, `compiler/ext/`, `compiler/depends/`: shared utilities
- `compiler/bsc/`, `compiler/bsb_exe/`: binary entry points
- `analysis/`: language server and tooling support
- `runtime/`: ReScript standard library sources
- `lib/`: compiled JS output of the stdlib
- `packages/`: Yarn workspaces for platform bins and playground
- `rewatch/`: Rust file watcher
- `ninja/`: vendored build tool
- `tests/`: unit, integration, syntax, analysis, tools, gentype, etc.

Important test locations:
- `tests/tests/`: mocha unit tests (runtime)
- `tests/build_tests/`: integration tests (build system)
- `tests/ounit_tests/`: compiler unit tests (OCaml)
- `tests/syntax_tests/`: syntax and roundtrip tests
- `tests/analysis_tests/`: language server/tooling tests
- `tests/tools_tests/`: tooling tests

## Yarn Workspaces and Version Constraints

- Workspaces: see `package.json` and `packages/`
- Version metadata is enforced by `yarn.config.cjs` (EXPECTED_VERSION)
  - To auto-fix metadata across workspaces and compiler sources:
    ```bash
    yarn constraints --fix
    ```

## Playground Bundle (JSOO)

```bash
make playground            # builds JS bundle (browser profile)
make playground-cmijs      # builds and bundles cmij dependencies
yarn workspace playground test
```

Bundle artifacts:
- `playground/compiler.js` (compiler API for browser/Node)
- `playground/packages/compiler-builtins/` (core cmij)
- `playground/packages/*` (3rd-party cmij)

Release to Cloudflare R2 (requires configured rclone remote):
```bash
make playground-release
```

## Branching, PRs, and DCO

- Target `master` for new features (v12 dev)
- Target `11.0_release` for fixes/maintenance; it is periodically merged into `master`
- DCO is required for every commit. Use:
  ```bash
  git commit -s -m "your message"   # adds Signed-off-by with your git config
  ```

## Release Process (summary)

- Ensure version numbers are correct across workspaces
- Update `CHANGELOG.md` via PR
- Tag release to trigger publishing (playground bundle + npm `rescript` with tag `ci`)
- Verify, then set npm dist-tag (`latest` or `next`)
- Prepare next-dev version: update expected version in `yarn.config.cjs`, run `yarn constraints --fix`, update `CHANGELOG.md`

## Performance and Output Quality

- Avoid introducing meaningless symbols
- Keep generated JS readable; prefer simple, maintainable transformations
- Consider compilation speed impact of changes
- Use appropriate optimization passes in Lambda and JS IR where applicable

## Common Tasks Cheat Sheet

```bash
# Build everything and copy exes
make

# Build stdlib
make lib

# All tests
make test

# Syntax + roundtrip
make test-syntax-roundtrip

# Update artifacts list (affects npm packages)
make artifacts

# Lint
npm run check

# Format
make format
```

## Troubleshooting

- OPAM: If `5.3.0` switch is missing, run `opam update && opam upgrade` and retry `opam switch create 5.3.0`
- Python: Ensure `python --version` is `<=3.11` if ninja build fails
- Yarn: Use `corepack enable` and Yarn v4; `yarn -v` should reflect workspace configuration
- Rewatch build: Ensure Rust toolchain installed and up-to-date
- Local compiler in projects: prefer `RESCRIPT_BSC_EXE=... npx rescript` for quick tests

## Key Files

- `Makefile`: build/test wrappers and helper targets
- `dune-project` / `rescript.opam`: dune/opam metadata and constraints
- `yarn.config.cjs`: enforces version metadata across workspaces and sources
- `.devcontainer/*`: containerized dev environment
- `cli/*`: Node entry points for compiler/build tooling

## Notes for Code Agents

- Respect code styles: OCaml snake_case; ReScript camelCase
- Keep edits focused; avoid large refactors unless necessary
- Add tests for new behavior; update snapshots where applicable
- Use DCO sign-off for all commits
- Prefer `make` targets over ad-hoc dune/cargo invocations when possible

