# Rewatch

Rust-based ReScript build system and CLI. Also hosts the LSP server.

## Key subdirectories

- `src/build/` — Build system logic (parsing, compilation, dependency resolution)
- `src/lsp/` — LSP server (tower-lsp) — see `src/lsp/AGENTS.md`

## Tests

Integration tests (build, watch, clean, format, LSP): `tests/rewatch_tests/` — see `tests/rewatch_tests/AGENTS.md`

## Building and testing

```bash
cargo build --manifest-path rewatch/Cargo.toml          # build
cargo test --manifest-path rewatch/Cargo.toml            # unit tests
make test-rewatch                                         # integration tests
```
