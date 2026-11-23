# Language server unit tests

These `ppx_expect` tests import the private `rescript_language_server_core`
library directly. Production sources contain no inline tests, so the shipped
`rescript` executable does not link the expect-test runtime or its dependencies.

Run from the repository root:

```sh
opam exec -- dune runtest tests/lsp_unit_tests
```

`opam exec -- make test-lsp` runs these tests and the stdio end-to-end suite in
`tests/lsp_tests`, after building the toolchain and runtime.
