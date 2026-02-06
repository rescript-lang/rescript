# noop-ppx

A minimal no-op PPX used exclusively for testing rewatch's PPX integration.

## Why does this exist?

Real PPX packages (like sury-ppx, decco, ppx-spice) ship pre-built native binaries for every platform, typically weighing 40-90+ MB unpacked. Since each test creates an isolated sandbox via `yarn install`, that entire weight gets restored per test run, making the suite unacceptably slow.

This package replaces a real PPX with a ~100 byte Node.js script that simply copies the input AST file to the output unchanged. This is sufficient because:

- **Build tests** verify that rewatch successfully orchestrates a build with PPX flags. The noop PPX satisfies the compiler's PPX protocol (read input file, write output file) without transforming anything.
- **Compiler-args tests** verify that rewatch includes the correct `-ppx` flags in parser args. This only requires the PPX path to be resolvable, not functional.

## How it works

The ReScript compiler invokes a PPX as:

```
<ppx-binary> <input-ast-file> <output-ast-file>
```

The PPX reads a binary-marshalled AST from the input file and writes a (possibly transformed) AST to the output file. Our `ppx.js` just copies the input to the output, leaving the AST unchanged.

### Cross-platform support

The `postinstall` script (`install.js`) generates a platform-appropriate `bin` entry point:

- **macOS/Linux**: A Node.js script with a `#!/usr/bin/env node` shebang
- **Windows**: A `bin.cmd` batch file that invokes `node ppx.js`, plus an empty `bin` file. The empty file is needed because rewatch's path resolution checks `.exists()` on the literal path. When the compiler runs `bin input output` via `Sys.command` (which uses `cmd /c`), Windows automatically prefers `bin.cmd` over the extension-less `bin`.
