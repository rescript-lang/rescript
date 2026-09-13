# Compiler agent instructions

Read these alongside the [root instructions](../AGENTS.md). All paths and
commands below are relative to the repository root unless stated otherwise.

## ⚠️ Critical Guidelines & Common Pitfalls

- **We are NOT bound by OCaml compatibility** - The ReScript compiler originated as a fork of the OCaml compiler, but we maintain our own AST and can make breaking changes. Focus on what's best for ReScript's JavaScript compilation target.

- **Never modify `parsetree0.ml`** - Existing PPX (parser extensions) rely on this frozen v0 version. When changing `parsetree.ml`, always update the mapping modules `ast_mapper_from0.ml` and `ast_mapper_to0.ml` to maintain PPX compatibility while allowing the main parsetree to evolve. **Test the bridge with the existing infra** — do not build new harnesses for this:
  - Add a source fixture exercising the new syntax to `tests/syntax_tests/data/ast-mapping/`. Every file there is run through `res_parser -test-ast-conversion` (which round-trips the parsetree through the frozen v0 AST before printing) as part of `make test-syntax`; the printed output must match the snapshot in its `expected/` directory.
  - For exact-identity invariants (locations, attributes) or the v0 wire shape itself, add cases to `tests/ounit_tests/ounit_ast_mapper0_tests.ml`, which tests `ast_mapper_to0`/`ast_mapper_from0` directly.

- **Missing test coverage** - Always add tests for syntax, lambda, and end-to-end behavior

- **Test early and often** - Add tests immediately after modifying each compiler layer to catch problems early, rather than waiting until all changes are complete

- **Do not introduce new keywords unless absolutely necessary** - Try to find ways to implement features without reserving keywords, as seen with the "catch" implementation that avoids making it a keyword.

## Compiler Architecture

### Compilation Pipeline

```
ReScript Source (.res)
  ↓ (ReScript Parser - compiler/syntax/)
Surface Syntax Tree
  ↓ (Frontend transformations - compiler/frontend/)
Surface Syntax Tree
  ↓ (OCaml Type Checker - compiler/ml/)
Typedtree
  ↓ (Lambda compilation - compiler/core/lam_*)
Lambda IR
  ↓ (JS compilation - compiler/core/js_*)
JS IR
  ↓ (JS output - compiler/core/js_dump*)
JavaScript Code
```

### Platform-specific compiler modules

The Dune `browser` profile builds the playground compiler. Platform-dependent
modules are stored below `platform/native/` and `platform/playground/` in their
owning compiler directory. Rules in that directory's `dune` file copy the
selected implementation into the build directory as an ordinary `.ml` module;
all other profiles select the native source. Generated module paths in errors
or stack traces therefore map back to one of those two source directories.

## Working on the Compiler

### Development Workflow

Read the relevant area guide linked from the [root instructions](../AGENTS.md#area-guidance).

1. **Understand which layer you're working on:**
   - **Syntax layer** (`compiler/syntax/`): Parsing and surface syntax
   - **ML layer** (`compiler/ml/`): Type checking and AST transformations
   - **Lambda layer** (`compiler/core/lam_*`): Intermediate representation and optimizations
   - **JS layer** (`compiler/core/js_*`): JavaScript generation

2. **Always run appropriate tests:**

   ```bash
   # For compiler or stdlib changes
   make test

   # For syntax changes
   make test-syntax

   # For specific test types
   make test-syntax-roundtrip
   make test-gentype
   make test-analysis
   ```

3. **Test your changes thoroughly:**
   - Syntax tests for new language features
   - Integration tests for behavior changes
   - Unit tests for utility functions
   - Always check JavaScript output quality

### Debugging Techniques

#### View Intermediate Representations

```bash
# Source code (for debugging preprocessing)
./cli/bsc.js -dsource myfile.res

# Parse tree (surface syntax after parsing)
./cli/bsc.js -dparsetree myfile.res

# Typed tree (after type checking)
./cli/bsc.js -dtypedtree myfile.res

# Raw lambda (unoptimized intermediate representation)
./cli/bsc.js -drawlambda myfile.res

# Use lambda printing for debugging (add in compiler/core/lam_print.ml)
```

#### Common Debug Scenarios

- **JavaScript formatting issues**: Check `compiler/ml/pprintast.ml`
- **Type checking issues**: Look in `compiler/ml/` type checker modules
- **Optimization bugs**: Check `compiler/core/lam_*.ml` analysis passes
- **Code generation bugs**: Look in `compiler/core/js_*.ml` modules

## Performance Considerations

The compiler is designed for fast feedback loops and scales to large codebases:

- **Avoid meaningless symbols** in generated JavaScript
- **Maintain readable JavaScript output**
- **Consider compilation speed impact** of changes
- **Use appropriate optimization passes** in Lambda and JS IRs
- **Profile** before and after performance-related changes

## Common Tasks

### Adding New Language Features

1. Update parser in `compiler/syntax/`
2. Update AST definitions in `compiler/ml/`
3. Implement type checking in `compiler/ml/`
4. Add Lambda IR handling in `compiler/core/lam_*`
5. Implement JS generation in `compiler/core/js_*`
6. Add comprehensive tests

### Debugging Compilation Issues

1. Identify which compilation phase has the issue
2. Use appropriate debugging flags (`-dparsetree`, `-dtypedtree`)
3. Check intermediate representations
4. Add debug output in relevant compiler modules
5. Verify with minimal test cases

### Working with Lambda IR

- Remember Lambda IR is the core optimization layer
- All `lam_*.ml` files process this representation
- Use `lam_print.ml` for debugging lambda expressions
- Test both with and without optimization passes
