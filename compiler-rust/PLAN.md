# ReScript Compiler: OCaml to Rust Rewrite Plan

## Executive Summary

This plan outlines a comprehensive, phased approach to rewriting the ReScript compiler from OCaml (~135K lines across 653 files) to Rust. The migration leverages existing Rust patterns from `rewatch/` (~8,500 lines) and **maintains critical backward compatibility** throughout the transition via FFI boundaries.

---

## Motivation

The rewrite is driven by multiple factors:

1. **Performance**: Rust's zero-cost abstractions and control over memory layout can improve compilation speed
2. **Rewatch Integration**: The build system (`rewatch/`) is already in Rust; unifying the toolchain eliminates OCaml/Rust boundaries and enables tighter integration
3. **Contributor Accessibility**: Rust has a larger developer community than OCaml, lowering the barrier to contributions
4. **Better Tooling**: Rust's ecosystem (cargo, rust-analyzer, clippy) provides superior development experience

## Constraints

- **CRITICAL**: Must be a drop-in replacement at all times - no breaking changes for users
- **CRITICAL**: No global state - compiler must support concurrent compilation of multiple files
- **Incremental migration** with FFI boundaries between OCaml and Rust components
- All existing tests must pass throughout the migration
- JavaScript output must remain byte-for-byte identical

---

## Current State Analysis

### Codebase Size by Component

| Component | Location | Lines | Files | Description |
|-----------|----------|-------|-------|-------------|
| Type Checker | `compiler/ml/` | 54,117 | 141 | OCaml compiler infrastructure, type checking |
| Lambda/JS IR | `compiler/core/` | 25,832 | 191 | Lambda IR, JS generation, optimizations |
| Parser | `compiler/syntax/` | 24,791 | 49 | ReScript parser (MIT licensed) |
| Utilities | `compiler/ext/` | 12,961 | 128 | Data structures, helpers |
| Frontend | `compiler/frontend/` | 9,072 | 78 | AST transformations, FFI processing |
| GenType | `compiler/gentype/` | 5,953 | 42 | TypeScript generation |
| Other | Various | 2,160 | 24 | Common, depends, bsc, jsoo |
| **Total** | | **~135,000** | **653** | |

### Largest Files (Migration Complexity Indicators)

| File | Lines | Component |
|------|-------|-----------|
| `res_core.ml` | 7,365 | Parser |
| `res_printer.ml` | 6,138 | Parser |
| `typecore.ml` | 4,802 | Type Checker |
| `ctype.ml` | 4,449 | Type Checker |
| `matching.ml` | 2,978 | Type Checker |
| `parmatch.ml` | 2,555 | Type Checker |
| `env.ml` | 2,141 | Type Checker |
| `lam_compile.ml` | 1,836 | Lambda IR |
| `js_exp_make.ml` | 1,780 | JS Generation |
| `js_dump.ml` | 1,654 | JS Generation |

### Compilation Pipeline

```
ReScript Source (.res)
       ↓
[compiler/syntax/] ─── Res_driver.parse
       ↓
Surface Syntax (Parsetree.t)
       ↓
[compiler/frontend/] ─── AST transformations, PPX
       ↓
[compiler/ml/] ─── Typecore.type_expression
       ↓
Typed AST (Typedtree.t)
       ↓
[compiler/ml/] ─── Translmod.transl_implementation
       ↓
Lambda IR (Lam.t)
       ↓
[compiler/core/lam_*] ─── Optimization passes
       ↓
[compiler/core/js_*] ─── Lam_compile_main.compile
       ↓
JavaScript IR (J.t)
       ↓
[compiler/core/js_dump*]
       ↓
JavaScript Output
```

### Existing Rust Infrastructure

The `rewatch/` directory (~8,500 lines) provides proven Rust patterns:
- **CLI**: `clap` for argument parsing
- **Config**: `serde` for JSON config parsing
- **Parallelism**: `rayon` for parallel compilation
- **Error handling**: `anyhow` for error propagation
- **Build state**: Arena-style data structures

### Rewatch Integration Benefits

Currently, `rewatch` spawns `bsc` (OCaml binary) as a subprocess for compilation. A unified Rust codebase enables:

1. **Direct library calls** instead of subprocess spawning (faster)
2. **Shared data structures** for build state and AST
3. **Better incremental compilation** with shared module graph
4. **Unified error handling** and diagnostics
5. **Single binary distribution** (simpler packaging)

The compiler Rust crate will be designed as a library that `rewatch` can link directly:

```rust
// In rewatch, after migration:
use rescript_compiler::{CompilationContext, CompilerConfig, ModuleCache};
use rayon::prelude::*;

fn compile_project(
    modules: Vec<ModulePath>,
    config: Arc<CompilerConfig>,
) -> Result<Vec<JsOutput>> {
    let cache = Arc::new(ModuleCache::new());

    // Compile all modules in parallel - no global state means this is safe!
    modules.par_iter().map(|module| {
        // Each compilation creates its own context
        let ctx = CompilationContext::new(config.clone(), cache.clone());
        ctx.compile_module(module)
    }).collect()
}
```

This enables significant speedups on multi-core machines - a key benefit of eliminating global state.

---

## Technical Approach

### Concurrency Architecture (No Global State)

**Key Requirement**: The compiler must support concurrent compilation of multiple files. This requires eliminating all global mutable state present in the OCaml implementation.

#### Global State in OCaml Compiler (to eliminate)

| OCaml Global | Purpose | Rust Alternative |
|--------------|---------|------------------|
| `Ident.currentstamp` | Unique identifier generation | Per-context `AtomicU32` or passed `IdGen` |
| `Ctype.current_level` | Type generalization level | Field in `TypeContext` |
| `Env.global_state` | Cached module information | Passed `CompilationEnv` |
| `Location.input_name` | Current file being compiled | Field in `ParseContext` |
| `Warnings.state` | Warning configuration | Field in `CompilerConfig` |
| `Config.*` | Compiler configuration | Immutable `CompilerConfig` |

See [GLOBAL_STATE_AUDIT.md](./GLOBAL_STATE_AUDIT.md) for a complete catalog of ~80+ global refs.

#### Compilation Context Design

All state is encapsulated in explicit context objects passed through the compilation pipeline:

```rust
/// Top-level compilation context - one per compilation unit
pub struct CompilationContext {
    /// Unique ID generator (thread-safe)
    id_gen: IdGenerator,
    /// Compiler configuration (immutable, shared)
    config: Arc<CompilerConfig>,
    /// Diagnostics collector
    diagnostics: DiagnosticsContext,
    /// Source file information
    source: SourceContext,
}

/// Type checking context - contains type arena and unification state
pub struct TypeContext<'ctx> {
    /// Arena for type expressions (not shared between compilations)
    arena: TypeArena<'ctx>,
    /// Current generalization level
    current_level: i32,
    /// Type environment (modules, values, types in scope)
    env: TypeEnv<'ctx>,
    /// Parent compilation context
    compilation: &'ctx CompilationContext,
}

/// Thread-safe identifier generator
pub struct IdGenerator {
    next_ident: AtomicU32,
    next_type_id: AtomicU32,
}

impl IdGenerator {
    pub fn fresh_ident(&self, name: &str) -> Ident {
        let stamp = self.next_ident.fetch_add(1, Ordering::Relaxed);
        Ident { name: name.into(), stamp }
    }

    pub fn fresh_type_id(&self) -> u32 {
        self.next_type_id.fetch_add(1, Ordering::Relaxed)
    }
}
```

#### Parallel Compilation with Rayon

With no global state, `rewatch` can compile files in parallel:

```rust
use rayon::prelude::*;

pub fn compile_project(
    modules: &[ModulePath],
    shared_config: Arc<CompilerConfig>,
    module_cache: Arc<ModuleCache>,  // Thread-safe cache of compiled interfaces
) -> Result<Vec<CompiledModule>> {
    // Compile independent modules in parallel
    modules
        .par_iter()
        .map(|module| {
            // Each compilation gets its own context - no shared mutable state
            let ctx = CompilationContext::new(
                shared_config.clone(),
                module_cache.clone(),
            );
            compile_module(&ctx, module)
        })
        .collect()
}
```

#### Thread-Safe Module Cache

For incremental compilation, compiled module interfaces are cached:

```rust
use dashmap::DashMap;

/// Thread-safe cache of compiled module interfaces
pub struct ModuleCache {
    /// Module path -> Compiled interface
    interfaces: DashMap<ModulePath, Arc<CompiledInterface>>,
    /// Dependency graph for invalidation
    dependencies: DashMap<ModulePath, Vec<ModulePath>>,
}

impl ModuleCache {
    /// Get or compile a module interface (thread-safe)
    pub fn get_or_compile(
        &self,
        path: &ModulePath,
        compile_fn: impl FnOnce() -> Result<CompiledInterface>,
    ) -> Result<Arc<CompiledInterface>> {
        // DashMap provides fine-grained locking
        self.interfaces
            .entry(path.clone())
            .or_try_insert_with(|| compile_fn().map(Arc::new))
            .map(|entry| entry.clone())
    }
}
```

#### Design Principles for Concurrency

1. **No `static mut`**: All mutable state is in explicit contexts
2. **No `thread_local!`**: Contexts are passed explicitly, not stored in TLS
3. **No `lazy_static!` with mutation**: Configuration is immutable after initialization
4. **Arena-per-compilation**: Each `TypeContext` owns its arena, no sharing
5. **Explicit dependency injection**: Modules receive dependencies, don't reach for globals
6. **`Send + Sync` where needed**: Shared data uses `Arc<T>` where `T: Send + Sync`

### OCaml → Rust Paradigm Mapping

| OCaml Feature | Rust Equivalent | Notes |
|---------------|-----------------|-------|
| Algebraic Data Types | `enum` | Direct mapping |
| Pattern Matching | `match` | Direct mapping |
| Mutable Records | `RefCell<T>` or arena | Careful design needed |
| Option Types | `Option<T>` | Direct mapping |
| Result Types | `Result<T, E>` | Direct mapping |
| GC | Ownership/Borrowing | Major paradigm shift |
| Functors | Traits + Generics | Different pattern |
| Polymorphic Variants | Tagged enums | Need careful design |

### Type Unification Strategy

**The Challenge**: OCaml uses mutable type variables with `Tlink` for sharing:
```ocaml
type type_expr = {mutable desc: type_desc; mutable level: int; id: int}
```

**Rust Solution**: Arena-based allocation with interior mutability, **owned by TypeContext** (not global):

```rust
use typed_arena::Arena;
use std::cell::{Cell, RefCell};

/// Type expression - allocated in TypeContext's arena
pub struct TypeExpr<'ctx> {
    desc: RefCell<TypeDesc<'ctx>>,
    level: Cell<i32>,
    id: u32,  // Unique within this context, from IdGenerator
}

pub enum TypeDesc<'ctx> {
    Tvar(Option<String>),
    Tarrow { arg: Arg<'ctx>, ret: &'ctx TypeExpr<'ctx> },
    Ttuple(Vec<&'ctx TypeExpr<'ctx>>),
    Tconstr(Path, Vec<&'ctx TypeExpr<'ctx>>),
    Tlink(&'ctx TypeExpr<'ctx>),  // Union-find linking
    // ...
}

/// Type arena owned by TypeContext - not shared between compilations
/// This is NOT Send/Sync - each thread has its own TypeContext
pub struct TypeArena<'ctx> {
    arena: Arena<TypeExpr<'ctx>>,
    id_gen: &'ctx IdGenerator,  // Borrows from CompilationContext
}

impl<'ctx> TypeArena<'ctx> {
    pub fn alloc(&'ctx self, desc: TypeDesc<'ctx>, level: i32) -> &'ctx TypeExpr<'ctx> {
        self.arena.alloc(TypeExpr {
            desc: RefCell::new(desc),
            level: Cell::new(level),
            id: self.id_gen.fresh_type_id(),
        })
    }
}
```

**Key Point**: `RefCell` provides interior mutability for unification but is NOT thread-safe. This is fine because each `TypeContext` is used by a single thread. Parallelism happens at the file level, not within type checking.

### FFI Strategy During Transition

Use serialization-based boundaries between OCaml and Rust components:

```rust
// Rust FFI export
#[no_mangle]
pub extern "C" fn parse_implementation(
    source: *const c_char,
    len: usize,
    out_buf: *mut u8,
    out_len: *mut usize,
) -> i32 {
    let result = parse(source);
    let bytes = bincode::serialize(&result).unwrap();
    // Copy to buffer...
}
```

```ocaml
(* OCaml FFI import *)
external parse_implementation_ffi :
  string -> bytes -> int ref -> int = "parse_implementation"
```

For performance-critical paths, consider `ocaml-rs` for direct memory sharing.

---

## Migration Phases

### Phase 1: Foundation Layer
**Scope**: ~13K lines from `compiler/ext/`
**Risk**: LOW
**Goal**: Establish Rust infrastructure, patterns, and FFI

**Pre-work: Global State Audit**

Before migrating, audit OCaml codebase for all global mutable state:

```bash
# Find all mutable global references
grep -rn "let.*=.*ref" compiler/
grep -rn "mutable" compiler/ml/*.ml
grep -rn "Hashtbl.create" compiler/  # Often used as global caches
```

Document each instance and plan how to thread it through contexts.

**Files to migrate (in order)**:
1. `ext/ident.ml` → Core identifier type (with `IdGenerator` instead of global stamp)
2. `ext/ext_list.ml`, `ext_string.ml`, `ext_array.ml` → Utilities
3. `ext/set_ident.ml`, `hash_ident.ml` → Collections
4. `ext/ext_json*.ml` → JSON utilities
5. Remaining utility modules

**Deliverables**:
- Rust crate structure established
- Core types (`Ident`, `Location`, `Path`) in Rust
- `CompilationContext` and `IdGenerator` infrastructure
- Working FFI boundary
- CI pipeline for Rust components (including ThreadSanitizer)
- Testing infrastructure with parallel execution tests

### Phase 2: Parser
**Scope**: ~25K lines from `compiler/syntax/`
**Risk**: MEDIUM
**Goal**: Self-contained ReScript parser in Rust

**Key Files to Migrate**:

| OCaml File | Lines | Rust Module | Notes |
|------------|-------|-------------|-------|
| `res_token.ml` | 200 | `parser/token.rs` | Token enum, straightforward |
| `res_scanner.ml` | 1,200 | `parser/scanner.rs` | Lexer, character-by-character |
| `res_grammar.ml` | 150 | `parser/grammar.rs` | Grammar types |
| `res_core.ml` | 7,365 | `parser/core.rs` | **Largest file** - main parser logic |
| `res_parser.ml` | 800 | `parser/mod.rs` | Parser state and entry points |
| `res_printer.ml` | 6,138 | `parser/printer.rs` | Pretty printer |
| `res_parsetree.ml` | 600 | `parsetree.rs` | AST types |
| `res_comments_table.ml` | 400 | `parser/comments.rs` | Comment attachment |
| `jsx_v4.ml` | 1,500 | `parser/jsx.rs` | JSX transformation |
| `res_diagnostics.ml` | 500 | `parser/diagnostics.rs` | Parser error messages |

**Challenges**:
1. **res_core.ml is 7,365 lines** - Recursive descent parser with complex precedence handling
2. **Comment preservation** - Must track comments for formatting
3. **Error recovery** - Parser continues after errors to report multiple issues
4. **JSX transformation** - Complex desugaring to function calls

**Approach**:
1. Start with token definitions and lexer (testable independently)
2. Define `Parsetree` types in Rust matching OCaml exactly
3. Implement parser incrementally, testing each production
4. Use golden tests comparing AST output between OCaml and Rust
5. Implement printer last (needed for roundtrip tests)

**Testing Strategy**:
```bash
# For each .res file in tests/syntax_tests/
./ocaml-bsc -dparsetree file.res > expected.txt
./rust-bsc -dparsetree file.res > actual.txt
diff expected.txt actual.txt
```

**Deliverables**:
- Complete Rust parser producing identical AST
- Golden tests comparing OCaml/Rust parser output
- PPX compatibility layer (`parsetree0` ↔ Rust AST)
- Parser benchmarks showing performance parity or better
- **Binary AST byte-for-byte parity** (see below)

### Binary AST Byte Parity (Critical for Drop-in Replacement)

The `-bs-ast` flag produces binary AST files (`.ast`, `.iast`) that downstream tools consume. For a true drop-in replacement, these files must be **byte-for-byte identical** to OCaml's output.

**Current Status**: 100% dependency parity, ~0% byte-identical (due to different sharing patterns)

**Root Cause**: OCaml's Marshal uses **pointer-based sharing** (same memory address → shared reference) while Rust uses **content-based sharing** (same content → shared reference).

#### How OCaml Marshal Sharing Works

```
Object 0: Position { file="test.res", line=1, col=0 }
Object 1: Position { file="test.res", line=1, col=5 }  ← Different memory, not shared
Object 2: Location { start=obj0, end=obj1 }
Object 3: Location { start=obj0, end=obj1 }            ← Same objects, SHARES obj2
```

#### Key Discovery: Position Sharing via `prev_end_pos`

In OCaml's parser (`res_parser.ml`):
```ocaml
let next p =
  p.prev_end_pos <- p.end_pos;  (* SAME OBJECT reference! *)
  let start_pos, end_pos, token = Scanner.scan p.scanner in
  p.start_pos <- start_pos;     (* NEW object from scanner *)
  p.end_pos <- end_pos          (* NEW object from scanner *)
```

When token B follows token A, `prev_end_pos` for B is the **same object** as `end_pos` for A.

#### Implementation Plan: Identity-Based Sharing

1. **Add `PositionId` to Position struct**
   ```rust
   #[derive(Clone, Copy, PartialEq, Eq, Hash)]
   pub struct PositionId(u32);

   pub struct Position {
       pub file_name: String,
       pub line: usize,
       pub bol: usize,
       pub cnum: usize,
       pub id: PositionId,  // Unique identity for sharing
   }
   ```

2. **Update scanner to assign unique IDs**
   ```rust
   impl Scanner {
       fn make_position(&mut self) -> Position {
           self.position_counter += 1;
           Position {
               file_name: self.filename.clone(),
               line: self.line,
               bol: self.line_offset,
               cnum: self.offset,
               id: PositionId(self.position_counter),
           }
       }
   }
   ```

3. **Preserve ID in parser's `next()`**
   ```rust
   impl Parser {
       pub fn next(&mut self) {
           // Key: prev_end_pos gets the SAME id as current end_pos
           self.prev_end_pos = self.end_pos.clone();  // Same id!

           let (start_pos, end_pos) = self.scanner.scan();
           self.start_pos = start_pos;  // New id from scanner
           self.end_pos = end_pos;      // New id from scanner
       }
   }
   ```

4. **Update marshal to share by identity**
   ```rust
   // Change from content-based to identity-based:
   position_table: HashMap<PositionId, u32>,  // Instead of HashMap<(file,line,bol,cnum), u32>
   ```

#### Implementation Tasks

- [ ] Add `PositionId` type to `location.rs`
- [ ] Add `id` field to `Position` struct
- [ ] Update scanner to assign unique IDs on position creation
- [ ] Update parser's `next()` to preserve ID when assigning `prev_end_pos`
- [ ] Update marshal `position_table` to key by `PositionId`
- [ ] Run parity test suite: `./scripts/test_ast_parity_suite.sh`
- [ ] Target: 100% byte-identical for parseable files

#### Parity Test Suite

```bash
# Run comprehensive parity test
./scripts/test_ast_parity_suite.sh --verbose --limit 100

# Compare specific file
./compiler-rust/target/debug/bsc -bs-ast test.res -o /tmp/rust.ast
packages/@rescript/darwin-arm64/bin/bsc.exe -bs-ast test.res -o /tmp/ocaml.ast
xxd /tmp/rust.ast > /tmp/rust.hex
xxd /tmp/ocaml.ast > /tmp/ocaml.hex
diff /tmp/rust.hex /tmp/ocaml.hex
```

### Phase 3: Lambda IR & JS Generation
**Scope**: ~26K lines from `compiler/core/`
**Risk**: MEDIUM
**Goal**: Code generation backend in Rust

**Key Files to Migrate**:

| OCaml File | Lines | Rust Module | Notes |
|------------|-------|-------------|-------|
| `lam.ml` | 800 | `lambda/mod.rs` | Core Lambda IR definition |
| `lam_primitive.ml` | 600 | `lambda/primitive.rs` | Primitive operations |
| `lam_constant.ml` | 300 | `lambda/constant.rs` | Constant values |
| `lam_arity.ml` | 200 | `lambda/arity.rs` | Function arity tracking |
| `lam_compile.ml` | 1,836 | `codegen/compile.rs` | **Core** - TypedTree → Lambda |
| `lam_compile_main.ml` | 600 | `codegen/main.rs` | Compilation entry point |
| `lam_pass_*.ml` | 2,500 | `lambda/passes/*.rs` | Optimization passes |
| `j.ml` | 800 | `js_ir/mod.rs` | JavaScript IR definition |
| `js_op.ml` | 400 | `js_ir/ops.rs` | JS operators |
| `js_dump.ml` | 1,654 | `js_ir/dump.rs` | **Critical** - JS code generation |
| `js_exp_make.ml` | 1,780 | `js_ir/exp_make.rs` | JS expression builders |
| `js_pass_*.ml` | 1,500 | `js_ir/passes/*.rs` | JS optimization passes |

**Lambda Optimization Passes** (must preserve semantics exactly):
- `lam_pass_alpha_conversion.ml` - Variable renaming
- `lam_pass_exits.ml` - Exit/static-raise optimization
- `lam_pass_lets_dce.ml` - Dead code elimination
- `lam_pass_remove_alias.ml` - Alias removal
- `lam_beta_reduce.ml` - Beta reduction
- `lam_eta_conversion.ml` - Eta expansion/reduction
- `lam_inline_pass.ml` - Function inlining

**JavaScript Passes**:
- `js_pass_flatten.ml` - Flatten nested expressions
- `js_pass_flatten_and_mark_dead.ml` - DCE for JS
- `js_pass_scope.ml` - Scope analysis
- `js_pass_tailcall_inline.ml` - Tail call optimization

**Challenges**:
1. **Optimization correctness** - Each pass must preserve program semantics
2. **JS output fidelity** - Output must be byte-for-byte identical
3. **Block tagging** - ReScript's variant representation
4. **Module bundling** - Cross-module references and imports

**Approach**:
1. Define Lambda IR types first (clear, well-documented)
2. Define JS IR types
3. Implement `lam_compile.ml` (TypedTree → Lambda) using FFI to get TypedTree from OCaml
4. Implement optimization passes one by one, testing each
5. Implement `js_dump.ml` last - compare output byte-by-byte

**Testing Strategy**:
```bash
# For each .res file in tests/tests/
./ocaml-bsc file.res -o expected.js
./rust-bsc file.res -o actual.js
diff expected.js actual.js  # Must be identical!
```

**Deliverables**:
- Lambda IR in Rust
- JavaScript IR in Rust
- All optimization passes migrated
- Identical JavaScript output (verified by snapshots)
- Comprehensive pass-by-pass tests

### Phase 4: Type Checker (Hardest Phase)
**Scope**: ~54K lines from `compiler/ml/`
**Risk**: HIGH
**Goal**: Complete type system in Rust

**Key Files to Migrate**:

| OCaml File | Lines | Rust Module | Notes |
|------------|-------|-------------|-------|
| `types.ml` | 800 | `types/mod.rs` | **Critical** - Type representation |
| `btype.ml` | 600 | `types/btype.rs` | Basic type operations |
| `ctype.ml` | 4,449 | `types/ctype.rs` | **Hardest** - Unification algorithm |
| `env.ml` | 2,141 | `types/env.rs` | Type environment |
| `typecore.ml` | 4,802 | `typechecker/core.rs` | **Core** - Expression type checking |
| `typedecl.ml` | 1,500 | `typechecker/decl.rs` | Type declaration checking |
| `typemod.ml` | 2,000 | `typechecker/mod.rs` | Module type checking |
| `matching.ml` | 2,978 | `typechecker/matching.rs` | Pattern compilation |
| `parmatch.ml` | 2,555 | `typechecker/parmatch.rs` | Exhaustiveness checking |
| `typetexp.ml` | 800 | `typechecker/typetexp.rs` | Type expression checking |
| `includecore.ml` | 600 | `typechecker/include.rs` | Signature inclusion |
| `includemod.ml` | 800 | `typechecker/include_mod.rs` | Module inclusion |
| `subst.ml` | 500 | `types/subst.rs` | Type substitution |
| `path.ml` | 300 | `types/path.rs` | Module paths |
| `predef.ml` | 400 | `types/predef.rs` | Predefined types |
| `printtyp.ml` | 1,200 | `types/print.rs` | Type printing for errors |

**Type System Challenges**:

1. **Mutable Unification** - OCaml uses mutable `type_expr` with `Tlink`:
   ```ocaml
   type type_expr = {mutable desc: type_desc; mutable level: int; id: int}
   ```
   Rust solution: Arena + `RefCell` (see Technical Approach section)

2. **Level-based Generalization** - Type variables are generalized based on level:
   - `current_level` tracks binding depth
   - Variables with level > current can be generalized
   - Requires careful threading through `TypeContext`

3. **Row Polymorphism** - Polymorphic variants and objects:
   - Open/closed rows
   - Row variables
   - Complex subtyping

4. **GADT Support** - Generalized algebraic data types:
   - Local type refinement
   - Existential types
   - Complex constraint solving

5. **Module System** - First-class modules:
   - Functors
   - Applicative vs generative
   - Signature matching

**Migration Order** (based on dependencies):

```
                    ┌─────────────┐
                    │  predef.ml  │
                    └──────┬──────┘
                           │
                    ┌──────▼──────┐
                    │   path.ml   │
                    └──────┬──────┘
                           │
                    ┌──────▼──────┐
                    │  types.ml   │◄──── Start here
                    └──────┬──────┘
                           │
              ┌────────────┼────────────┐
              │            │            │
       ┌──────▼──────┐ ┌───▼───┐ ┌──────▼──────┐
       │  btype.ml   │ │subst.ml│ │printtyp.ml │
       └──────┬──────┘ └───┬───┘ └─────────────┘
              │            │
              └─────┬──────┘
                    │
             ┌──────▼──────┐
             │  ctype.ml   │◄──── Hardest
             └──────┬──────┘
                    │
             ┌──────▼──────┐
             │   env.ml    │
             └──────┬──────┘
                    │
    ┌───────────────┼───────────────┐
    │               │               │
┌───▼────┐    ┌─────▼─────┐    ┌────▼────┐
│typecore│    │ typedecl  │    │ typemod │
└────────┘    └───────────┘    └─────────┘
```

**Testing Strategy**:

1. **Unit tests for unification**:
   ```rust
   #[test]
   fn test_unify_int_int() {
       let ctx = TypeContext::new();
       let t1 = ctx.new_type(Tconstr(path_int, vec![]));
       let t2 = ctx.new_type(Tconstr(path_int, vec![]));
       assert!(ctx.unify(t1, t2).is_ok());
   }
   ```

2. **Property-based testing**:
   ```rust
   proptest! {
       #[test]
       fn unify_is_symmetric(t1: TypeExpr, t2: TypeExpr) {
           let r1 = unify(t1.clone(), t2.clone());
           let r2 = unify(t2, t1);
           assert_eq!(r1.is_ok(), r2.is_ok());
       }
   }
   ```

3. **Oracle comparison** - Compare with OCaml type checker:
   ```bash
   # Type check and compare inferred types
   ./ocaml-bsc -dtypedtree file.res > expected.txt
   ./rust-bsc -dtypedtree file.res > actual.txt
   diff expected.txt actual.txt
   ```

**Deliverables**:
- Complete type checker in Rust
- Type inference identical to OCaml
- Error messages comparable quality
- Module system fully working
- Extensive property-based test suite

### Phase 5: Frontend & Integration
**Scope**: ~9K lines from `compiler/frontend/` + ~6K from `compiler/gentype/` + integration
**Risk**: MEDIUM
**Goal**: Complete integration, remove OCaml

**Key Files to Migrate**:

| OCaml File | Lines | Rust Module | Notes |
|------------|-------|-------------|-------|
| **Frontend (AST Transformations)** |
| `ast_attributes.ml` | 600 | `frontend/attributes.rs` | Attribute parsing |
| `ast_core_type.ml` | 400 | `frontend/core_type.rs` | Type annotation handling |
| `ast_derive_*.ml` | 1,500 | `frontend/derive/*.rs` | Derive macros |
| `ast_external_process.ml` | 800 | `frontend/external.rs` | FFI/external handling |
| `ast_pat.ml` | 300 | `frontend/pattern.rs` | Pattern transformations |
| `ast_exp.ml` | 400 | `frontend/exp.rs` | Expression transformations |
| `bs_builtin_ppx.ml` | 1,200 | `frontend/ppx.rs` | Built-in PPX |
| **GenType (TypeScript Generation)** |
| `GenTypeMain.ml` | 800 | `gentype/mod.rs` | GenType entry point |
| `TranslateTypeDeclarations.ml` | 600 | `gentype/type_decl.rs` | Type → TS |
| `TranslateSignature.ml` | 500 | `gentype/signature.rs` | Signature → TS |
| `EmitType.ml` | 400 | `gentype/emit.rs` | TS code emission |
| **Binary/CLI** |
| `rescript_compiler_main.ml` | 300 | `main.rs` | CLI entry point |

**Integration Tasks**:

1. **Remove FFI Boundaries**
   - Each phase added FFI for incremental migration
   - Now remove serialization overhead
   - Direct Rust calls between components

2. **Unify with Rewatch**
   ```rust
   // rewatch/src/build/compile.rs - before
   fn compile_module(module: &Module) -> Result<()> {
       Command::new("bsc")  // Spawns OCaml binary
           .args(&module.bsc_args())
           .spawn()?
   }

   // After Phase 5
   fn compile_module(module: &Module, config: &CompilerConfig) -> Result<()> {
       let ctx = CompilationContext::new(config);
       rescript_compiler::compile(&ctx, &module.source)?  // Direct call!
   }
   ```

3. **Single Binary Distribution**
   - `rescript` binary includes compiler
   - No separate `bsc` needed
   - Simpler installation

4. **Performance Optimization**
   - Profile hot paths
   - Optimize memory allocation
   - Consider SIMD for lexer
   - Parallel optimization passes (if safe)

**PPX Compatibility**:

PPX plugins compile against `parsetree0.ml`. Options:

1. **Maintain compatibility layer**:
   ```rust
   // Convert Rust Parsetree → OCaml parsetree0
   pub fn to_parsetree0(ast: &Parsetree) -> Vec<u8> {
       // Serialize to OCaml-compatible format
   }
   ```

2. **Deprecate OCaml PPX** (long term):
   - Provide Rust PPX API
   - Migration guide for PPX authors

**GenType Considerations**:

GenType generates TypeScript definitions. Challenges:
- Reads from `.cmt` files (typed tree)
- Need to either:
  - Output compatible `.cmt` format, OR
  - Rewrite GenType to use Rust types directly

**Testing Strategy**:

1. **Full compilation test**:
   ```bash
   # Compile entire ReScript stdlib
   ./rust-rescript build

   # Compare all outputs
   diff -r lib/ocaml lib/rust
   ```

2. **Performance benchmarks**:
   ```bash
   hyperfine './ocaml-rescript build' './rust-rescript build'
   ```

3. **Real-world projects**:
   - Compile major ReScript projects
   - Run their test suites
   - Check for any behavioral differences

**Cleanup Tasks**:

- [ ] Remove `compiler/` OCaml code
- [ ] Remove OCaml build dependencies from CI
- [ ] Update installation instructions
- [ ] Update contribution guide for Rust
- [ ] Archive OCaml codebase for reference

**Deliverables**:
- Single `bsc` binary in Rust
- Integrated with `rewatch` for single `rescript` binary
- All OCaml code removed
- Performance parity or better (target: 2x faster)
- Complete documentation
- Migration guide for PPX authors

---

## Key Data Structure Designs

### Parsetree (Surface Syntax)

```rust
// src/parsetree.rs

#[derive(Debug, Clone)]
pub struct Expression {
    pub desc: ExpressionDesc,
    pub loc: Location,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub enum ExpressionDesc {
    Ident(Longident),
    Constant(Constant),
    Let { rec_flag: RecFlag, bindings: Vec<ValueBinding>, body: Box<Expression> },
    Fun { arg_label: ArgLabel, default: Option<Box<Expression>>,
          pattern: Pattern, body: Box<Expression>, arity: Option<i32> },
    Apply { func: Box<Expression>, args: Vec<(ArgLabel, Expression)> },
    Match { expr: Box<Expression>, cases: Vec<Case> },
    Tuple(Vec<Expression>),
    Construct { path: Longident, arg: Option<Box<Expression>> },
    Record { fields: Vec<RecordField>, base: Option<Box<Expression>> },
    IfThenElse { cond: Box<Expression>, then_: Box<Expression>,
                 else_: Option<Box<Expression>> },
    // ... other variants
}
```

### Lambda IR

```rust
// src/lambda.rs

#[derive(Debug, Clone)]
pub enum Lambda {
    Var(Ident),
    GlobalModule { id: Ident, dynamic_import: bool },
    Const(LamConstant),
    Apply { func: Box<Lambda>, args: Vec<Lambda>, info: ApInfo },
    Function { arity: i32, params: Vec<Ident>, body: Box<Lambda> },
    Let { kind: LetKind, id: Ident, value: Box<Lambda>, body: Box<Lambda> },
    LetRec { bindings: Vec<(Ident, Lambda)>, body: Box<Lambda> },
    Prim { primitive: LamPrimitive, args: Vec<Lambda>, loc: Location },
    Switch { arg: Box<Lambda>, sw: LambdaSwitch },
    IfThenElse { cond: Box<Lambda>, then_: Box<Lambda>, else_: Box<Lambda> },
    TryWith { body: Box<Lambda>, exn: Ident, handler: Box<Lambda> },
    Sequence { first: Box<Lambda>, second: Box<Lambda> },
    // ... other variants
}
```

### JavaScript IR

```rust
// src/js_ir.rs

#[derive(Debug, Clone)]
pub enum JsExpression {
    Var(VIdent),
    Literal(JsLiteral),
    Call { func: Box<JsExpression>, args: Vec<JsExpression>, info: CallInfo },
    Fun { params: Vec<Ident>, body: JsBlock, is_async: bool },
    Array { elements: Vec<JsExpression>, mutable: bool },
    Object { properties: Vec<(PropertyName, JsExpression)> },
    Cond { test: Box<JsExpression>, then_: Box<JsExpression>,
           else_: Box<JsExpression> },
    Bin { op: BinOp, left: Box<JsExpression>, right: Box<JsExpression> },
    StaticIndex { obj: Box<JsExpression>, field: String },
    Await(Box<JsExpression>),
    // ... other variants
}

pub type JsBlock = Vec<JsStatement>;

#[derive(Debug, Clone)]
pub struct JsProgram {
    pub block: JsBlock,
    pub exports: Exports,
}
```

---

## Backward Compatibility Strategy

**CRITICAL REQUIREMENT**: The compiler must remain a drop-in replacement at all times.

### Compatibility Guarantees

1. **CLI interface unchanged**: Same flags, same behavior
2. **rescript.json compatibility**: All configuration options work identically
3. **JavaScript output**: Byte-for-byte identical (verified by CI)
4. **Error messages**: Same format, same locations
5. **PPX support**: Existing PPX plugins continue working

### Feature Flags for Gradual Rollout

During migration, environment variables control which implementation is used:

```bash
# Use OCaml parser (default during Phase 2)
RESCRIPT_PARSER=ocaml rescript build

# Use Rust parser (opt-in testing)
RESCRIPT_PARSER=rust rescript build
```

This allows:
- Gradual rollout to users
- Easy bisection when issues arise
- A/B testing for performance comparison
- Quick rollback if problems are detected

---

## Risk Mitigation

| Risk | Impact | Mitigation |
|------|--------|------------|
| Type unification bugs | HIGH | Property-based testing with `proptest`, test oracle comparing OCaml/Rust |
| JS output regression | HIGH | Golden snapshot tests for all test files, diff-based CI |
| Performance degradation | MEDIUM | Benchmarks with `criterion`, performance gates in CI |
| FFI overhead | MEDIUM | Minimize crossings, use bincode, profile hot paths |
| PPX compatibility | MEDIUM | Maintain `parsetree0.ml`, bidirectional AST conversion |
| Module system complexity | HIGH | Extensive testing, gradual migration with careful verification |
| Breaking user code | CRITICAL | Feature flags, identical output verification, extensive real-world testing |
| Hidden global state | HIGH | `cargo clippy` lints, code review checklist, `static` audit |
| Data races | CRITICAL | ThreadSanitizer in CI, `cargo miri`, stress testing with parallel compilation |
| Non-deterministic output | HIGH | Run tests with varied thread counts, compare outputs across runs |

---

## Success Criteria

### Correctness
- [ ] All existing tests pass (syntax, ounit, build, integration)
- [ ] JavaScript output byte-for-byte identical for all test cases
- [ ] Type inference produces identical types
- [ ] Error messages maintain quality

### Concurrency
- [ ] No global mutable state (verified by code review and `cargo clippy`)
- [ ] Compiling same file concurrently produces identical results
- [ ] No data races (verified by `cargo miri test` and ThreadSanitizer)
- [ ] Linear speedup with parallel file compilation (benchmarked)

### Performance
- [ ] Compilation time equal or better than OCaml
- [ ] Memory usage within 20% of OCaml
- [ ] No regression in incremental build times
- [ ] Parallel compilation shows >2x speedup on multi-core machines

### Code Quality
- [ ] Follows patterns established in `rewatch/`
- [ ] Documentation coverage >80%
- [ ] Test coverage >85%
- [ ] `unsafe` only at FFI boundaries
- [ ] All public APIs are `Send + Sync` safe or documented as single-threaded

---

## Testing Strategy

This section details how to leverage the **existing test infrastructure** (~5,000+ test files, ~1,500+ golden snapshots) for validating the Rust implementation against the OCaml baseline.

### Existing Test Assets Summary

| Test Category | Files | Golden Snapshots | Location |
|--------------|-------|------------------|----------|
| Syntax/Parser | 1,827 | 511+ `.txt` | `tests/syntax_tests/data/` |
| Type Errors | 722 | 247+ `.expected` | `tests/build_tests/super_errors/` |
| Integration | 1,349 | 658 `.mjs` outputs | `tests/tests/src/` |
| Tools | 203 | 321+ `.expected` | `tests/tools_tests/` |
| GenType | 291 | `.gen.tsx` outputs | `tests/gentype_tests/` |
| Analysis | 364 | Various | `tests/analysis_tests/` |
| Build System | 35+ | `input.js` validators | `tests/build_tests/*/` |

---

### Phase 1: Foundation Testing

**Goal**: Verify core types (`Ident`, `Location`, `Path`) match OCaml behavior.

#### Unit Tests (New)
```rust
// compiler-rust/tests/ident_tests.rs
#[test]
fn test_ident_same_semantics() {
    let gen = IdGenerator::new();
    let id1 = gen.create("x");
    let id2 = gen.create("x");

    // Match OCaml semantics: same name, different stamps
    assert!(id1.equal(&id2));      // Same name
    assert!(!id1.same(&id2));      // Different identifiers
    assert!(id1.stamp() < id2.stamp()); // Stamps increase
}
```

#### Concurrency Tests (New)
```rust
#[test]
fn test_parallel_id_generation() {
    let gen = Arc::new(IdGenerator::new());
    let handles: Vec<_> = (0..100).map(|_| {
        let gen = gen.clone();
        std::thread::spawn(move || {
            (0..1000).map(|i| gen.create(&format!("x{}", i))).collect::<Vec<_>>()
        })
    }).collect();

    let all_ids: Vec<_> = handles.into_iter().flat_map(|h| h.join().unwrap()).collect();
    let stamps: HashSet<_> = all_ids.iter().map(|id| id.stamp()).collect();

    // All 100,000 stamps must be unique
    assert_eq!(stamps.len(), 100_000);
}
```

---

### Phase 2: Parser Testing

**Goal**: Rust parser produces **identical AST** to OCaml parser for all 1,827 syntax test files.

#### Reuse Existing Golden Files

The existing syntax tests generate `.txt` expected files. We can compare Rust output directly:

```bash
# Existing OCaml golden files
tests/syntax_tests/data/parsing/grammar/expressions/expected/ternary.res.txt
tests/syntax_tests/data/printer/expr/expected/apply.res.txt
tests/syntax_tests/data/parsing/errors/expected/scanner/*.res.txt
```

#### Parser Golden Test Script
```bash
#!/bin/bash
# scripts/test_rust_parser.sh

RUST_BSC="./target/release/bsc"
DIFF_COUNT=0

for res_file in tests/syntax_tests/data/**/*.res; do
    expected="${res_file%.res}.res.txt"
    if [[ -f "$expected" ]]; then
        # Parse with Rust compiler
        $RUST_BSC -dparsetree "$res_file" 2>&1 > /tmp/rust_output.txt

        # Compare with existing OCaml-generated expected file
        if ! diff -q "$expected" /tmp/rust_output.txt > /dev/null; then
            echo "DIFF: $res_file"
            diff "$expected" /tmp/rust_output.txt | head -20
            ((DIFF_COUNT++))
        fi
    fi
done

echo "Total differences: $DIFF_COUNT"
exit $DIFF_COUNT
```

#### Roundtrip Testing (Idempotency)

Leverage existing roundtrip test infrastructure:

```bash
# Existing test: scripts/test_syntax.sh with ROUNDTRIP_TEST=1
# Flow: Parse → Print → Parse → Print → Verify Text1 == Text2

# For Rust: Same flow, compare with OCaml roundtrip output
for res_file in tests/syntax_tests/data/idempotency/*.res; do
    # OCaml roundtrip
    ./cli/bsc.js -pp "$res_file" > /tmp/ocaml_roundtrip.res

    # Rust roundtrip
    ./target/release/bsc -pp "$res_file" > /tmp/rust_roundtrip.res

    diff /tmp/ocaml_roundtrip.res /tmp/rust_roundtrip.res
done
```

#### Error Recovery Testing

Test parser error messages match:

```bash
# Existing error test files
tests/syntax_tests/data/parsing/errors/scanner/*.res
tests/syntax_tests/data/parsing/recovery/*.res

# Compare error output
for err_file in tests/syntax_tests/data/parsing/errors/**/*.res; do
    expected="${err_file%.res}.res.txt"

    ./target/release/bsc -dparsetree "$err_file" 2>&1 > /tmp/rust_errors.txt
    diff "$expected" /tmp/rust_errors.txt
done
```

#### Parser Benchmark Comparison

Use existing benchmarks:

```bash
# Existing: tests/syntax_benchmarks/
# Files: RedBlackTree.res, RedBlackTreeNoComments.res, Napkinscript.res

hyperfine \
    './cli/bsc.js -dparsetree tests/syntax_benchmarks/RedBlackTree.res' \
    './target/release/bsc -dparsetree tests/syntax_benchmarks/RedBlackTree.res'
```

---

### Phase 3: Lambda/JS IR Testing

**Goal**: Rust backend produces **byte-for-byte identical JavaScript** for all 658 integration test files.

#### Reuse Integration Test Outputs

The existing tests compile `.res` files and run them:

```bash
# Existing compiled outputs
tests/tests/src/*.mjs  # 658+ JavaScript output files

# These are executed by Mocha tests - same files test Rust output
```

#### JavaScript Output Comparison Script
```bash
#!/bin/bash
# scripts/test_rust_codegen.sh

mkdir -p /tmp/rust_outputs /tmp/ocaml_outputs

for res_file in tests/tests/src/*.res; do
    basename=$(basename "$res_file" .res)

    # Compile with OCaml
    ./cli/bsc.js "$res_file" -o "/tmp/ocaml_outputs/${basename}.mjs"

    # Compile with Rust
    ./target/release/bsc "$res_file" -o "/tmp/rust_outputs/${basename}.mjs"
done

# Byte-for-byte comparison
diff -r /tmp/ocaml_outputs /tmp/rust_outputs
```

#### Lambda IR Comparison

Add debug output for Lambda IR comparison:

```bash
# New test: compare -drawlambda output
for res_file in tests/tests/src/*.res; do
    ./cli/bsc.js -drawlambda "$res_file" > /tmp/ocaml_lambda.txt
    ./target/release/bsc -drawlambda "$res_file" > /tmp/rust_lambda.txt
    diff /tmp/ocaml_lambda.txt /tmp/rust_lambda.txt
done
```

#### Optimization Pass Testing

Test each optimization pass independently:

```rust
// compiler-rust/tests/lambda_passes.rs

#[test]
fn test_dce_pass() {
    let input = parse_lambda(r#"
        let unused = 1 in
        let used = 2 in
        used
    "#);

    let output = lam_pass_dce(input);

    // Verify unused binding is removed
    assert!(!output.has_binding("unused"));
    assert!(output.has_binding("used"));
}
```

---

### Phase 4: Type Checker Testing

**Goal**: Type inference identical to OCaml, error messages match 247+ golden snapshots.

#### Reuse Super Errors Test Suite

The existing super_errors tests capture error messages:

```bash
# Existing golden files
tests/build_tests/super_errors/expected/*.expected

# Example content (optional_record_field_missing_case.res.expected):
#   File "fixtures/optional_record_field_missing_case.res", line 5, characters 2-14:
#   5 |   | {x: None} => 1
#         ^^^^^^^^^^^^
#   Error: This pattern matches values of type {...}
#   but a pattern was expected which matches values of type {...}
```

#### Type Error Comparison Script
```bash
#!/bin/bash
# scripts/test_rust_type_errors.sh

cd tests/build_tests/super_errors

for fixture in fixtures/*.res; do
    expected="expected/$(basename "$fixture").expected"

    # Compile with Rust, capture stderr
    ../../../target/release/bsc "$fixture" 2>&1 | \
        # Normalize paths (existing normalization logic)
        sed 's|/.*/fixtures/|fixtures/|g' > /tmp/rust_error.txt

    if ! diff -q "$expected" /tmp/rust_error.txt > /dev/null; then
        echo "DIFF: $fixture"
        diff "$expected" /tmp/rust_error.txt
    fi
done
```

#### Typed Tree Comparison

Compare `-dtypedtree` output:

```bash
for res_file in tests/tests/src/*.res; do
    ./cli/bsc.js -dtypedtree "$res_file" 2>/dev/null > /tmp/ocaml_typed.txt
    ./target/release/bsc -dtypedtree "$res_file" 2>/dev/null > /tmp/rust_typed.txt

    # Compare inferred types (normalize for minor formatting differences)
    diff <(sort /tmp/ocaml_typed.txt) <(sort /tmp/rust_typed.txt)
done
```

#### Property-Based Type Testing

```rust
// compiler-rust/tests/type_properties.rs
use proptest::prelude::*;

proptest! {
    #[test]
    fn unify_reflexive(ty in arb_type()) {
        let ctx = TypeContext::new();
        let t1 = ctx.instantiate(&ty);
        let t2 = ctx.instantiate(&ty);
        prop_assert!(ctx.unify(t1, t2).is_ok());
    }

    #[test]
    fn unify_symmetric(ty1 in arb_type(), ty2 in arb_type()) {
        let ctx = TypeContext::new();
        let r1 = ctx.unify(ty1.clone(), ty2.clone());
        let r2 = ctx.unify(ty2, ty1);
        prop_assert_eq!(r1.is_ok(), r2.is_ok());
    }
}
```

---

### Phase 5: Integration Testing

**Goal**: Full compiler produces identical results, passes all existing tests.

#### Full Test Suite Execution

```bash
# Run entire existing test suite with Rust compiler
RESCRIPT_COMPILER=rust make test

# Or specifically:
RESCRIPT_COMPILER=rust make test-syntax
RESCRIPT_COMPILER=rust make test-analysis
RESCRIPT_COMPILER=rust make test-gentype
RESCRIPT_COMPILER=rust make test-tools
```

#### Stdlib Compilation Test

```bash
# Compile stdlib with both compilers, compare outputs
make clean
RESCRIPT_COMPILER=ocaml make lib
mv lib/es6 /tmp/ocaml_stdlib

make clean
RESCRIPT_COMPILER=rust make lib
mv lib/es6 /tmp/rust_stdlib

# Compare all generated files
diff -r /tmp/ocaml_stdlib /tmp/rust_stdlib
```

#### Real-World Project Tests

```bash
# Clone and test popular ReScript projects
for repo in rescript-react rescript-core rescript-webapi; do
    git clone --depth 1 https://github.com/rescript-association/$repo /tmp/$repo
    cd /tmp/$repo

    # Build with both compilers
    RESCRIPT_COMPILER=ocaml npx rescript build
    cp -r lib /tmp/${repo}_ocaml

    RESCRIPT_COMPILER=rust npx rescript build
    cp -r lib /tmp/${repo}_rust

    diff -r /tmp/${repo}_ocaml /tmp/${repo}_rust
done
```

---

### Continuous Integration Configuration

```yaml
# .github/workflows/rust-compiler.yml

name: Rust Compiler Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Build Rust Compiler
        run: cargo build --release --manifest-path compiler-rust/Cargo.toml

      - name: Run Rust Unit Tests
        run: cargo test --manifest-path compiler-rust/Cargo.toml

      - name: Parser Golden Tests
        run: ./scripts/test_rust_parser.sh

      - name: Codegen Golden Tests
        run: ./scripts/test_rust_codegen.sh

      - name: Type Error Golden Tests
        run: ./scripts/test_rust_type_errors.sh

      - name: Full Integration Tests
        run: RESCRIPT_COMPILER=rust make test

  concurrency:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Build with ThreadSanitizer
        run: |
          RUSTFLAGS="-Z sanitizer=thread" cargo +nightly test \
            --manifest-path compiler-rust/Cargo.toml

      - name: Parallel Stress Test
        run: |
          # Compile same file 100 times in parallel
          seq 100 | parallel -j20 \
            './target/release/bsc tests/tests/src/Belt_HashSetTest.res -o /tmp/out_{}.js'

          # All outputs must be identical
          md5sum /tmp/out_*.js | cut -d' ' -f1 | sort -u | wc -l | grep -q '^1$'

  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Parser Benchmark
        run: |
          cargo build --release --manifest-path compiler-rust/Cargo.toml
          hyperfine --warmup 3 \
            './cli/bsc.js -dparsetree tests/syntax_benchmarks/RedBlackTree.res' \
            './target/release/bsc -dparsetree tests/syntax_benchmarks/RedBlackTree.res'

      - name: Compilation Benchmark
        run: |
          hyperfine --warmup 3 \
            './cli/bsc.js tests/tests/src/Belt_HashSetTest.res' \
            './target/release/bsc tests/tests/src/Belt_HashSetTest.res'
```

---

### Test Result Tracking

Create test comparison dashboard:

```bash
#!/bin/bash
# scripts/test_comparison_report.sh

echo "# Rust vs OCaml Compiler Comparison"
echo ""
echo "| Test Category | OCaml | Rust | Match |"
echo "|---------------|-------|------|-------|"

# Syntax tests
SYNTAX_TOTAL=$(find tests/syntax_tests -name "*.res" | wc -l)
SYNTAX_PASS=$(./scripts/test_rust_parser.sh 2>&1 | tail -1 | grep -o '[0-9]*')
echo "| Syntax Parser | $SYNTAX_TOTAL | $((SYNTAX_TOTAL - SYNTAX_PASS)) | ${SYNTAX_PASS:-0} diffs |"

# Integration tests
INTEG_TOTAL=$(find tests/tests/src -name "*.res" | wc -l)
INTEG_PASS=$(./scripts/test_rust_codegen.sh 2>&1 | grep -c "differ" || echo 0)
echo "| Integration | $INTEG_TOTAL | $((INTEG_TOTAL - INTEG_PASS)) | ${INTEG_PASS:-0} diffs |"

# Type errors
ERROR_TOTAL=$(find tests/build_tests/super_errors/fixtures -name "*.res" | wc -l)
ERROR_PASS=$(./scripts/test_rust_type_errors.sh 2>&1 | grep -c "DIFF" || echo 0)
echo "| Type Errors | $ERROR_TOTAL | $((ERROR_TOTAL - ERROR_PASS)) | ${ERROR_PASS:-0} diffs |"
```

---

### Summary: Test Reuse by Phase

| Phase | Existing Tests Reused | New Tests Needed |
|-------|----------------------|------------------|
| **Phase 1** | None | Unit tests for Ident, Location, Path; Concurrency tests |
| **Phase 2** | 1,827 syntax tests, 511 golden files | Roundtrip comparison script |
| **Phase 3** | 658 integration tests, .mjs outputs | Lambda IR comparison, pass-specific tests |
| **Phase 4** | 247 error snapshots, typed tree tests | Property-based type tests, unification unit tests |
| **Phase 5** | All above + stdlib, real-world projects | Performance benchmarks, stress tests |

**Total reusable test files**: ~5,000+
**Total golden snapshots**: ~1,500+

---

## Crate Structure

```
compiler-rust/
├── Cargo.toml
├── PLAN.md              # This file
├── PROGRESS.md          # Progress tracking
├── GLOBAL_STATE_AUDIT.md # Catalog of OCaml globals
├── src/
│   ├── lib.rs
│   ├── ident.rs          # Phase 1
│   ├── location.rs       # Phase 1
│   ├── context.rs        # Phase 1
│   ├── config.rs         # Phase 1
│   ├── diagnostics.rs    # Phase 1
│   ├── cache.rs          # Phase 1
│   ├── parsetree.rs      # Phase 2
│   ├── parser/           # Phase 2
│   │   ├── mod.rs
│   │   ├── scanner.rs
│   │   ├── core.rs
│   │   └── printer.rs
│   ├── lambda.rs         # Phase 3
│   ├── js_ir.rs          # Phase 3
│   ├── codegen/          # Phase 3
│   │   ├── mod.rs
│   │   ├── lam_compile.rs
│   │   └── js_dump.rs
│   ├── types.rs          # Phase 4
│   ├── typechecker/      # Phase 4
│   │   ├── mod.rs
│   │   ├── ctype.rs
│   │   ├── typecore.rs
│   │   └── env.rs
│   └── ffi.rs            # FFI boundary (temporary)
└── tests/
    ├── parser_golden.rs
    ├── typechecker_tests.rs
    └── codegen_golden.rs
```

---

## Milestones

1. **M1: Foundation** - Utilities migrated, FFI working, CI established
2. **M2: Parser** - Full parser in Rust, identical AST output
3. **M3: Backend** - Lambda/JS generation in Rust, identical JS output
4. **M4: Type Checker** - Complete type checking in Rust
5. **M5: Integration** - Single Rust binary, OCaml removed

---

## References

- [GLOBAL_STATE_AUDIT.md](./GLOBAL_STATE_AUDIT.md) - Complete catalog of OCaml global state
- [PROGRESS.md](./PROGRESS.md) - Current progress and next steps
- `rewatch/src/` - Reference Rust patterns from the build system
