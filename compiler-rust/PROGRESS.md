# ReScript Compiler Rust Rewrite - Progress

> **Note**: This is a living document. Update it as you complete tasks, discover new work items, or gain insights. See [PLAN.md](./PLAN.md) for the full migration plan.

## Current Phase: Phase 2 - Parser

### Overall Progress

| Phase | Status | Progress | Key Blocker |
|-------|--------|----------|-------------|
| Phase 1: Foundation | ✅ Complete | 100% | None |
| Phase 2: Parser | 🚧 In Progress | ~65% | None |
| Phase 3: Lambda/JS IR | 🚧 In Progress | ~30% | None |
| Phase 4: Type Checker | ✅ Complete | 100% | Parallel with Phase 2 |
| Phase 5: Integration | ⏳ Not Started | 0% | Depends on Phase 4 |

---

## Phase 1 Progress

### Completed ✅

- [x] **Global State Audit** - Documented ~80+ global refs in [GLOBAL_STATE_AUDIT.md](./GLOBAL_STATE_AUDIT.md)
  - Identified all `Ident.currentstamp`, `Ctype.current_level`, etc.
  - Cataloged hash tables used as global caches
  - Documented threading strategy for each

- [x] **Crate Structure** - Created `compiler-rust/` with initial structure
  - `Cargo.toml` with dependencies (serde, rayon, dashmap, typed-arena, etc.)
  - Module organization established

- [x] **Ident Module** (`src/ident.rs`)
  - `Ident` type with `name`, `stamp`, `flags`
  - No global state - requires `IdGenerator` for creation
  - Full test suite including concurrent generation tests
  - `IdentTable` type alias using `IndexMap`

- [x] **Context Module** (`src/context.rs`)
  - `IdGenerator` with `AtomicI32` for thread-safe stamp generation
  - `SourceContext` for file information
  - `CompilationContext` tying everything together
  - Concurrent tests passing

- [x] **Config Module** (`src/config.rs`)
  - `CompilerConfig` struct mirroring `js_config.ml`
  - Immutable after construction (no refs)
  - Builder pattern for construction
  - JSX configuration (version, module)
  - All compilation flags

- [x] **Location Module** (`src/location.rs`)
  - `Position` type for source positions
  - `Location` type for source spans
  - `Located<T>` for values with locations
  - Range normalization for error reporting
  - Display formatting for diagnostics

- [x] **Diagnostics Module** (`src/diagnostics.rs`)
  - `Warning` enum with all warning types (matching OCaml)
  - `WarningConfig` for enabling/disabling warnings
  - `DiagnosticsContext` for collecting warnings/errors
  - No global state - context is passed explicitly
  - Warning specification parsing (+n, -n, @n syntax)

- [x] **Cache Module** (`src/cache.rs`)
  - `ModulePath` for module file identification
  - `CompiledInterface` for cached module data
  - `ModuleCache` with thread-safe `DashMap`
  - Dependency tracking and invalidation
  - Concurrent access tests passing

- [x] **FFI Boundary** (`src/ffi.rs`)
  - Serialization-based OCaml/Rust boundary with `bincode`
  - `FfiBuffer` for safe memory management
  - `FfiHandle` for opaque type passing
  - `ParseResult`, `CompileRequest`, `CompileResult` types
  - Error handling with `FfiResult` and `FfiStatus`
  - Full test suite passing

- [x] **CI Pipeline** (`.github/workflows/ci.yml`)
  - `test-compiler-rust` job added
  - Cargo fmt check
  - Cargo clippy with all targets
  - Cargo test (all tests)
  - Cargo doc tests
  - Runs in parallel with existing OCaml CI

### Not Started ⏳

- [ ] **Utility Modules** (as needed by parser)
  - `ext_list.ml` equivalents
  - `ext_string.ml` equivalents
  - `ext_array.ml` equivalents

- [ ] **ThreadSanitizer CI** (optional enhancement)
  - Nightly Rust with `-Z sanitizer=thread`
  - Stress testing for parallel compilation

---

## Next Steps

1. ~~Add FFI boundary infrastructure for OCaml interop~~ ✅
2. ~~Set up CI pipeline with Rust checks~~ ✅
3. ~~Begin Phase 2: Parser implementation~~ 🚧 In Progress
   - ~~`res_comment.ml` → Comment module~~ ✅
   - ~~`res_token.ml` → Token definitions~~ ✅
   - ~~`res_scanner.ml` → Lexer~~ ✅
   - ~~`res_utf8.ml`, `ext_utf8.ml` → UTF-8 utilities~~ ✅
   - ~~`res_diagnostics.ml` → Parser diagnostics~~ ✅
   - ~~`res_grammar.ml` → Grammar definitions~~ ✅
   - ~~`res_parser.ml` → Parser state~~ ✅
   - `res_core.ml` → Parser core (next, ~7365 lines)
4. Add utility modules as needed by parser

---

## Phase 2 Progress

### Completed ✅

- [x] **Comment Module** (`src/parser/comment.rs`)
  - `CommentStyle` enum (SingleLine, MultiLine, DocComment, ModuleComment)
  - `Comment` struct with location tracking
  - Factory methods for creating comments
  - 7 tests passing

- [x] **Token Module** (`src/parser/token.rs`)
  - Full `Token` enum with all ReScript tokens (~100 variants)
  - Operator precedence table
  - Keyword lookup and classification
  - Display implementation
  - 7 tests passing

- [x] **UTF-8 Module** (`src/parser/utf8.rs`)
  - `ByteClass` enum for classifying UTF-8 bytes
  - Codepoint encoding/decoding functions
  - UTF-16 length calculation for column tracking
  - 10 tests passing

- [x] **Parser Diagnostics** (`src/parser/diagnostics.rs`)
  - `DiagnosticCategory` enum for error types
  - `ParserDiagnostic` struct with location tracking
  - Helper functions for capitalization
  - 4 tests passing

- [x] **Scanner Module** (`src/parser/scanner.rs`)
  - Full lexical scanner (~1100 lines)
  - Character navigation (next, peek, peek2, peek3)
  - Position tracking (line, column, UTF-16 offset)
  - Identifier/keyword scanning with special cases (list{, dict{)
  - Number literal scanning (int, float, hex, octal, binary, bigint)
  - String literal scanning with escape sequences
  - Template literal scanning (interpolation support)
  - Comment scanning (single-line, multi-line, doc comments)
  - All operators and punctuation
  - Diamond mode for type parameter context
  - Error recovery with diagnostic collection
  - 12 tests passing

- [x] **Grammar Module** (`src/parser/grammar.rs`)
  - `Grammar` enum with ~50 parsing contexts
  - Token classification functions (is_expr_start, is_pattern_start, etc.)
  - List element and terminator predicates
  - Display implementation for error messages
  - 8 tests passing

- [x] **Parser State Module** (`src/parser/state.rs`)
  - `Parser` struct with token, position, and breadcrumb tracking
  - `ParserMode` and `RegionStatus` enums
  - Token advancement with comment collection
  - Error reporting with region-based deduplication
  - Breadcrumb management for error recovery
  - Lookahead with state save/restore
  - Template literal token support
  - 9 tests passing

- [x] **Parser Core Modules** (`src/parser/core.rs`, `expr.rs`, `pattern.rs`, `typ.rs`, `module.rs`)
  - Recursive descent parser with error recovery
  - Expression parsing (`expr.rs`) - 8 tests with timeout protection
  - Pattern parsing (`pattern.rs`) - 8 tests with timeout protection
  - Type parsing (`typ.rs`) - 6 tests with timeout protection
  - Module/structure parsing (`module.rs`) - 10 tests with timeout protection
  - Core parser utilities (`core.rs`) - AST helpers, ES6 arrow detection
  - All parser tests use 5-second timeout to catch hangs

- [x] **Printer Module** (`src/parser/printer.rs`)
  - AST-to-source printer for roundtrip testing
  - Handles all ReScript constructs (expressions, patterns, types, modules)
  - 48 roundtrip tests with timeout protection
  - Tests ensure parse→print→parse roundtrip correctness

### Not Started ⏳

- [ ] **Full Parser Integration**
  - Complete res_core.ml port (~7365 lines remaining)
  - JSX parsing
  - Advanced error recovery

### Parser Feature Gap Analysis (2026-01-18)

**Test Results Summary:**
- Total files tested: 1233
- Successful: 344 (28%)
- Failed: 889 (72%)
- Parse failures: 689
- Roundtrip failures: 200
- Timeout failures: 0

**Missing Parser Features (by frequency):**

| Feature | Count | Description |
|---------|-------|-------------|
| Module expressions | 83 | `include`, `open`, functor syntax |
| Labeled arguments (~) | 56 | `~foo`, `~foo=?`, `~foo as bar` |
| Division in expressions | 27 | `/` operator conflicts with regex/comments |
| Object field syntax | 27 | `{"field": value}`, `{..spread}` |
| Type constraints in exprs | 24 | `(expr : type)` inside collections |
| Identifier parsing | 18 | Edge cases in identifier recognition |
| Block expressions | 15 | `{...}` blocks in various contexts |
| Arrow functions | 13 | `=>` in edge cases |
| Type parameters | 10 | `<T>` angle bracket contexts |
| Labeled args in types | 10 | `(~foo: int) => unit` |
| Record equality | 9 | `{foo = bar}` vs `{foo: bar}` |
| Extension points | 8 | `%ext`, `%%ext`, `@attr` in complex positions |
| And keyword | 8 | `type t = ... and s = ...` |
| Underscore sugar | 7 | `_` placeholder in expressions |
| Module types | 7 | `module type S = sig ... end` |
| Pattern colon | 8 | `(pat : type)` in patterns |
| Backtick patterns | 4 | Polyvariant patterns |
| Single quote | 4 | Type variables `'a` |
| Type extensions | 3 | `type t += Constructor` |
| Tagged templates | 3 | `` tag`string` `` |

**Printer/Roundtrip Issues:**

| Issue | Description |
|-------|-------------|
| Attribute parentheses | `@attr (a + b)` loses parens → `@attr a + b` |
| Type chaining | `type t = ... and s = ...` loses `and` on 3rd type |
| Long tuples | Line-wrapped tuples printed on single line |
| Type constructors | `(a, b) constr` loses outer parens |
| Comment preservation | Some comments lost in roundtrip |

**Priority Implementation Order:**

1. **High Priority (blocks most tests):**
   - [ ] Module expressions (`include`, `open`, functors)
   - [ ] Labeled arguments (`~foo`, optional args)
   - [ ] Division operator disambiguation
   - [ ] Object/record field syntax

2. **Medium Priority:**
   - [ ] Type constraints in expressions
   - [ ] Block expressions
   - [ ] Extension points
   - [ ] And keyword for type chains

3. **Low Priority (edge cases):**
   - [ ] Underscore sugar
   - [ ] Tagged templates
   - [ ] Exotic identifiers

---

## Phase 4 Progress (Type Checker)

Phase 4 is being developed in parallel with Phase 2 (Parser).

### Completed ✅

- [x] **Path Module** (`src/types/path.rs`)
  - `Path` enum (Pident, Pdot, Papply) for module paths
  - Path comparison, equality, and hashing
  - `Typath` enum for constructor path categorization
  - Utility methods: `name()`, `head()`, `last()`, `flatten()`
  - 10 tests passing

- [x] **AST Types Module** (`src/types/asttypes.rs`)
  - Common AST types shared between parsetree and typedtree
  - `Constant`, `RecFlag`, `DirectionFlag`, `PrivateFlag`, `MutableFlag`
  - `ArgLabel` with labeled and optional argument support
  - `Located<T>` for values with source locations
  - 5 tests passing

- [x] **Variance Module** (`src/types/variance.rs`)
  - `Variance` bitfield type for type parameter variance
  - `VarianceFlag` enum (MayPos, MayNeg, MayWeak, Inj, Pos, Neg, Inv)
  - Variance operations: union, inter, conjugate, subset
  - Helper methods: `is_covariant()`, `is_contravariant()`, `is_invariant()`
  - 8 tests passing

- [x] **Type Expressions Module** (`src/types/type_expr.rs`)
  - `TypeExpr` struct with interior mutability for unification
  - `TypeDesc` enum with all type constructors:
    - `Tvar`, `Tarrow`, `Ttuple`, `Tconstr`, `Tobject`
    - `Tfield`, `Tnil`, `Tlink`, `Tsubst`, `Tvariant`
    - `Tunivar`, `Tpoly`, `Tpackage`
  - `RowDesc`, `RowField` for polymorphic variants
  - `AbbrevMemo` for type abbreviation caching
  - Reference types: `TypeExprRef`, `AbbrevMemoRef`, `RowFieldRef`
  - 5 tests passing

- [x] **Type Context Module** (`src/types/context.rs`)
  - `TypeContext<'a>` - Per-compilation type checking context
  - Arena-based allocation for type expressions
  - Level management: `begin_def()`, `end_def()`, `current_level()`
  - Type creation: `new_var()`, `new_arrow()`, `new_tuple()`, `new_constr()`
  - Union-find: `repr()` for following Tlink chains
  - GADT instance tracking
  - No global state - all state is explicit
  - 5 tests passing

- [x] **Declarations Module** (`src/types/decl.rs`)
  - `ValueDescription`, `TypeDeclaration` structs
  - `TypeKind` enum (Abstract, Record, Variant, Open)
  - `ConstructorDeclaration`, `LabelDeclaration`
  - `ConstructorDescription`, `LabelDescription` for environment
  - `ConstructorTag` for heap block tagging
  - `ModuleType`, `Signature`, `SignatureItem` for module types
  - `RecordRepresentation` for record layout
  - 4 tests passing

- [x] **Basic Type Operations** (`src/types/btype.rs`)
  - `repr()` with path compression (in context.rs)
  - Type predicates: `is_tvar`, `is_tunivar`, `is_tconstr`
  - Row operations: `row_field_repr`, `row_more`, `row_repr`, `row_field`, `row_fixed`, `static_row`
  - Type traversal: `iter_row`, `iter_type_expr`, `iter_type_expr_kind`
  - Type marking: `mark_type`, `mark_type_node`, `unmark_type`
  - Utilities: `hash_variant`, `is_row_name`, `proxy`, `row_of_type`
  - 6 tests passing

- [x] **Core Type Operations** (`src/types/ctype.rs`)
  - `unify()` function with full pattern matching on type descriptors
  - `UnifyState` for tracking unified pairs and mode
  - Occurs check: `occurs`, `deep_occur`
  - Link and level management: `link_type`, `update_level`
  - Row unification: `unify_rows`, `unify_row_field`, `unify_object_fields`
  - Generalization: `generalize`, `generalize_structure`
  - Instantiation: `instance`, `copy_type`, `copy_row`, `copy_row_field`
  - Type helpers: `is_function`, `function_arity`, `split_arrow`, `tuple_elements`
  - 8 tests passing

- [x] **Type Environment** (`src/types/env.rs`)
  - `Env` type with immutable/persistent structure
  - `BindingTable` for indexed lookups by name and stamp
  - Value, type, constructor, label, module lookups
  - `EnvSummary` for environment delta tracking
  - GADT constraint support
  - `initial_env` for built-in types
  - 6 tests passing

- [x] **Typed Tree** (`src/types/typedtree.rs`)
  - Typed AST structures: `Pattern`, `Expression`, `Case`, `ValueBinding`
  - Pattern descriptors: `Tpat_any`, `Tpat_var`, `Tpat_tuple`, etc.
  - Expression descriptors: `Texp_ident`, `Texp_constant`, `Texp_let`, etc.
  - Support types: `Partial`, `Constant`, `EnvRef`
  - Module expressions and coercion types
  - 5 tests passing

- [x] **Type Core** (`src/types/typecore.rs`)
  - Expression type checking: `type_expect`, `type_expression`
  - Pattern type checking: `type_pattern`
  - Let binding: `type_let_bindings`, `type_binding`
  - Error types: `TypeCoreError`, `TypeClashContext`
  - Type checking state: `TypeCheckContext`, `PatternState`
  - Environment extensions for built-in types
  - Constant conversion and type checking
  - Expression cases implemented:
    - Identifiers, constants, tuples, let bindings
    - If-then-else, sequence, while loop, for loop
    - Assert, array literal
    - Function application (`Pexp_apply`)
    - Match expressions (`Pexp_match`)
    - Try/catch expressions (`Pexp_try`)
    - Function expressions (`Pexp_fun`)
    - Constructor application (`Pexp_construct`)
    - Polymorphic variants (`Pexp_variant`)
    - Record expressions (`Pexp_record`)
    - Field access and mutation (`Pexp_field`, `Pexp_setfield`)
    - Type constraints (`Pexp_constraint`)
    - Type coercion (`Pexp_coerce`)
    - Open expressions (`Pexp_open`)
    - Newtype expressions (`Pexp_newtype`)
    - Await expressions (`Pexp_await`)
    - Method send (`Pexp_send`)
    - Let exception (`Pexp_letexception`)
    - Extensions (`Pexp_extension`)
    - Let module (`Pexp_letmodule`) - placeholder
    - Pack (`Pexp_pack`) - placeholder
    - JSX elements (`Pexp_jsx_element`) - placeholder
  - Core type translation: `transl_type` for translating parsed `CoreType` to `TypeExpr`
    - All type constructors: var, arrow, tuple, constr, alias, poly, object, variant, package
    - Type constraint integration in patterns and expressions
  - Pattern cases implemented:
    - Wildcard (`Ppat_any`)
    - Variable (`Ppat_var`)
    - Constant (`Ppat_constant`)
    - Tuple (`Ppat_tuple`)
    - Alias (`Ppat_alias`)
    - Or-pattern (`Ppat_or`)
    - Array (`Ppat_array`)
    - Constructor (`Ppat_construct`)
    - Polymorphic variant (`Ppat_variant`)
    - Record (`Ppat_record`)
    - Constraint (`Ppat_constraint`)
    - Interval (`Ppat_interval`)
    - Exception (`Ppat_exception`)
    - Open (`Ppat_open`)
  - 8 tests passing

- [x] **Module Type Checking Infrastructure** (`src/types/typemod.rs`) ✅
  - `type_module_expr` for all module expression variants
    - Module identifiers (`Pmod_ident`)
    - Module structures (`Pmod_structure`)
    - Functors (`Pmod_functor`)
    - Functor application (`Pmod_apply`)
    - Module constraints (`Pmod_constraint`)
    - Module unpack (`Pmod_unpack`)
    - Module extensions (`Pmod_extension`)
  - `type_structure` for module body type checking
  - `type_structure_item` for all structure item variants
    - Evaluated expressions (`Pstr_eval`)
    - Value bindings (`Pstr_value`)
    - Primitive declarations (`Pstr_primitive`)
    - Type declarations (`Pstr_type`)
    - Type extensions (`Pstr_typext`)
    - Exception declarations (`Pstr_exception`)
    - Module bindings (`Pstr_module`)
    - Recursive modules (`Pstr_recmodule`)
    - Module type declarations (`Pstr_modtype`)
    - Open declarations (`Pstr_open`)
    - Include declarations (`Pstr_include`)
    - Attributes (`Pstr_attribute`)
    - Extensions (`Pstr_extension`)
  - Extended `typedtree.rs` with module types:
    - `ModuleType`, `ModuleBinding`, `ModuleTypeDeclaration`
    - `OpenDeclaration`, `IncludeDeclaration`, `TypeExtension`
    - All `StructureItemDesc` variants
  - Error types: `TypeModError`, `TypeModResult`

- [x] **Signature Matching** (`src/types/includemod.rs`) ✅
  - `modtypes` for module type subtyping
  - `signatures` for signature inclusion checking
  - `value_descriptions` for value type compatibility
  - `type_declarations` for type declaration compatibility
  - `extension_constructors` for extension constructor compatibility
  - `modtype_declarations` for module type declaration compatibility
  - Error types: `IncludeModError`, `IncludeModResult`
  - Utility functions: `is_empty_signature`, `signature_values`, `signature_types`, `signature_modules`
  - 5 tests passing

### Phase 4 Complete ✅

All Phase 4 components are now implemented:
- [x] Core Type Translation
- [x] Module Type Checking Infrastructure
- [x] Signature Matching

---

## Phase 3 Progress (Lambda/JS IR)

Phase 3 is being developed for Lambda IR transformations and JavaScript code generation.

### Completed ✅

- [x] **Lambda IR Core Types** (`src/lambda/mod.rs`)
  - Core `Lambda` enum with all constructors (Lvar, Lconst, Lapply, Lfunction, etc.)
  - `LFunction`, `Apply`, `LambdaSwitch`, `PrimInfo` structs
  - `ApplyStatus`, `InlineAttribute`, `FunctionAttribute`, `DirectionFlag` enums
  - Builder methods for Lambda construction
  - 6 tests passing

- [x] **Lambda Constants** (`src/lambda/constant.rs`)
  - `Constant` enum with all literal types
  - Integer, float, string, bigint, char, boolean, null, undefined
  - Block constants for tuples, records, variants
  - Pointer constants for module references
  - Utility methods: `is_truthy`, `type_name`, `int()`, `float()`, `string()`, etc.
  - 8 tests passing

- [x] **Lambda Primitives** (`src/lambda/primitive.rs`)
  - `Primitive` enum with ~100+ operations
  - Arithmetic, comparison, string, array, object operations
  - JS interop primitives (Pjs_call, Pjs_apply, etc.)
  - Field access and mutation primitives
  - 6 tests passing

- [x] **Compatibility Types** (`src/lambda/compat.rs`)
  - `LetKind`, `Comparison`, `FieldDbgInfo`, `SetFieldDbgInfo`
  - Type aliases for consistency with OCaml code
  - 4 tests passing

- [x] **Tag Info** (`src/lambda/tag_info.rs`)
  - `TagInfo` enum for block tagging (variants, records, tuples, extensions)
  - `RecordRepresentation` for record layout
  - `RecordInfo` for inlined record tracking
  - 5 tests passing

- [x] **Arity Tracking** (`src/lambda/arity.rs`)
  - `Arity` enum for function arity information
  - `merge`, `merge_arities`, `get_first_arity` functions
  - Used for currying/uncurrying optimization
  - 8 tests passing

- [x] **Free Variable Analysis** (`src/lambda/free_variables.rs`)
  - `free_variables()` to compute free variables in Lambda expressions
  - `has_free_variables()` for efficient checking
  - `is_closed()` to check if expression is closed
  - Properly handles all binding constructs (let, function, for, catch, etc.)
  - 11 tests passing

- [x] **Side Effect Analysis** (`src/lambda/analysis.rs`)
  - `no_side_effects()` to check if Lambda is pure
  - `size()` to estimate code size for inlining decisions
  - `is_pure_primitive()` to check primitive purity
  - `lfunction_can_be_inlined()`, `ok_to_inline_fun_when_app()`, `safe_to_inline()`
  - 10 tests passing

- [x] **Closure Analysis** (`src/lambda/closure.rs`)
  - `VarStats` struct with `top` and `times` fields
  - `Position` enum (Begin, NotBegin, Sink)
  - `free_variables_with_stats()` for enriched free variable analysis
  - `is_closed()`, `is_closed_with_map()` for closure checks
  - 11 tests passing

- [x] **Variable Substitution** (`src/lambda/subst.rs`)
  - `subst()` to apply substitution mapping to Lambda
  - `subst_single()`, `make_subst()`, `singleton()`, `extend()` helpers
  - Properly handles all Lambda constructors
  - 9 tests passing

- [x] **Beta Reduction** (`src/lambda/beta_reduce.rs`)
  - `simple_beta_reduce()` for direct substitution when possible
  - `no_names_beta_reduce()` to create let bindings for all args
  - `propagate_beta_reduce_with_map()` for smart inlining with stats
  - `refine_let()` for optimized let binding creation
  - 8 tests passing

- [x] **Pretty Printing** (`src/lambda/print.rs`)
  - `LambdaPrinter` struct for formatting
  - `format_constant()`, `format_primitive()`, `format_lambda()` formatters
  - `lambda_to_string()`, `primitive_to_string()` string converters
  - S-expression style output for debugging
  - 8 tests passing

- [x] **Compilation Context** (`src/lambda/compile_context.rs`)
  - `Continuation` enum for what to do with expression results
  - `TailType`, `MaybeTail`, `Tail` for tail position tracking
  - `JmpTable` for exception handler mapping
  - `LamStats` for compilation metadata (exports, identifier kinds)
  - `IdKind` enum for identifier kind tracking
  - `CompileContext` struct tying everything together
  - 8 tests passing

### In Progress 🚧

- [ ] **Typed Tree to Lambda Conversion** (`src/lambda/convert.rs`)
  - Translation from typed AST to Lambda IR
  - Primitive conversion
  - Exception handling transformation
  - Pattern compilation

### Not Started ⏳

- [ ] **Lambda to JS IR Compilation** (`src/lambda/compile.rs`)
  - Main compilation pass from Lambda to JavaScript IR
  - Statement and expression generation
  - Continuation-based code generation

- [ ] **Exit Code Analysis** (`src/lambda/exit_code.rs`)
  - Static exception flow analysis

- [ ] **Strongly Connected Components** (`src/lambda/scc.rs`)
  - Recursive binding optimization

- [ ] **Lambda Optimization Passes**
  - Dead code elimination
  - Common subexpression elimination
  - Constant folding

---

## Files Created

| File | Status | Description |
|------|--------|-------------|
| `Cargo.toml` | ✅ Complete | Package configuration |
| `Cargo.lock` | ✅ Complete | Dependency lock file |
| `src/lib.rs` | ✅ Complete | Crate root with re-exports |
| `src/ident.rs` | ✅ Complete | Identifier type (replaces `ext/ident.ml`) |
| `src/context.rs` | ✅ Complete | Compilation contexts |
| `src/config.rs` | ✅ Complete | Compiler configuration |
| `src/location.rs` | ✅ Complete | Source locations |
| `src/diagnostics.rs` | ✅ Complete | Warnings and errors |
| `src/cache.rs` | ✅ Complete | Module cache |
| `src/ffi.rs` | ✅ Complete | FFI boundary for OCaml interop |
| `src/parser/mod.rs` | ✅ Complete | Parser module root |
| `src/parser/comment.rs` | ✅ Complete | Comment representation |
| `src/parser/token.rs` | ✅ Complete | Token definitions |
| `src/parser/utf8.rs` | ✅ Complete | UTF-8 encoding/decoding utilities |
| `src/parser/diagnostics.rs` | ✅ Complete | Parser error diagnostics |
| `src/parser/scanner.rs` | ✅ Complete | Lexical scanner |
| `src/parser/grammar.rs` | ✅ Complete | Grammar rules and token classification |
| `src/parser/state.rs` | ✅ Complete | Parser state management |
| `src/types/mod.rs` | ✅ Complete | Types module root |
| `src/types/path.rs` | ✅ Complete | Module paths (replaces `ml/path.ml`) |
| `src/types/asttypes.rs` | ✅ Complete | Common AST types |
| `src/types/variance.rs` | ✅ Complete | Variance analysis |
| `src/types/type_expr.rs` | ✅ Complete | Core type expressions |
| `src/types/context.rs` | ✅ Complete | Type checking context |
| `src/types/decl.rs` | ✅ Complete | Type declarations |
| `src/types/btype.rs` | ✅ Complete | Basic type operations |
| `src/types/ctype.rs` | ✅ Complete | Core type operations (unify, generalize, instance) |
| `src/types/env.rs` | ✅ Complete | Type environment |
| `src/types/typedtree.rs` | ✅ Complete | Typed AST (patterns, expressions) |
| `src/types/typecore.rs` | ✅ Complete | Type checking logic |
| `src/types/typemod.rs` | ✅ Complete | Module type checking |
| `src/types/includemod.rs` | ✅ Complete | Signature matching |
| `src/lambda/mod.rs` | ✅ Complete | Lambda IR core types |
| `src/lambda/constant.rs` | ✅ Complete | Lambda constants |
| `src/lambda/primitive.rs` | ✅ Complete | Lambda primitives |
| `src/lambda/compat.rs` | ✅ Complete | Compatibility types |
| `src/lambda/tag_info.rs` | ✅ Complete | Block tag information |
| `src/lambda/arity.rs` | ✅ Complete | Arity tracking |
| `src/lambda/free_variables.rs` | ✅ Complete | Free variable analysis |
| `src/lambda/analysis.rs` | ✅ Complete | Side effect and size analysis |
| `src/lambda/closure.rs` | ✅ Complete | Closure analysis |
| `src/lambda/subst.rs` | ✅ Complete | Variable substitution |
| `src/lambda/beta_reduce.rs` | ✅ Complete | Beta reduction |
| `src/lambda/print.rs` | ✅ Complete | Pretty printing for debugging |
| `src/lambda/compile_context.rs` | ✅ Complete | Compilation context |
| `PLAN.md` | ✅ Complete | Comprehensive migration plan |
| `PROGRESS.md` | ✅ Complete | This file |
| `GLOBAL_STATE_AUDIT.md` | ✅ Complete | OCaml global state catalog |

---

## Test Results

```
# Last test run: 2026-01-18
cargo test

running 342 tests
# Total tests passing: 342
# All tests pass!

# Phase 4 tests (83 tests)
test types::asttypes::tests::* ... ok (5 tests)
test types::btype::tests::* ... ok (6 tests)
test types::context::tests::* ... ok (6 tests)
test types::ctype::tests::* ... ok (8 tests)
test types::decl::tests::* ... ok (4 tests)
test types::env::tests::* ... ok (6 tests)
test types::path::tests::* ... ok (10 tests)
test types::type_expr::tests::* ... ok (5 tests)
test types::typedtree::tests::* ... ok (5 tests)
test types::typecore::tests::* ... ok (8 tests)
test types::typemod::tests::* ... ok (1 test)
test types::includemod::tests::* ... ok (5 tests)
test types::variance::tests::* ... ok (8 tests)
test types::tests::* ... ok (2 tests)

test result: ok. 342 passed; 0 failed; 0 ignored

# Roundtrip test results (syntax_tests/)
# Tested against: tests/syntax_tests/data/**/*.res (1233 files)
# Pass rate: 28% (344/1233)
# No timeouts - all tests complete within 5 second limit
```

---

## Blockers & Issues

None currently.

---

## Notes

### Decisions Made

1. **Edition 2024** - Using latest Rust edition for best ergonomics
2. **SmolStr for names** - Efficient small string storage for identifiers
3. **IndexMap for tables** - Maintains insertion order, good for debugging
4. **AtomicI32 for stamps** - Thread-safe ID generation with minimal overhead
5. **Renamed `gen` variables** - `gen` is reserved in Rust 2024, use `id_gen` instead
6. **DashMap for caches** - Thread-safe concurrent access for module cache
7. **Arena-based type allocation** - `TypeContext` uses `typed-arena` for type expressions
8. **Interior mutability for unification** - `TypeExpr` uses `RefCell` for `desc` mutation
9. **Reference indices for types** - `TypeExprRef` is an index into the arena (simpler than `&'a TypeExpr`)

### Open Questions

1. Should we use `ocaml-rs` for direct FFI or stick with serialization?
   - Decision: Start with serialization (simpler), optimize later if needed

2. How to handle PPX compatibility during transition?
   - Decision: Maintain `parsetree0.ml`, implement bidirectional conversion

---

## Timeline

| Milestone | Target | Status |
|-----------|--------|--------|
| M1: Foundation | TBD | ✅ Complete |
| M2: Parser | TBD | ⏳ Not Started |
| M3: Backend | TBD | ⏳ Not Started |
| M4: Type Checker | TBD | ⏳ Not Started |
| M5: Integration | TBD | ⏳ Not Started |
