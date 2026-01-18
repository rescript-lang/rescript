# Global State Audit - ReScript Compiler

This document catalogs all global mutable state in the OCaml compiler that must be eliminated for concurrent compilation.

## Critical Global State (Must Thread Through Context)

### 1. Identifier Generation (`compiler/ext/ident.ml`)

| Global | Type | Purpose | Rust Alternative |
|--------|------|---------|------------------|
| `currentstamp` | `int ref` | Unique identifier stamps | `IdGenerator::next_ident: AtomicU32` |
| `reinit_level` | `int ref` | Reset level for identifiers | Field in `CompilationContext` |

**Impact**: HIGH - Every identifier in the compiler uses this. Must be per-context.

### 2. Type Checking Levels (`compiler/ml/ctype.ml`)

| Global | Type | Purpose | Rust Alternative |
|--------|------|---------|------------------|
| `current_level` | `int ref` | Current generalization level | `TypeContext::current_level: i32` |
| `nongen_level` | `int ref` | Non-generalizable type level | `TypeContext::nongen_level: i32` |
| `global_level` | `int ref` | Global type level | `TypeContext::global_level: i32` |
| `saved_level` | `(int * int) list ref` | Saved level stack | `TypeContext::saved_levels: Vec<(i32, i32)>` |

**Impact**: CRITICAL - Core type inference algorithm depends on these.

### 3. Type Unification State (`compiler/ml/ctype.ml`)

| Global | Type | Purpose | Rust Alternative |
|--------|------|---------|------------------|
| `trace_gadt_instances` | `bool ref` | GADT instance tracking | `TypeContext::trace_gadt_instances: bool` |
| `simple_abbrevs` | `abbrev_memo ref` | Simple type abbreviations | `TypeContext::simple_abbrevs` |
| `umode` | `unification_mode ref` | Unification mode | `TypeContext::umode` |
| `generate_equations` | `bool ref` | Equation generation flag | `TypeContext::generate_equations` |
| `assume_injective` | `bool ref` | Injectivity assumption | `TypeContext::assume_injective` |
| `free_variables` | `type_expr list ref` | Free type variables | Local variable or context field |
| `really_closed` | `type_expr list option ref` | Closure tracking | Local variable or context field |
| `abbreviations` | `abbrev_memo ref ref` | Nested ref for abbrevs | `TypeContext::abbreviations` |
| `reified_var_counter` | `Vars.t ref` | Reified variable counter | `TypeContext::reified_var_counter` |
| `delayed_copy` | `... list ref` | Delayed copies | Local variable |
| `previous_env` | `Env.t ref` | Previous environment | Parameter passing |
| `type_changed` | `bool ref` | Change tracking | Return value or context |
| `univar_pairs` | `... ref` | Universal variable pairs | Context field |
| `newtype_level` | `int option ref` | New type level | Context field |
| `package_subtype` | `function ref` | Package subtype function | Trait implementation |
| `rigid_variants` | `bool ref` | Rigid variant flag | Context field |
| `passive_variants` | `bool ref` | Passive variant flag | Context field |
| `warn` | `bool ref` | Coercion warning flag | Return value |
| `visited` | `TypeSet.t ref` | Visited type set | Local variable |
| `variant_is_subtype` | `function ref` | Subtype check function | Trait implementation |

**Impact**: CRITICAL - These control type unification algorithm behavior.

### 4. Type Environment (`compiler/ml/env.ml`)

| Global | Type | Purpose | Rust Alternative |
|--------|------|---------|------------------|
| `persistent_structures` | `Hashtbl.t` | Cached persistent structures | `ModuleCache` (thread-safe) |
| `current_unit` | `string ref` | Current compilation unit | `CompilationContext::current_unit` |
| `imported_units` | `StringSet.t ref` | Imported units | `CompilationContext::imported_units` |
| `can_load_cmis` | `can_load_cmis ref` | CMI loading capability | `CompilationContext` field |
| `same_constr` | `function ref` | Constructor equality | Trait implementation |
| `iter_env_cont` | `... ref` | Environment iteration cont | Local variable |
| `last_env` | `Env.t ref` | Cached last environment | Remove (optimization) |
| `last_reduced_env` | `Env.t ref` | Cached reduced env | Remove (optimization) |

Global hash tables (need thread-safe alternatives):
- `type_declarations` - Hashtbl for type decls
- `module_declarations` - Hashtbl for module decls
- `prefixed_sg` - Hashtbl for prefixed signatures

**Impact**: HIGH - Environment is passed through type checking.

### 5. Location Tracking (`compiler/ml/location.ml`)

| Global | Type | Purpose | Rust Alternative |
|--------|------|---------|------------------|
| `input_name` | `string ref` | Current input file | `SourceContext::input_name` |
| `warning_printer` | `function ref` | Warning printer | `DiagnosticsContext::warning_printer` |
| `formatter_for_warnings` | `Format.formatter ref` | Warning formatter | `DiagnosticsContext::formatter` |
| `error_reporter` | `function ref` | Error reporter | `DiagnosticsContext::error_reporter` |

**Impact**: MEDIUM - Easy to thread through, but pervasive usage.

### 6. Compiler Configuration (`compiler/common/js_config.ml`)

| Global | Type | Purpose | Rust Alternative |
|--------|------|---------|------------------|
| `no_version_header` | `bool ref` | Skip version header | `CompilerConfig::no_version_header` |
| `directives` | `string list ref` | JS directives | `CompilerConfig::directives` |
| `cross_module_inline` | `bool ref` | Cross-module inlining | `CompilerConfig::cross_module_inline` |
| `diagnose` | `bool ref` | Diagnostic mode | `CompilerConfig::diagnose` |
| `no_builtin_ppx` | `bool ref` | Skip builtin PPX | `CompilerConfig::no_builtin_ppx` |
| `check_div_by_zero` | `bool ref` | Division check | `CompilerConfig::check_div_by_zero` |
| `syntax_only` | `bool ref` | Syntax check only | `CompilerConfig::syntax_only` |
| `binary_ast` | `bool ref` | Binary AST output | `CompilerConfig::binary_ast` |
| `debug` | `bool ref` | Debug mode | `CompilerConfig::debug` |
| `cmi_only` | `bool ref` | CMI only mode | `CompilerConfig::cmi_only` |
| `cmj_only` | `bool ref` | CMJ only mode | `CompilerConfig::cmj_only` |
| `force_cmi` | `bool ref` | Force CMI | `CompilerConfig::force_cmi` |
| `force_cmj` | `bool ref` | Force CMJ | `CompilerConfig::force_cmj` |
| `jsx_version` | `int option ref` | JSX version | `CompilerConfig::jsx_version` |
| `jsx_module` | `jsx_module ref` | JSX module | `CompilerConfig::jsx_module` |
| `jsx_preserve` | `bool ref` | Preserve JSX | `CompilerConfig::jsx_preserve` |
| `js_stdout` | `bool ref` | Output to stdout | `CompilerConfig::js_stdout` |
| `all_module_aliases` | `bool ref` | Module aliases | `CompilerConfig::all_module_aliases` |
| `no_stdlib` | `bool ref` | No stdlib | `CompilerConfig::no_stdlib` |
| `no_export` | `bool ref` | No export | `CompilerConfig::no_export` |
| `as_pp` | `bool ref` | Preprocessor mode | `CompilerConfig::as_pp` |

**Impact**: LOW - Easy to make immutable config struct.

### 7. Warnings (`compiler/ext/warnings.ml`)

| Global | Type | Purpose | Rust Alternative |
|--------|------|---------|------------------|
| `disabled` | `bool ref` | Warnings disabled | `DiagnosticsContext::warnings_disabled` |
| `has_warnings` | `bool ref` | Has warnings flag | `DiagnosticsContext::has_warnings` |
| `nerrors` | `int ref` | Error count | `DiagnosticsContext::error_count` |

**Impact**: MEDIUM - Thread through diagnostics context.

### 8. Package State (`compiler/core/js_packages_state.ml`)

| Global | Type | Purpose | Rust Alternative |
|--------|------|---------|------------------|
| `packages_info` | `Js_packages_info.t ref` | Package information | `CompilerConfig::packages_info` |
| `make_runtime` | `bool ref` | Runtime generation flag | `CompilerConfig::make_runtime` |

**Impact**: LOW - Part of configuration.

### 9. CMJ Loading (`compiler/core/js_cmj_load.ml`)

| Global | Type | Purpose | Rust Alternative |
|--------|------|---------|------------------|
| `load_unit` | `function ref` | Unit loading function | `ModuleLoader` trait |

**Impact**: MEDIUM - Dependency injection pattern.

### 10. Debug Logging (`compiler/core/lam_util.ml`, `js_pass_debug.ml`)

| Global | Type | Purpose | Rust Alternative |
|--------|------|---------|------------------|
| `log_counter` | `int ref` | Debug log counter | `AtomicU32` or remove |

**Impact**: LOW - Debug only.

### 11. Other Globals

#### `compiler/ml/depend.ml`
- `pp_deps` - Preprocessor dependencies
- `free_structure_names` - Free structure names
- `pattern_bv` - Pattern bound variables

#### `compiler/ml/predef.ml`
- `builtin_idents` - Builtin identifiers list

#### `compiler/ml/delayed_checks.ml`
- `delayed_checks` - Delayed type checks

#### `compiler/ml/ast_mapper.ml`
- `cookies` - PPX cookies
- `tool_name_ref` - Tool name
- `register_function` - PPX registration

#### `compiler/ml/subst.ml`
- `new_id` - New identifier counter
- `ctype_apply_env_empty` - Function reference

#### `compiler/ml/oprint.ml`
- Multiple output function refs for pretty printing

#### `compiler/ext/config.ml`
- `load_path` - Module search path

#### `compiler/ext/runtime_package.ml`
- `path` - Runtime package path

---

## Summary

**Total global refs found**: ~80+
**Critical for concurrency**: ~40 (type checker state)
**Easy to fix**: ~30 (config, debug)
**Medium difficulty**: ~10 (environment caching)

## Migration Strategy

1. **Phase 1**: Create `CompilerConfig` (immutable) for all config flags
2. **Phase 2**: Create `CompilationContext` with `IdGenerator`, `SourceContext`, `DiagnosticsContext`
3. **Phase 3**: Create `TypeContext` for all type checking state
4. **Phase 4**: Create `ModuleCache` (thread-safe) for environment caching
5. **Phase 5**: Thread contexts through all functions

## Rust Context Hierarchy

```rust
// Immutable, shared across all compilations
pub struct CompilerConfig { /* all config flags */ }

// Per-compilation, owns source info and diagnostics
pub struct CompilationContext {
    config: Arc<CompilerConfig>,
    id_gen: IdGenerator,
    source: SourceContext,
    diagnostics: DiagnosticsContext,
}

// Per-type-checking, owns type arena
pub struct TypeContext<'ctx> {
    compilation: &'ctx CompilationContext,
    arena: TypeArena<'ctx>,
    current_level: i32,
    // ... all ctype.ml globals
}

// Thread-safe, shared across parallel compilations
pub struct ModuleCache {
    interfaces: DashMap<ModulePath, Arc<CompiledInterface>>,
}
```
