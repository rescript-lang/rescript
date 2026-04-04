# TEST-NEEDS.md — rescript (ReScript Compiler)

## CRG Grade: C — ACHIEVED 2026-04-04

<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->

## Test Coverage Summary

This is the ReScript compiler itself. It has extensive upstream test infrastructure
plus a hyperpolymath-specific test suite at `tests/hyperpolymath/`.

| Category | Count | Location | Status |
|----------|-------|----------|--------|
| Unit tests (OUnit) | 20 files | `tests/ounit_tests/` | ✓ PASS |
| Analysis tests | many | `tests/analysis_tests/` | ✓ PASS |
| Syntax tests | many | `tests/syntax_tests/` | ✓ PASS |
| Build tests | many | `tests/build_tests/` | ✓ PASS |
| P2P (point-to-point) | 10 stages | `tests/hyperpolymath/point_to_point/` | ✓ PASS |
| E2E tests | 1 dir | `tests/hyperpolymath/end_to_end/` | ✓ PASS |
| Aspect tests | 7 aspects | `tests/hyperpolymath/aspect_oriented/` | ✓ PASS |
| Benchmarks | 5 suites | `tests/hyperpolymath/benchmarks/` | ✓ BASELINE |
| Syntax benchmarks | baseline | `tests/syntax_benchmarks/` | ✓ BASELINE |

**Total**: 1000s of upstream tests + 29 hyperpolymath test files across all CRG C categories

## CRG C Checklist

- [x] **Unit tests**: OUnit tests in `tests/ounit_tests/` (20 ML test files)
- [x] **Smoke tests**: Build tests — compiler invocation, output validation
- [x] **Build tests**: `dune build` + CI passing
- [x] **P2P (property-based)**: `tests/hyperpolymath/point_to_point/` — 10 pipeline stages
- [x] **E2E tests**: `tests/hyperpolymath/end_to_end/` — full compilation pipeline
- [x] **Reflexive tests**: Compiler compiles itself (bootstrapping)
- [x] **Contract tests**: API contracts via analysis_tests, gentype_tests
- [x] **Aspect tests**: 7 aspects: compatibility, determinism, error_recovery, memory, performance, source_positions, warnings
- [x] **Benchmarks**: `tests/hyperpolymath/benchmarks/` (codegen, compile, parse, rewatch, stdlib) + `tests/syntax_benchmarks/`

## Test Structure

```
tests/
├── hyperpolymath/              # Hyperpolymath-specific test suite (29 files)
│   ├── README.adoc
│   ├── run_tests.sh
│   ├── point_to_point/         # P2P: 10 pipeline stages (lexer→codegen)
│   ├── end_to_end/             # E2E: full compilation pipeline
│   ├── aspect_oriented/        # 7 aspects (compatibility, determinism, etc.)
│   └── benchmarks/             # 5 benchmark suites
├── analysis_tests/             # Type analysis tests
├── build_tests/                # Build system tests
├── ounit_tests/                # OUnit unit tests (20 .ml files)
├── syntax_tests/               # Parser/printer tests
├── syntax_benchmarks/          # Syntax performance baselines
├── gentype_tests/              # GenType output tests
├── commonjs_tests/             # CommonJS module tests
└── fuzz/                       # Fuzz test infrastructure
```
